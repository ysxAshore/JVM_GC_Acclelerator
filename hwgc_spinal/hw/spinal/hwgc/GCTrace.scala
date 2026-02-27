package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCTrace extends Module with GCParameters with HWParameters{
  val io = new Bundle {
    val ToTrace = slave(new GCToTrace)
    val Trace2Aop = master(new ToAopParameters)
    val Mreq = master(new LocalMMUIO)
    val Trace2Stack = master Stream UInt(GCElementWidth bits)
    val ConfigIO = slave(new GCTraceConfigIO)
    val DebugTimeStampe = in UInt(64 bits)
  }

  // default value
  io.ToTrace.Ready := False
  io.ToTrace.Done := False

  io.Trace2Aop.Valid := False
  io.Trace2Aop.RegionAttr := U(0)
  io.Trace2Aop.Task := U(0)

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  io.Trace2Stack.valid := False
  io.Trace2Stack.payload.clearAll()

  // State Machine

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(18)(i => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val Kid = RegInit(U(0, 32 bits))
  val OopType = RegInit(U(0, GCOopTypeWidth bits))
  val KlassPtr = RegInit(U(0, GCElementWidth bits))
  val SrcOopPtr = RegInit(U(0, GCElementWidth bits))
  val DestOopPtr = RegInit(U(0, GCElementWidth bits))
  val ScanningInYoung = RegInit(False)
  val StepIndex = RegInit(U(0, 32 bits))
  val StepNCreate = RegInit(U(0, 32 bits))
  val ArrayLength = RegInit(U(0, 32 bits))
  val PartialArrayStart = RegInit(U(0, 32 bits))

  val p = RegInit(U(0, GCElementWidth bits))
  val q = RegInit(U(0, GCElementWidth bits))
  val vtable_len = RegInit(U(0, 32 bits))
  val start_map = RegInit(U(0, GCElementWidth bits))
  val end_map = RegInit(U(0, GCElementWidth bits))
  val src = RegInit(U(0, GCElementWidth bits))
  val dest = RegInit(U(0, GCElementWidth bits))
  val heap_oop = RegInit(U(0, GCElementWidth bits))
  val regionAttr = RegInit(U(0, 16 bits))
  val region = RegInit(U(0, 32 bits))
  val bool_base_value = RegInit(U(0, 8 bits))

  val previousState = RegInit(overall_state.states(0))
  val for_counter = RegInit(U(0, 32 bits))

  switch(state){
    is(overall_state.states(0)){
      io.ToTrace.Ready := True
      when(io.ToTrace.Valid && io.ToTrace.Ready){
        io.ToTrace.Done := False

        Kid := io.ToTrace.Kid
        OopType := io.ToTrace.OopType
        KlassPtr := io.ToTrace.KlassPtr
        SrcOopPtr := io.ToTrace.SrcOopPtr
        DestOopPtr := io.ToTrace.DestOopPtr
        ScanningInYoung := io.ToTrace.ScanningInYoung
        StepIndex := io.ToTrace.StepIndex
        StepNCreate := io.ToTrace.StepNCreate
        ArrayLength := io.ToTrace.ArrayLength
        PartialArrayStart := Mux(io.ToTrace.OopType === U(PartialArrayOop), io.ToTrace.PartialArrayStart, U(0))

        // typeArrayKid not enter to trace module
        for_counter := U(0)
        state := overall_state.states(1)

        if(DebugEnable){
          report(Seq(
            "[GCTrace<", io.DebugTimeStampe,
            ">] Receive GCTrace Task",
            ", RegionAttrBase = ", io.ConfigIO.RegionAttrBase,
            ", RegionAttrShiftBy = ", io.ConfigIO.RegionAttrShiftBy,
            ", RegionAttrBiasedBase = ", io.ConfigIO.RegionAttrBiasedBase,
            ", HeapRegionBias = ", io.ConfigIO.HeapRegionBias,
            ", HeapRegionShiftBy = ", io.ConfigIO.HeapRegionShiftBy,
            ", LogOfHRGrainBytes = ", io.ConfigIO.LogOfHRGrainBytes,
            ", HumongousReclaimCandidatesBoolBase = ", io.ConfigIO.HumongousReclaimCandidatesBoolBase,
            ", OopType = ", io.ToTrace.OopType,
            ", KlassPtr = ", io.ToTrace.KlassPtr,
            ", SrcOopPtr = ", io.ToTrace.SrcOopPtr,
            ", DestOopPtr = ", io.ToTrace.DestOopPtr,
            ", Kid = ", io.ToTrace.Kid, "\n",
            ", ArrayLength = ", io.ToTrace.ArrayLength,
            ", PartialArrayStart = ", io.ToTrace.PartialArrayStart,
            ", StepIndex = ", io.ToTrace.StepIndex,
            ", StepNCreate = ", io.ToTrace.StepNCreate,
            "\n"
          ))
        }
      }
    }

    is(overall_state.states(1)) {
      when(OopType === U(PartialArrayOop) || Kid === U(ObjectArrayKlassID)){
        val addr = (DestOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16))).resize(MMUAddrWidth bits)
        val writeValue = Mux(OopType === U(PartialArrayOop), ArrayLength, ArrayLength % io.ConfigIO.ChunkSize).resize(32 bits)
        issueReq(io.Mreq, addr, True, getWstrb(4), writeValue, issued) { rd =>
          state := overall_state.states(2)
        }
      }.otherwise{
        val addr = (KlassPtr + U(160)).resize(MMUAddrWidth bits)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          vtable_len := rd(31 downto 0)
          state := overall_state.states(5)
        }
      }
    }

    is(overall_state.states(2)){
      val cond = Mux(OopType === U(PartialArrayOop), for_counter < StepNCreate, ArrayLength > (ArrayLength % io.ConfigIO.ChunkSize).resize(32 bits))
      when(cond){
        io.Trace2Stack.valid := True
        io.Trace2Stack.payload := (SrcOopPtr + U(2)).resize(GCElementWidth bits)
        state := overall_state.states(3)
      }.otherwise{
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(3)){
      when(io.Trace2Stack.fire && OopType === U(PartialArrayOop)){
        for_counter := for_counter + U(1)
        state := overall_state.states(2)
      }.elsewhen(io.Trace2Stack.fire){
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      val low = (DestOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(16), U(24)) + PartialArrayStart * Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))).resize(GCElementWidth bits)
      val high = (DestOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(16), U(24)) + StepIndex * Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))).resize(GCElementWidth bits)
      val temp_p = DestOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(16), U(24))
      val temp_q = (temp_p + ArrayLength * Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))).resize(GCElementWidth)
      p := Mux(temp_p < low, low, temp_p)
      q := Mux(temp_q > high, high, temp_q)

      // TRACE_PLUS -> done to end
      state := overall_state.states(8)
    }

    is(overall_state.states(5)){
      val addr = (KlassPtr + 296).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        start_map := (KlassPtr + U(464) + (vtable_len + rd(63 downto 32)) * U(8)).resize(GCElementWidth bits)
        end_map := (KlassPtr + U(464) + (vtable_len + rd(63 downto 32) + rd(31 downto 0)) * U(8)).resize(GCElementWidth bits)
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(6)){
      when(start_map < end_map){
        val addr = (end_map - U(8)).resize(MMUAddrWidth bits)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          p := DestOopPtr + rd(31 downto 0)
          q := (DestOopPtr + rd(31 downto 0) + rd(63 downto 32) * Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))).resize(GCElementWidth bits)
          end_map := end_map - U(8)

          // TRACE_DEC -> done to the state
          state := overall_state.states(9)
        }
      }.otherwise{
        for_counter := U(0)
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      when(Kid === U(InstanceMirrorKlassID)){
        val addr = SrcOopPtr + U(40)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          p := DestOopPtr + U(184)
          q := (DestOopPtr + U(184) + rd(31 downto 0) * Mux(io.ConfigIO.UseCompressedOops, 4, 8)).resize(GCElementWidth)

          // TRACE_PLUS -> done to end
          state := overall_state.states(8)
        }
      }.elsewhen(Kid === U(InstanceRefKlassID)){
        when(for_counter === U(3)){
          state := overall_state.states(0)
          io.ToTrace.Done := True
        }.otherwise {
          val discovered_offset = Mux(io.ConfigIO.UseCompressedOops && io.ConfigIO.UseCompressedKlassPointers, U"x18", Mux(io.ConfigIO.UseCompressedOops, U"x1c", U"x28"))
          val referent_offset = Mux(io.ConfigIO.UseCompressedOops && io.ConfigIO.UseCompressedKlassPointers, U"xc", U"x10")

          src := Mux(for_counter === U(1), SrcOopPtr + referent_offset, SrcOopPtr + discovered_offset)
          dest := Mux(for_counter === U(1), DestOopPtr + referent_offset, DestOopPtr + discovered_offset)

          for_counter := for_counter + U(1)
          previousState := overall_state.states(7)
          state := overall_state.states(10)
        }
      }.otherwise{
        state := overall_state.states(0)
        io.ToTrace.Done := True
      }
    }

    // TRACE_PLUS
    is(overall_state.states(8)){
      when(p < q){
        src := p - DestOopPtr + SrcOopPtr
        dest := p
        p := p + Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))
        previousState := overall_state.states(8)
        state := overall_state.states(10)
      }.otherwise{
        state := overall_state.states(0)
        io.ToTrace.Done := True
      }
    }

    is(overall_state.states(9)){
      when(p < q){
        val current_q = q - Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))
        src := current_q - DestOopPtr + SrcOopPtr
        dest := current_q

        previousState := overall_state.states(9)
        state := overall_state.states(10)
      }.otherwise{
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(10)){
      issueReq(io.Mreq, src, False, U(0), U(0), issued) { rd =>
        heap_oop := Mux(io.ConfigIO.UseCompressedOops, Cat(U(0, 32 bits), rd(31 downto 0)).asUInt, rd(GCElementWidth - 1 downto 0))
        state := overall_state.states(11)
      }
    }

    is(overall_state.states(11)){
      when(heap_oop === U(0)){
        state := previousState
      }.otherwise{
        val compressed_oop = (io.ConfigIO.CompressedOopBase + (heap_oop << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth bits)
        val current_oop = Mux(io.ConfigIO.UseCompressedOops, compressed_oop, heap_oop)
        val addr = (io.ConfigIO.RegionAttrBiasedBase + (current_oop >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth bits)

        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          regionAttr := rd(15 downto 0)
          heap_oop := current_oop

          state := overall_state.states(12)
        }
      }
    }

    is(overall_state.states(12)){
      val regionAttrType = regionAttr(15 downto 8).asSInt
      val cond = ((dest ^ heap_oop) >> io.ConfigIO.LogOfHRGrainBytes(5 downto 0)) =/= U(0)
      when(regionAttrType >= 0){
        io.Trace2Stack.valid := True
        io.Trace2Stack.payload := dest + Mux(io.ConfigIO.UseCompressedOops, U(1), U(0))
        when(io.Trace2Stack.fire){
          state := previousState
        }
      }.elsewhen(cond){
        when(regionAttrType === S(-2)){
          state := overall_state.states(13)
        }.otherwise{
          state := overall_state.states(16)
        }
      }.otherwise{
        state := previousState
      }
    }

    is(overall_state.states(13)){
      val current_region = ((heap_oop - (io.ConfigIO.HeapRegionBias << io.ConfigIO.HeapRegionShiftBy(4 downto 0))) >> io.ConfigIO.LogOfHRGrainBytes).resize(32 bits)
      val addr = (io.ConfigIO.HumongousReclaimCandidatesBoolBase + current_region).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        bool_base_value := rd(7 downto 0)
        region := current_region
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      when(bool_base_value === U(0)){
        state := overall_state.states(16)
      }.otherwise{
        val addr = (io.ConfigIO.HumongousReclaimCandidatesBoolBase + region).resize(MMUAddrWidth bits)
        issueReq(io.Mreq, addr, True, getWstrb(1), U(0), issued) { rd =>
          state := overall_state.states(15)
        }
      }
    }

    is(overall_state.states(15)){
      val addr = (io.ConfigIO.RegionAttrBase + region * U(2) + U(1)).resize(MMUAddrWidth bits)
      val writeValue = S(-1)
      issueReq(io.Mreq, addr, True, getWstrb(1), writeValue.asUInt, issued){ rd =>
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      when(ScanningInYoung){
        state := previousState
      }.otherwise{
        io.Trace2Aop.Valid := True
        io.Trace2Aop.Task := dest
        io.Trace2Aop.RegionAttr := regionAttr

        when(io.Trace2Aop.Valid && io.Trace2Aop.Ready){
          state := overall_state.states(17)
        }
      }
    }

    is(overall_state.states(17)){
      when(io.Trace2Aop.Done){
        state := previousState
      }
    }
  }
}

object GCTraceVerilog extends App {
  Config.spinal.generateVerilog(new GCTrace())
}