package hwgc

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

class GCOopCopy2Survivor extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToCopy = master(new GCToCopy)
    val ToTrace = master(new GCToTrace)
    val ToAllocate = master(new GCToAllocate)
    val ToCopySurvivor = slave(new GCToSurvivor)
    val ConfigIO = slave(new GCCopy2SurvivorConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ToCopy.clearIn()
  io.ToTrace.clearIn()
  io.ToAllocate.clearIn()
  io.ToCopySurvivor.clearOut()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(27)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))

  val markWord = RegInit(U(0, GCElementWidth bits))
  val klassPtr = RegInit(U(0, GCElementWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val regionAttrPtr = RegInit(U(0, GCElementWidth bits))

  val destOopPtr = RegInit(U(0, GCElementWidth bits))

  val lh = RegInit(U(0, 32 bits))
  val kid = RegInit(U(0, 32 bits))
  val array_length = RegInit(U(0, 32 bits))
  val size = RegInit(U(0, 32 bits))
  val src_region_attr = RegInit(U(0, 16 bits))
  val age = RegInit(U(0, 32 bits))
  val new_mark = RegInit(U(0, GCElementWidth bits))
  val dest_attr_ptr = RegInit(U(0, GCElementWidth bits))
  val dest_region_attr = RegInit(U(0, 16 bits))
  val buffer_ptr = RegInit(U(0, GCElementWidth bits))
  val buffer = RegInit(U(0, GCElementWidth bits))
  val region_top = RegInit(U(0, GCElementWidth bits))
  val region_end = RegInit(U(0, GCElementWidth bits))
  val from_region = RegInit(U(0, GCElementWidth bits))
  val young_index = RegInit(U(0, 32 bits))
  val temp_value = RegInit(U(0, GCElementWidth bits))

  val issued = RegInit(False)
  val issuedCopy = RegInit(False)
  val issuedTrace = RegInit(False)
  val copyDone = RegInit(False)
  val copyFirstBeatDone = RegInit(False)
  val traceDone = RegInit(False)
  val allocateSelect = RegInit(False)
  val allocate_done = RegInit(False)

  when(io.ToCopy.Done){
    copyDone := True
  }
  when(io.ToTrace.Done){
    traceDone := True
  }
  when(io.ToCopy.firstBeatDone){
    copyFirstBeatDone := True
  }
  when(io.ToAllocate.Done){
    allocate_done := True
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCAop<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def sendToAllocate(select: Bool): Unit = {
    allocateSelect := select
    io.ToAllocate.Valid := True
    io.ToAllocate.Size := size
    io.ToAllocate.DestAttrType := dest_region_attr(15 downto 8)

    when(io.ToAllocate.Valid && io.ToAllocate.Ready){
      state := overall_state.states(20)
    }
  }

  switch(state){
    is(overall_state.states(0)){
      io.ToCopySurvivor.Ready := True
      when(io.ToCopySurvivor.Valid && io.ToCopySurvivor.Ready){
        srcOopPtr := io.ToCopySurvivor.SrcOopPtr
        markWord := io.ToCopySurvivor.MarkWord
        klassPtr := io.ToCopySurvivor.KlassPtr
        regionAttrPtr := io.ToCopySurvivor.RegionAttrPtr

        state := overall_state.states(1)
        dbg(Seq("Receive task from OopProcess"))
      }
    }

    is(overall_state.states(1)){
      val compressedKlassPtr = (io.ConfigIO.CompressedKlassPointerBase + klassPtr(31 downto 0) << io.ConfigIO.CompressedKlassPointerShift).resize(GCElementWidth)
      val new_klassPtr = Mux(io.ConfigIO.UseCompressedKlassPointer, compressedKlassPtr, klassPtr)
      val addr = new_klassPtr + U(8)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        klassPtr := new_klassPtr
        lh := rd(31 downto 0)
        kid := rd(63 downto 32)
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      when(lh.asSInt > S(0)){
        size := (lh >> U(3)).resize(32)
        state := overall_state.states(3)
      }.otherwise{
        val addr = srcOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointer, U(12), U(16))
        issueReq(io.Mreq, addr, False, U(4), U(0), issued) { rd =>
          array_length := rd(31 downto 0)
          state := overall_state.states(3)
        }
      }
    }

    is(overall_state.states(3)){
      when(lh.asSInt < S(0)){
        val temp = ((array_length << lh(7 downto 0)) + lh(23 downto 16)).resize(32)
        size := Mux(temp(2 downto 0) =/= U(0), (temp >> U(3)) + U(1), temp >> U(3)).resize(32)
      }
      issueReq(io.Mreq, regionAttrPtr, False, U(2), U(0), issued){ rd =>
        src_region_attr := rd(15 downto 0)
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      val region_attr_type = src_region_attr(15 downto 8)
      val dest_attr_addr = (io.ConfigIO.ParScanThreadStatePtr + U"x178" + region_attr_type * U(2)).resize(GCElementWidth)
      dest_attr_ptr := dest_attr_addr
      age := U(0)

      when(region_attr_type === U(0)){
        when(!markWord(0)){
          val addr = Mux(markWord(1), markWord ^ U(2, GCElementWidth bits), markWord)
          issueReq(io.Mreq, addr, False, U(8), U(0), issued){ rd =>
            new_mark := rd(GCElementWidth - 1 downto 0)
            state := overall_state.states(5)
          }
        }.otherwise{
          age := ((markWord >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32)
          state := overall_state.states(5)
        }
      }.otherwise{
        issueReq(io.Mreq, dest_attr_addr, False, U(2), U(0), issued) { rd =>
          dest_region_attr := rd(15 downto 0)
          state := overall_state.states(6)
        }
      }
    }

    is(overall_state.states(5)){
      val cond = src_region_attr(15 downto 8) === U(0) && !markWord(0)
      val temp = Mux(cond, ((new_mark >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32), age)
      when(cond){
        age := ((new_mark >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32)
      }

      when(temp < io.ConfigIO.AgeThreshold){
        dest_attr_ptr := regionAttrPtr
        dest_region_attr := src_region_attr
        state := overall_state.states(6)
      }.otherwise{
        issueReq(io.Mreq, dest_attr_ptr, False, U(2), U(0), issued) { rd =>
          dest_region_attr := rd(15 downto 0)
          state := overall_state.states(6)
        }
      }
    }

    is(overall_state.states(6)){
      val addr = (io.ConfigIO.HeapRegionBiasedBase + (srcOopPtr >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued){ rd =>
        from_region := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      val dest_attr_type = dest_region_attr(15 downto 8)
      val addr = (io.ConfigIO.PlabAllocatorPtr + U"x10" + dest_attr_type * U(8)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        buffer_ptr := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(8)){
      issueReq(io.Mreq, buffer_ptr, False, U(8), U(0), issued) { rd =>
        buffer := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      val addr = (buffer + U"x30").resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, False, U(16), U(0), issued) { rd =>
        region_top := rd(GCElementWidth - 1 downto 0)
        region_end := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      when((region_end - region_top) / U(8) >= size){
        destOopPtr := region_top
        val addr = buffer + U"x30"
        val writeValue = (region_top + size * U(8)).resize(GCElementWidth)
        issueReq(io.Mreq, addr, True, U(8), writeValue, issued){ _ =>
          region_top := writeValue
          state := overall_state.states(11)
        }
      }.otherwise{
        sendToAllocate(False)
      }
    }

    is(overall_state.states(11)){
      val writeValue = Cat(destOopPtr(GCElementWidth - 1 downto 2), U(3, 2 bits)).resize(GCElementWidth).asUInt
      issueReq(io.Mreq, srcOopPtr, True, U(8), writeValue, issued) { _ =>
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(12)){
      val addr = from_region + U(256)
      issueReq(io.Mreq, addr, False, U(4), U(0), issued) { rd =>
        young_index := rd(31 downto 0)
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(13)){
      val addr = (io.ConfigIO.YoungWordsBase + young_index * U(8)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        temp_value := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      val addr = (io.ConfigIO.YoungWordsBase + young_index * U(8)).resize(MMUAddrWidth)
      val writeValue = (temp_value + size).resize(GCElementWidth)
      issueReq(io.Mreq, addr, True, U(8), writeValue, issued) { _ =>
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(15)){
      val cond = dest_region_attr(15 downto 8) === U(0) && !markWord(0)
      val temp = (markWord & ~(U"x1111" << 3).resize(GCElementWidth)) | ((Mux(age + U(1) < U(15), age + U(1), age) & U(x"1111", 32 bits)) << U(3)).resize(GCElementWidth)
      val writeValue = Mux(cond, temp, markWord)
      issueReq(io.Mreq, destOopPtr, True, U(8), writeValue, issued){ _ =>
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      val cond = dest_region_attr(15 downto 8) === U(0) && !markWord(0)
      when(cond){
        val addr = Mux(markWord(1), markWord ^ U(2, GCElementWidth bits), markWord)
        issueReq(io.Mreq, addr, False, U(8), U(0), issued){ rd =>
          temp_value := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(17)
        }
      }.otherwise{
        state := overall_state.states(18)
      }
    }

    is(overall_state.states(17)){
      val addr = Mux(markWord(1), markWord ^ U(2, GCElementWidth bits), markWord)
      val writeValue = (temp_value & ~(U"x1111" << 3).resize(GCElementWidth)) | ((Mux(age + U(1) < U(15), age + U(1), age) & U(x"1111", 32 bits)) << U(3)).resize(GCElementWidth)
      issueReq(io.Mreq, addr, True, U(8), writeValue, issued){ _ =>
        state := overall_state.states(18)
      }
    }

    is(overall_state.states(18)){
      val needTrace = kid =/= U(TypeArrayKlassID, 32 bits)

      io.ToCopy.Valid := !issuedCopy
      io.ToCopy.Size := size - U(1)
      io.ToCopy.SrcOopPtr := srcOopPtr + U(8)
      io.ToCopy.DestOopPtr := destOopPtr + U(8)

      io.ToTrace.Valid := needTrace && !issuedTrace && (io.ToCopy.firstBeatDone || copyFirstBeatDone)
      io.ToTrace.OopType := U(NotArrayOop)
      io.ToTrace.KlassPtr := klassPtr
      io.ToTrace.SrcOopPtr := srcOopPtr
      io.ToTrace.DestOopPtr := destOopPtr
      io.ToTrace.Kid := kid
      io.ToTrace.ScanningInYoung := dest_region_attr(15 downto 8) === U(0, 8 bits)
      io.ToTrace.ArrayLength := array_length
      io.ToTrace.PartialArrayStart := U(0)
      io.ToTrace.StepIndex := (array_length % io.ConfigIO.ChunkSize).resize(32)
      io.ToTrace.StepNCreate := Mux(array_length > (array_length % io.ConfigIO.ChunkSize), U(1), U(0)).resize(32)

      val copyFire = io.ToCopy.Valid && io.ToCopy.Ready
      val traceFire = io.ToTrace.Valid && io.ToTrace.Ready

      when(copyFire){
        issuedCopy := True
        dbg(Seq("send the task to copy module"))
      }

      when(traceFire){
        issuedTrace := True
        dbg(Seq("send the task to trace module"))
      }

      val copyIssuedDone  = copyFire || issuedCopy
      val traceIssuedDone = traceFire || issuedTrace || !needTrace

      when(copyIssuedDone && traceIssuedDone) {
        issuedCopy  := False
        issuedTrace := False
        state := overall_state.states(19)
      }
    }

    is(overall_state.states(19)){
      val needTrace = kid =/= U(TypeArrayKlassID, 32 bits)
      val copyFinished = copyDone || io.ToCopy.Done
      val traceFinished = traceDone || io.ToTrace.Done || !needTrace

      when(copyFinished && traceFinished){
        copyDone := False
        traceDone := False
        copyFirstBeatDone := False

        io.ToCopySurvivor.Done := True
        io.ToCopySurvivor.DestOopPtr := destOopPtr
        state := overall_state.states(0)
      }
    }

    is(overall_state.states(20)){
      when(io.ToAllocate.Done || allocate_done){
        allocate_done := False
        destOopPtr := io.ToAllocate.DestObjPtr
        state := Mux(allocateSelect, overall_state.states(25), overall_state.states(21))
      }
    }

    is(overall_state.states(21)){
      when(destOopPtr === U(0)){
        val addr = io.ConfigIO.PlabAllocatorPtr + U"x18"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          buffer_ptr := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(22)
        }
      }.otherwise{
        state := overall_state.states(11)
      }
    }

    is(overall_state.states(22)){
      issueReq(io.Mreq, buffer_ptr, False, U(8), U(0), issued) { rd =>
        buffer := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(23)
      }
    }

    is(overall_state.states(23)){
      val addr = buffer + U"x30"
      issueReq(io.Mreq, addr, False, U(16), U(0), issued) { rd =>
        region_top := rd(GCElementWidth - 1 downto 0)
        region_end := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        dest_region_attr := (dest_region_attr & U(x"ff", 16 bits)) | U(x"100", 16 bits)

        state := overall_state.states(24)
      }
    }

    is(overall_state.states(24)){
      when((region_end - region_top) / U(8) >= size){
        destOopPtr := region_top
        val addr = buffer + U"x30"
        val writeValue = (region_top + size * U(8)).resize(GCElementWidth)
        issueReq(io.Mreq, addr, True, U(8), writeValue, issued) { _ =>
          region_top := writeValue
          state := overall_state.states(25)
        }
      }.otherwise{
        sendToAllocate(True)
      }
    }

    is(overall_state.states(25)){
      val addr = dest_attr_ptr + U(1)
      issueReq(io.Mreq, addr, True, U(1), U(1), issued) { _ =>
        state := overall_state.states(11)
      }
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}