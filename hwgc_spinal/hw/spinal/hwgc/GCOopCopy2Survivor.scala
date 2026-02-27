package hwgc

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

class GCOopCopy2Survivor extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToAllocate = master(new GCToAllocate)
    val Process2Copy = master(new GCToCopy)
    val Process2Trace = master(new GCToTrace)
    val Process2CopySurvivor = slave(new GCProcess2Survivor)
    val ConfigIO = slave(new GCCopy2SurvivorConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ToAllocate.Valid := False
  io.ToAllocate.DestAttrType := U(0)
  io.ToAllocate.Size := U(0)

  io.Process2Copy.Valid := False
  io.Process2Copy.SrcOopPtr := U(0)
  io.Process2Copy.DestOopPtr := U(0)
  io.Process2Copy.Size := U(0)

  io.Process2CopySurvivor.DestOopPtr := U(0)
  io.Process2CopySurvivor.Done := False
  io.Process2CopySurvivor.Ready := False

  io.Process2Trace.Valid := False
  io.Process2Trace.Kid := U(0)
  io.Process2Trace.OopType := U(0)
  io.Process2Trace.KlassPtr := U(0)
  io.Process2Trace.SrcOopPtr := U(0)
  io.Process2Trace.DestOopPtr := U(0)
  io.Process2Trace.ScanningInYoung := False
  io.Process2Trace.StepIndex := U(0)
  io.Process2Trace.StepNCreate := U(0)
  io.Process2Trace.ArrayLength := U(0)
  io.Process2Trace.PartialArrayStart := U(0)

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(27)(i => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))

  val markWord = RegInit(U(0, GCElementWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val regionAttrPtr = RegInit(U(0, GCElementWidth bits))

  val klass_ptr = RegInit(U(0, GCElementWidth bits))
  val lh = RegInit(U(0, 32 bits))
  val kid = RegInit(U(0, 32 bits))
  val arrayLength = RegInit(U(0, 32 bits))
  val size = RegInit(U(0, 32 bits))
  val srcRegionAttr = RegInit(U(0, 16 bits))
  val age = RegInit(U(0, 32 bits))
  val new_mark = RegInit(U(0, GCElementWidth bits))
  val dest_attr_ptr = RegInit(U(0, GCElementWidth bits))
  val destRegionAttr = RegInit(U(0, 16 bits))
  val bufferPtr = RegInit(U(0, GCElementWidth bits))
  val buffer = RegInit(U(0, GCElementWidth bits))
  val region_top = RegInit(U(0, GCElementWidth bits))
  val region_end = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val from_region = RegInit(U(0, GCElementWidth bits))
  val young_index = RegInit(U(0, 32 bits))
  val originValue = RegInit(U(0, GCElementWidth bits))

  val issued = RegInit(False)
  val issuedCopy = RegInit(False)
  val issuedTrace = RegInit(False)
  val copyDone = RegInit(False)
  val traceDone = RegInit(False)
  val allocateSelect = RegInit(False)

  switch(state){
    is(overall_state.states(0)){
      io.Process2CopySurvivor.Ready := True
      when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
        srcOopPtr := io.Process2CopySurvivor.SrcOopPtr
        markWord := io.Process2CopySurvivor.MarkWord
        regionAttrPtr := io.Process2CopySurvivor.RegionAttrPtr

        state := overall_state.states(1)

        if(DebugEnable){
          report(Seq(
            "[GCOopCopy2Survivor<", io.DebugTimeStamp,
            ">]Receive task from OopProcess",
            "\n"
          ))
        }
      }
    }

    is(overall_state.states(1)){
      val addr = (srcOopPtr + U"x8").resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        klass_ptr := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      val compressedKlassPtr = (io.ConfigIO.CompressedKlassPointerBase + klass_ptr << io.ConfigIO.CompressedKlassPointerShift).resize(GCElementWidth bits)
      val addr = Mux(io.ConfigIO.UseCompressedKlassPointer, compressedKlassPtr, klass_ptr)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        klass_ptr := addr
        lh := rd(31 downto 0)
        kid := rd(63 downto 32)
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      when(lh > U(0)){
        size := (lh >> U(3)).resize(32 bits)
        state := overall_state.states(4)
      }.otherwise{
        val addr = (srcOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointer, U(12), U(16))).resize(MMUAddrWidth bits)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          arrayLength := rd(31 downto 0)
          state := overall_state.states(4)
        }
      }
    }

    is(overall_state.states(4)){
      when(lh < U(0)){
        val temp = ((arrayLength << lh(7 downto 0)) + lh(23 downto 16)).resize(32 bits)
        size := Mux(temp(2 downto 0) =/= U(0), (temp >> U(3)) + U(1), temp >> U(3)).resize(32 bits)
      }
      issueReq(io.Mreq, regionAttrPtr, False, U(0), U(0), issued){ rd =>
        srcRegionAttr := rd(15 downto 0)
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      val region_attr_type = srcRegionAttr(15 downto 8)
      dest_attr_ptr := (io.ConfigIO.ParScanThreadStatePtr + U"x178" + region_attr_type * U(2)).resize(GCElementWidth bits)
      age := U(0)

      when(region_attr_type === U(0)){
        when(!markWord(0)){
          val addr = Mux(markWord(1), markWord ^ U(2), markWord).resize(MMUAddrWidth bits)
          issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
            new_mark := rd(GCElementWidth - 1 downto 0)
            state := overall_state.states(6)
          }
        }.otherwise{
          age := ((markWord >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32 bits)
          state := overall_state.states(6)
        }
      }.otherwise{
        val addr = (io.ConfigIO.ParScanThreadStatePtr + U"x178" + region_attr_type * U(2)).resize(MMUAddrWidth bits)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          destRegionAttr := rd(15 downto 0)
          state := overall_state.states(7)
        }
      }
    }

    is(overall_state.states(6)){
      val cond = srcRegionAttr(15 downto 8) === U(0) && !markWord(0)
      val temp = Mux(cond, ((new_mark >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32 bits), age)
      when(cond){
        age := ((new_mark >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32 bits)
      }

      when(temp < io.ConfigIO.AgeThreshold){
        dest_attr_ptr := regionAttrPtr
        destRegionAttr := srcRegionAttr
        state := overall_state.states(7)
      }.otherwise{
        issueReq(io.Mreq, dest_attr_ptr, False, U(0), U(0), issued) { rd =>
          destRegionAttr := rd(15 downto 0)
          state := overall_state.states(7)
        }
      }
    }

    is(overall_state.states(7)){
      val addr = (io.ConfigIO.HeapRegionBiasedBase + (srcOopPtr >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
        from_region := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(8)){
      val dest_attr_type = destRegionAttr(15 downto 8)
      val addr = (io.ConfigIO.PlabAllocatorPtr + U"x10" + dest_attr_type * 8).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        bufferPtr := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      issueReq(io.Mreq, bufferPtr, False, U(0), U(0), issued) { rd =>
        buffer := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      val addr = (buffer + U"x30").resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        region_top := rd(GCElementWidth - 1 downto 0)
        region_end := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        state := overall_state.states(11)
      }
    }

    is(overall_state.states(11)){
      when((region_end - region_top) / U(8) >= size){
        destOopPtr := region_top
        val addr = buffer + U"x30"
        val writeValue = (region_top + size * U(8)).resize(GCElementWidth bits)
        issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued){ rd =>
          region_top := writeValue
          state := overall_state.states(12)
        }
      }.otherwise{
        allocateSelect := False
        io.ToAllocate.Valid := True
        io.ToAllocate.Size := size
        io.ToAllocate.DestAttrType := destRegionAttr(15 downto 8)

        when(io.ToAllocate.Valid && io.ToAllocate.Ready){
          state := overall_state.states(21)
        }
      }
    }

    is(overall_state.states(12)){
      // ALLOC return
      val writeValue = Cat(destOopPtr(GCElementWidth - 1 downto 2), U(3)).resize(GCElementWidth bits)
      issueReq(io.Mreq, srcOopPtr, True, getWstrb(8), writeValue.asUInt, issued) { rd =>
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(13)){
      val addr = (from_region + U(256)).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        young_index := rd(31 downto 0)
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      val addr = (io.ConfigIO.YoungWordsBase + young_index * U(8)).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        originValue := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(15)){
      val addr = (io.ConfigIO.YoungWordsBase + young_index * U(8)).resize(MMUAddrWidth bits)
      val writeValue = (originValue + size).resize(GCElementWidth bits)
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { rd =>
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      val cond = destRegionAttr(15 downto 8) === U(0) && !markWord(0)
      val temp = (markWord & ~(U"x1111" << 3).resize(GCElementWidth bits)) | ((Mux(age + U(1) < U(15), age + U(1), age) & U(x"1111", 32 bits)) << 3).resize(GCElementWidth bits)
      val writeValue = Mux(cond, temp, markWord)
      issueReq(io.Mreq, destOopPtr, True, getWstrb(8), writeValue, issued){ rd =>
        state := overall_state.states(17)
      }
    }

    is(overall_state.states(17)){
      val cond = destRegionAttr(15 downto 8) === U(0) && !markWord(0)
      when(cond){
        val addr = Mux(markWord(1), markWord ^ U(2), markWord).resize(MMUAddrWidth bits)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
          originValue := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(18)
        }
      }.otherwise{
        state := overall_state.states(19)
      }
    }

    is(overall_state.states(18)){
      val addr = Mux(markWord(1), markWord ^ U(2), markWord).resize(MMUAddrWidth bits)
      val writeValue = (originValue & ~(U"x1111" << 3).resize(GCElementWidth bits)) | ((Mux(age + U(1) < U(15), age + U(1), age) & U(x"1111", 32 bits)) << 3).resize(GCElementWidth bits)
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued){ rd =>
        state := overall_state.states(19)
      }
    }

    is(overall_state.states(19)){
      val needTrace = kid =/= U(TypeArrayKlassID, 32 bits)

      io.Process2Copy.Valid := !issuedCopy
      io.Process2Copy.Size := size - U(1)
      io.Process2Copy.SrcOopPtr := srcOopPtr + U(8)
      io.Process2Copy.DestOopPtr := destOopPtr + U(8)

      io.Process2Trace.Valid := needTrace && !issuedTrace
      io.Process2Trace.OopType := U(NotArrayOop)
      io.Process2Trace.KlassPtr := klass_ptr
      io.Process2Trace.SrcOopPtr := srcOopPtr
      io.Process2Trace.DestOopPtr := destOopPtr

      io.Process2Trace.Kid := kid
      io.Process2Trace.ScanningInYoung := destRegionAttr(15 downto 8) === U(Type_Young, 8 bits)

      io.Process2Trace.ArrayLength := arrayLength
      io.Process2Trace.PartialArrayStart := U(0)
      io.Process2Trace.StepIndex := (arrayLength % io.ConfigIO.ChunkSize).resize(32 bits)
      io.Process2Trace.StepNCreate := Mux(arrayLength > (arrayLength % io.ConfigIO.ChunkSize), U(1), U(0)).resize(32 bits)

      val copyFire = io.Process2Copy.Valid && io.Process2Copy.Ready
      val traceFire = io.Process2Trace.Valid && io.Process2Trace.Ready

      when(copyFire){
        issuedCopy := True
        if(DebugEnable){
          report(Seq(
            "[GCOopCopy2Survivor<", io.DebugTimeStamp,
            ">]send the task to copy module",
            "\n"
          ))
        }
      }

      when(traceFire){
        issuedTrace := True
        if(DebugEnable){
          report(Seq(
            "[GCOopCopy2Survivor<", io.DebugTimeStamp,
            ">]send the task to trace module",
            "\n"
          ))
        }
      }

      val copyIssuedDone  = copyFire || issuedCopy
      val traceIssuedDone = traceFire || issuedTrace || !needTrace

      when(copyIssuedDone && traceIssuedDone) {
        issuedCopy  := False
        issuedTrace := False
        state := overall_state.states(20)
      }
    }

    is(overall_state.states(20)){
      when(io.Process2Copy.Done){
        copyDone := True
      }
      when(io.Process2Trace.Done){
        traceDone := True
      }

      val needTrace = kid =/= TypeArrayKlassID
      val copyFinished = copyDone || io.Process2Copy.Done
      val traceFinished = traceDone || io.Process2Trace.Done || !needTrace

      when(copyFinished && traceFinished){
        copyDone := False
        traceDone := False

        io.Process2CopySurvivor.Done := True
        io.Process2CopySurvivor.DestOopPtr := destOopPtr

        state := overall_state.states(0)
      }
    }

    is(overall_state.states(21)){
      when(io.ToAllocate.Done){
        destOopPtr := io.ToAllocate.DestObjPtr
        state := Mux(allocateSelect, overall_state.states(26), overall_state.states(22))
      }
    }

    is(overall_state.states(22)){
      when(destOopPtr === U(0)){
        val addr = (io.ConfigIO.PlabAllocatorPtr + U"x18").resize(MMUAddrWidth bits)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          bufferPtr := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(23)
        }
      }.otherwise{
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(23)){
      issueReq(io.Mreq, bufferPtr, False, U(0), U(0), issued) { rd =>
        buffer := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(24)
      }
    }

    is(overall_state.states(24)){
      val addr = (buffer + U"x30").resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        region_top := rd(GCElementWidth - 1 downto 0)
        region_end := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        destRegionAttr := (destRegionAttr & U(x"ff", 16 bits)) | U(x"100", 16 bits)

        state := overall_state.states(25)
      }
    }

    is(overall_state.states(25)){
      when((region_end - region_top) / U(8) >= size){
        destOopPtr := region_top
        val addr = (buffer + U"x30").resize(MMUAddrWidth bits)
        val writeValue = (region_top + size * U(8)).resize(GCElementWidth bits)
        issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { rd =>
          region_top := writeValue
          state := overall_state.states(26)
        }
      }.otherwise{
        allocateSelect := True
        io.ToAllocate.Valid := True
        io.ToAllocate.Size := size
        io.ToAllocate.DestAttrType := destRegionAttr(15 downto 8)

        when(io.ToAllocate.Valid && io.ToAllocate.Ready){
          state := overall_state.states(21)
        }
      }
    }

    is(overall_state.states(26)){
      val addr = dest_attr_ptr + U(1)
      issueReq(io.Mreq, addr, True, getWstrb(1), U(1), issued) { rd =>
        state := overall_state.states(12)
      }
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}