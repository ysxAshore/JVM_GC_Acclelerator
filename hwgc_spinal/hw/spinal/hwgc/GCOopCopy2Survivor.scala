package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCOopCopy2Survivor extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToCopy = master(new GCToCopy)
    val ToTrace = master(new GCToTrace)
    val ToStack = master(new GCUpdatedRegion)
    val ToAllocate = master(new GCToAllocate)
    val ToCopySurvivor = slave(new GCToSurvivor)
    val TaskDone = in(Bool())
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
    val states = Array.tabulate(19)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))

  val markWord = RegInit(U(0, GCElementWidth bits))
  val klassPtr = RegInit(U(0, GCElementWidth bits))
  val srcLength = RegInit(U(0, 32 bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val srcRegionAttr = RegInit(U(0, 16 bits))
  val regionAttrPtr = RegInit(U(0, GCElementWidth bits))

  val destOopPtr = RegInit(U(0, GCElementWidth bits))

  val lh = RegInit(U(0, 32 bits))
  val kid = RegInit(U(0, 32 bits))
  val size = RegInit(U(0, 32 bits))
  val age = RegInit(U(0, 32 bits))
  val new_mark = RegInit(U(0, GCElementWidth bits))
  val dest_attr_ptr = RegInit(U(0, GCElementWidth bits))
  val dest_region_attr = RegInit(U(0, 16 bits))
  val temp_value = RegInit(U(0, GCElementWidth bits))

  // dest attr regs
  val destAttrRegionValid = RegInit(False)
  val destAttrRegionCache = RegInit(U(0, 32 bits))

  // plab Cache regs
  val plabForceOld  = RegInit(False)
  val plabTargetIdx = RegInit(U(0, 1 bits))
  val plabCacheBuffer = Vec(Reg(UInt(GCElementWidth bits)) init(0), 2)
  val plabCacheBufferPtr = Vec(Reg(UInt(GCElementWidth bits)) init(0), 2)
  val plabCacheBufferValid = Vec(RegInit(False), 2)
  val plabCacheTop       = Vec(Reg(UInt(GCElementWidth bits)) init(0), 2)
  val plabCacheEnd       = Vec(Reg(UInt(GCElementWidth bits)) init(0), 2)
  val plabCacheValid = Vec(RegInit(False), 2)
  val waitWriteRegionTop = RegInit(False)

  val issued = RegInit(False)
  val writeSrcOopPtr = RegInit(False)
  val issuedCopy = RegInit(False)
  val issuedTrace = RegInit(False)
  val copyDone = RegInit(False)
  val traceDone = RegInit(False)

  // alloc regs
  val allocateDone = RegInit(False)
  val allocateSelect = RegInit(False)
  val waitAllocateFire = RegInit(False)

  io.ToStack.Valid0 := plabCacheValid(0)
  io.ToStack.Valid1 := plabCacheValid(1)
  io.ToStack.Buffer0 := plabCacheBuffer(0)
  io.ToStack.Buffer1 := plabCacheBuffer(1)
  io.ToStack.RegionTop0 := plabCacheTop(0)
  io.ToStack.RegionTop1 := plabCacheTop(1)

  when(io.TaskDone){
    destAttrRegionValid := False
    plabCacheValid(0) := False
    plabCacheValid(1) := False
    plabCacheBufferValid(0) := False
    plabCacheBufferValid(1) := False
  }
  when(io.ToCopy.Done){
    copyDone := True
  }
  when(io.ToTrace.Done){
    traceDone := True
  }
  when(io.ToAllocate.Done){
    allocateDone := True
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCAop<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def allocateCacheWithIdx(idx: UInt, selectOld: Bool): Unit = {
    when(((plabCacheEnd(idx) - plabCacheTop(idx)) / U(8)) >= size){
      destOopPtr := plabCacheTop(idx)
      plabCacheTop(idx) := (plabCacheTop(idx) + size * U(8)).resize(GCElementWidth)
      state := overall_state.states(8)
    }.otherwise{
      val addr = plabCacheBuffer(idx) + U"x30"

      when(!waitWriteRegionTop) {
        issueReq(io.Mreq, addr, True, U(8), plabCacheTop(idx), issued) { _ =>
          waitWriteRegionTop := True
        }
      }

      allocateSelect := selectOld
      when(!waitAllocateFire) {
        io.ToAllocate.Valid := True
        io.ToAllocate.Size := size
        io.ToAllocate.DestAttrType := Mux(selectOld, U(1, 8 bits), dest_region_attr(15 downto 8))
      }

      when(io.ToAllocate.Valid && io.ToAllocate.Ready){
        waitAllocateFire := True
      }

      when((io.ToAllocate.Valid && io.ToAllocate.Ready || waitAllocateFire) && (waitWriteRegionTop || io.Mreq.Response.fire)){
        waitAllocateFire := False
        waitWriteRegionTop := False
        plabCacheValid(idx) := False
        state := overall_state.states(13)
      }
    }
  }

  def resolveDestAttr(regionType: UInt): Unit = {
    val dest_attr_addr = (io.ConfigIO.ParScanThreadStatePtr + U"x178").resize(GCElementWidth)
    dest_attr_ptr := dest_attr_addr + regionType * 2

    when(!destAttrRegionValid){
      issueReq(io.Mreq, dest_attr_addr, False, U(2), U(0), issued) { rd =>
        destAttrRegionValid := True
        destAttrRegionCache := rd(31 downto 0)
        dest_region_attr := Mux(regionType === U(1), rd(31 downto 16), rd(15 downto 0))
        plabTargetIdx := Mux(regionType === U(1), rd(31 downto 16), rd(15 downto 0))(15 downto 8).resize(1)
        state := overall_state.states(4)
      }
    }.otherwise{
      dest_region_attr := Mux(regionType === U(1), destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0))
      plabTargetIdx := Mux(regionType === U(1), destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0))(15 downto 8).resize(1)
      state := overall_state.states(4)
    }
  }


  switch(state){
    is(overall_state.states(0)){
      io.ToCopySurvivor.Ready := True
      when(io.ToCopySurvivor.Valid && io.ToCopySurvivor.Ready){
        srcOopPtr := io.ToCopySurvivor.SrcOopPtr
        markWord := io.ToCopySurvivor.MarkWord
        klassPtr := io.ToCopySurvivor.KlassPtr
        srcLength := io.ToCopySurvivor.SrcLength
        srcRegionAttr := io.ToCopySurvivor.SrcRegionAttr
        regionAttrPtr := io.ToCopySurvivor.RegionAttrPtr

        age := U(0)
        issued := False

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
      }.otherwise{
        val temp = ((srcLength << lh(7 downto 0)) + lh(23 downto 16)).resize(32)
        size := Mux(temp(2 downto 0) =/= U(0), (temp >> U(3)) + U(1), temp >> U(3)).resize(32)
      }

      plabForceOld := False

      val region_attr_type = srcRegionAttr(15 downto 8)
      when(region_attr_type === U(0)){
        when(!markWord(0)){
          val addr = Mux(markWord(1), markWord ^ U(2, GCElementWidth bits), markWord)
          issueReq(io.Mreq, addr, False, U(8), U(0), issued){ rd =>
            new_mark := rd(GCElementWidth - 1 downto 0)
            state := overall_state.states(3)
          }
        }.otherwise{
          age := ((markWord >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32)
          state := overall_state.states(3)
        }
      }.otherwise{
        resolveDestAttr(region_attr_type)
      }
    }

    is(overall_state.states(3)){
      val cond = srcRegionAttr(15 downto 8) === U(0) && !markWord(0)
      val temp = Mux(cond, ((new_mark >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32), age)
      when(cond){
        age := ((new_mark >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32)
      }

      when(temp < io.ConfigIO.AgeThreshold){
        dest_attr_ptr := regionAttrPtr
        dest_region_attr := srcRegionAttr
        plabTargetIdx := srcRegionAttr(15 downto 8).resize(1)
        state := overall_state.states(4)
      }.otherwise{
        resolveDestAttr(srcRegionAttr(15 downto 8))
      }
    }

    is(overall_state.states(4)){
      when(plabCacheValid(plabTargetIdx)){
        state := overall_state.states(7)
      }.elsewhen(plabCacheBufferValid(plabTargetIdx)) {
        state := overall_state.states(6)
      }.otherwise {
        val addr = (io.ConfigIO.PlabAllocatorPtr + U"x10" + plabTargetIdx * U(8)).resize(MMUAddrWidth)
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          plabCacheBufferPtr(plabTargetIdx) := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(5)
        }
      }
    }

    is(overall_state.states(5)){
      issueReq(io.Mreq, plabCacheBufferPtr(plabTargetIdx), False, U(8), U(0), issued) { rd =>
        plabCacheBuffer(plabTargetIdx) := rd(GCElementWidth - 1 downto 0)
        plabCacheBufferValid(plabTargetIdx) := True
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(6)){
      val addr = plabCacheBuffer(plabTargetIdx) + U"x30"
      issueReq(io.Mreq, addr, False, U(16), U(0), issued) { rd =>
        plabCacheTop(plabTargetIdx) := rd(GCElementWidth - 1 downto 0)
        plabCacheEnd(plabTargetIdx) := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        plabCacheValid(plabTargetIdx) := True

        when(plabForceOld){
          dest_region_attr := (dest_region_attr & U(x"00ff", 16 bits)) | U(x"0100", 16 bits)
        }

        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      allocateCacheWithIdx(plabTargetIdx, plabForceOld)
    }

    is(overall_state.states(8)){
      val needTrace = kid =/= U(TypeArrayKlassID, 32 bits)
      val totalSize = (size * U(8)).resize(size.getWidth)
      val compressedSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(16), U(20))

      io.ToCopy.Valid := !issuedCopy
      io.ToCopy.Size := Mux(kid === U(ObjectArrayKlassID, 32 bits), totalSize - compressedSize, totalSize - U(8))
      io.ToCopy.SrcOopPtr := Mux(kid === U(ObjectArrayKlassID, 32 bits), srcOopPtr + compressedSize, srcOopPtr + U(8))
      io.ToCopy.DestOopPtr := Mux(kid === U(ObjectArrayKlassID, 32 bits), destOopPtr + compressedSize, destOopPtr + U(8))

      io.ToTrace.Valid := needTrace && !issuedTrace
      io.ToTrace.OopType := U(NotArrayOop)
      io.ToTrace.KlassPtr := klassPtr
      io.ToTrace.SrcOopPtr := srcOopPtr
      io.ToTrace.DestOopPtr := destOopPtr
      io.ToTrace.Kid := kid
      io.ToTrace.ScanningInYoung := dest_region_attr(15 downto 8) === U(0, 8 bits)
      io.ToTrace.ArrayLength := srcLength
      io.ToTrace.PartialArrayStart := U(0)
      io.ToTrace.StepIndex := (srcLength % io.ConfigIO.ChunkSize).resize(32)
      io.ToTrace.StepNCreate := Mux(srcLength > (srcLength % io.ConfigIO.ChunkSize), U(1), U(0)).resize(32)

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

      when(!writeSrcOopPtr) {
        val writeValue = Cat(destOopPtr(GCElementWidth - 1 downto 2), U(3, 2 bits)).resize(GCElementWidth).asUInt
        issueReq(io.Mreq, srcOopPtr, True, U(8), writeValue, issued) { _ =>
          writeSrcOopPtr := True
        }
      }

      when(copyIssuedDone && traceIssuedDone && writeSrcOopPtr) {
        issuedCopy  := False
        issuedTrace := False
        writeSrcOopPtr := False
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      val cond = dest_region_attr(15 downto 8) === U(0) && !markWord(0)
      val temp = (markWord & ~(U"x1111" << 3).resize(GCElementWidth)) | ((Mux(age + U(1) < U(15), age + U(1), age) & U(x"1111", 32 bits)) << U(3)).resize(GCElementWidth)
      val writeValue = Mux(cond, temp, markWord)
      issueReq(io.Mreq, destOopPtr, True, U(8), writeValue, issued){ _ =>
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      val cond = dest_region_attr(15 downto 8) === U(0) && !markWord(0)
      when(cond){
        val addr = Mux(markWord(1), markWord ^ U(2, GCElementWidth bits), markWord)
        issueReq(io.Mreq, addr, False, U(8), U(0), issued){ rd =>
          temp_value := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(11)
        }
      }.otherwise{
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(11)){
      val addr = Mux(markWord(1), markWord ^ U(2, GCElementWidth bits), markWord)
      val writeValue = (temp_value & ~(U"x1111" << 3).resize(GCElementWidth)) | ((Mux(age + U(1) < U(15), age + U(1), age) & U(x"1111", 32 bits)) << U(3)).resize(GCElementWidth)
      issueReq(io.Mreq, addr, True, U(8), writeValue, issued){ _ =>
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(12)){
      val needTrace = kid =/= U(TypeArrayKlassID, 32 bits)
      val copyFinished = copyDone || io.ToCopy.Done
      val traceFinished = traceDone || io.ToTrace.Done || !needTrace

      when(copyFinished && traceFinished){
        copyDone := False
        traceDone := False

        io.ToCopySurvivor.Done := True
        io.ToCopySurvivor.DestOopPtr := destOopPtr
        state := overall_state.states(0)
      }
    }

    is(overall_state.states(13)){
      when(io.ToAllocate.Done || allocateDone){
        allocateDone := False
        destOopPtr := io.ToAllocate.DestObjPtr
        state := Mux(allocateSelect, overall_state.states(15), overall_state.states(14))
      }
    }

    is(overall_state.states(14)){
      when(destOopPtr === U(0)){
        plabTargetIdx := U(1)
        plabForceOld := True
        state := overall_state.states(4)
      }.otherwise{
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(15)){
      val addr = dest_attr_ptr + U(1)
      issueReq(io.Mreq, addr, True, U(1), U(1), issued) { _ =>
        state := overall_state.states(8)
      }
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}