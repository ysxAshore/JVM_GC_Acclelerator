package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class CopySurvivorSlotCtx() extends Bundle with GCParameters {
  val owner = UInt(1 bits)
  val markWord = UInt(GCElementWidth bits)
  val klassPtr = UInt(GCElementWidth bits)
  val srcLength = UInt(32 bits)
  val srcOopPtr = UInt(GCElementWidth bits)
  val srcRegionAttr = UInt(16 bits)
  val regionAttrPtr = UInt(GCElementWidth bits)

  val destOopPtr = UInt(GCElementWidth bits)

  val lh = UInt(32 bits)
  val kid = UInt(32 bits)
  val size = UInt(32 bits)
  val age = UInt(32 bits)

  val destAttrPtr = UInt(GCElementWidth bits)
  val destRegionAttr = UInt(16 bits)

  val plabTargetIdx = UInt(1 bits)
  val plabForceOld = Bool()

  val writeSrcOopPtrDone = Bool()

  val copyIssued = Bool()
  val traceIssued = Bool()
  val copyDone = Bool()
  val traceDone = Bool()

  val allocIssued = Bool()
  val allocDone = Bool()
  val allocSelect = Bool()
  val waitWriteRegionTop = Bool()
}

class GCOopCopy2Survivor extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToCopy = master(new GCToCopy)
    val ToFetch = master(new GCWriteSrcOopPtr)
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
  io.Mreq.Response.ready := True

  io.ToCopy.clearIn()
  io.ToFetch.clearIn()
  io.ToTrace.clearIn()
  io.ToAllocate.clearIn()
  io.ToCopySurvivor.Done := False
  io.ToCopySurvivor.DoneOwner := U(0)
  io.ToCopySurvivor.DestOopPtr := U(0)
  io.ToCopySurvivor.isTypeArray := False

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(14)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  object MmuOp extends SpinalEnum {
    val NONE, READ_KLASS, READ_DEST_ATTR, READ_PLAB_PTR, READ_PLAB_BUF, READ_PLAB_TOPEND, WRITE_SRC_FWD, WRITE_MARK, WRITE_REGION_TOP, WRITE_FORCE_OLD = newElement()
  }


  // slot state and context
  val slotValid = Vec(RegInit(False), 2)
  val slotState = Vec(RegInit(overall_state.states(0)), 2)
  val slotCtx = Vec.fill(2)(Reg(CopySurvivorSlotCtx()) init CopySurvivorSlotCtx().getZero)


  // shared caches
  val destAttrRegionValid = RegInit(False)
  val destAttrRegionCache = RegInit(U(0, 32 bits))

  val plabCacheBuffer = Vec(Reg(UInt(GCElementWidth bits)) init(0), 2)
  val plabCacheBufferPtr = Vec(Reg(UInt(GCElementWidth bits)) init(0), 2)
  val plabCacheBufferValid = Vec(RegInit(False), 2)
  val plabCacheTop = Vec(Reg(UInt(GCElementWidth bits)) init(0), 2)
  val plabCacheEnd = Vec(Reg(UInt(GCElementWidth bits)) init(0), 2)
  val plabCacheValid = Vec(RegInit(False), 2)
  val plabRefillBusy = Vec(RegInit(False), 2)
  val plabRefillOwner = Vec(Reg(UInt(1 bits)) init(0), 2)

  val KlassCacheEntries = 16
  val klassCacheValid  = Vec(RegInit(False), KlassCacheEntries)
  val klassCachePtr    = Vec(Reg(UInt(GCElementWidth bits)) init(0), KlassCacheEntries)
  val klassCacheKidLh  = Vec(Reg(UInt(GCElementWidth bits)) init(0), KlassCacheEntries)

  val klassCacheReplacePtr = RegInit(U(0, log2Up(KlassCacheEntries) bits))


  // shared resource owners
  val mmuBusy = RegInit(False)
  val mmuOwner = Reg(UInt(1 bits)) init(0)
  val mmuOp = RegInit(MmuOp.NONE)
  val mmuIssued = Vec(RegInit(False), 2)

  val copyBusy = RegInit(False)
  val copyOwner = Reg(UInt(1 bits)) init(0)

  val traceBusy = RegInit(False)
  val traceOwner = Reg(UInt(1 bits)) init(0)

  val allocBusy = RegInit(False)
  val allocOwner = Reg(UInt(1 bits)) init(0)


  // helpers
  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCOopCopy2Survivor<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def clearSlotRuntime(i: Int): Unit = {
    slotCtx(i).destOopPtr := 0
    slotCtx(i).lh := 0
    slotCtx(i).kid := 0
    slotCtx(i).size := 0
    slotCtx(i).age := 0
    slotCtx(i).destAttrPtr := 0
    slotCtx(i).destRegionAttr := 0
    slotCtx(i).plabTargetIdx := 0
    slotCtx(i).plabForceOld := False
    slotCtx(i).writeSrcOopPtrDone := False
    slotCtx(i).copyIssued := False
    slotCtx(i).traceIssued := False
    slotCtx(i).copyDone := False
    slotCtx(i).traceDone := False
    slotCtx(i).allocIssued := False
    slotCtx(i).allocDone := False
    slotCtx(i).allocSelect := False
    slotCtx(i).waitWriteRegionTop := False
    mmuIssued(i) := False
  }

  def allocSlot(i: Int): Unit = {
    slotValid(i) := True
    slotState(i) := overall_state.states(1)

    slotCtx(i).owner := io.ToCopySurvivor.Owner
    slotCtx(i).srcOopPtr := io.ToCopySurvivor.SrcOopPtr
    slotCtx(i).markWord := io.ToCopySurvivor.MarkWord
    slotCtx(i).klassPtr := io.ToCopySurvivor.KlassPtr
    slotCtx(i).srcLength := io.ToCopySurvivor.SrcLength
    slotCtx(i).srcRegionAttr := io.ToCopySurvivor.SrcRegionAttr
    slotCtx(i).regionAttrPtr := io.ToCopySurvivor.RegionAttrPtr

    clearSlotRuntime(i)
    dbg(Seq("Allocate task to slot", i.toString, ", src=", io.ToCopySurvivor.SrcOopPtr))
  }

  def finishSlot(i: Int): Unit = {
    slotValid(i) := False
    slotState(i) := overall_state.states(0)
    clearSlotRuntime(i)
    dbg(Seq("Finish slot", i.toString))
  }

  def compressedKlassPtrOf(i: Int): UInt = (io.ConfigIO.CompressedKlassPointerBase + slotCtx(i).klassPtr(31 downto 0) << io.ConfigIO.CompressedKlassPointerShift).resize(GCElementWidth)

  def lookupKlassPtrOf(i: Int): UInt = Mux(io.ConfigIO.UseCompressedKlassPointer, compressedKlassPtrOf(i), slotCtx(i).klassPtr)

  def klassCacheHitVecOf(i: Int): Vec[Bool] = {
    val hitVec = Vec(Bool(), KlassCacheEntries)
    for(j <- 0 until KlassCacheEntries){
      hitVec(j) := klassCacheValid(j) && (klassCachePtr(j) === lookupKlassPtrOf(i))
    }
    hitVec
  }

  def klassCacheHitOf(i: Int): Bool = klassCacheHitVecOf(i).orR

  def klassCacheHitIndexOf(i: Int): UInt = OHToUInt(klassCacheHitVecOf(i).asBits)

  def checkDestRegionAttrValid(i : Int): Unit = {
    val regionAttrType = slotCtx(i).srcRegionAttr(15 downto 8)
    slotCtx(i).destAttrPtr := (io.ConfigIO.ParScanThreadStatePtr + U"x178" + regionAttrType * 2).resize(GCElementWidth)
    when(destAttrRegionValid) {
      slotCtx(i).destRegionAttr := Mux(regionAttrType === U(1), destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0))
      slotCtx(i).plabTargetIdx := Mux(regionAttrType === U(1), destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0))(15 downto 8).resize(1)
      slotState(i) := overall_state.states(5)
    }.otherwise{
      slotState(i) := overall_state.states(4)
    }
  }

  def selectDestAttrOfRegionType(regionType: UInt): UInt = Mux(regionType === U(1), destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0)).resize(16)

  def writeBackSizeForCopy(i: UInt): UInt = Mux(slotCtx(i).kid === U(ObjectArrayKlassID, 32 bits),
    Mux(io.ConfigIO.UseCompressedKlassPointer, U(16), U(20)),
    U(8)
  )

  def allocateCacheWithIdx(i : Int): Unit = {
    val idx = slotCtx(i).plabTargetIdx
    val selectOld = slotCtx(i).plabForceOld
    when(((plabCacheEnd(idx) - plabCacheTop(idx)) / U(8)) >= slotCtx(i).size){
      slotCtx(i).destOopPtr := plabCacheTop(idx)
      plabCacheTop(idx) := (plabCacheTop(idx) + slotCtx(i).size * U(8)).resize(GCElementWidth)
      slotState(i) := overall_state.states(9)
    }.otherwise {
      // send allocate and write regionTop
      slotCtx(i).allocSelect := selectOld
      when(slotCtx(i).allocIssued && slotCtx(i).waitWriteRegionTop){
        plabCacheValid(idx) := False
        slotState(i) := overall_state.states(12)
      }
    }
  }


  // task done update the cache regs
  when(io.TaskDone){
    destAttrRegionValid := False
    plabCacheValid(0) := False
    plabCacheValid(1) := False
    plabCacheBufferValid(0) := False
    plabCacheBufferValid(1) := False
  }

  io.ToStack.Valid0 := plabCacheValid(0)
  io.ToStack.Valid1 := plabCacheValid(1)
  io.ToStack.Buffer0 := plabCacheBuffer(0)
  io.ToStack.Buffer1 := plabCacheBuffer(1)
  io.ToStack.RegionTop0 := plabCacheTop(0)
  io.ToStack.RegionTop1 := plabCacheTop(1)

  // downstream done capture
  when(copyBusy && io.ToCopy.Done){
    copyBusy := False
    slotCtx(copyOwner).copyDone := True
    dbg(Seq("copy done for slot ", copyOwner))
  }

  when(traceBusy && io.ToTrace.Done){
    traceBusy := False
    slotCtx(traceOwner).traceDone := True
    dbg(Seq("trace done for slot ", traceOwner))
  }

  when(allocBusy && io.ToAllocate.Done){
    allocBusy := False
    slotCtx(allocOwner).allocDone := True
    slotCtx(allocOwner).destOopPtr := io.ToAllocate.DestObjPtr
    dbg(Seq("allocate done for slot ", allocOwner))
  }


  // single external input -> dual internal slot register
  val hasFreeSlot = !slotValid(0) || !slotValid(1)
  io.ToCopySurvivor.Ready := hasFreeSlot

  when(io.ToCopySurvivor.Valid && io.ToCopySurvivor.Ready){
    when(!slotValid(0)){
      allocSlot(0)
    }.otherwise{
      allocSlot(1)
    }
  }


  // local transitions that do not directly drive shared resources
  for(i <- 0 until 2){
    when(slotValid(i)){
      switch(slotState(i)){
        is(overall_state.states(1)){
          when(klassCacheHitOf(i)){
            val idx = klassCacheHitIndexOf(i)
            slotCtx(i).klassPtr := lookupKlassPtrOf(i)
            slotCtx(i).lh := klassCacheKidLh(idx)(31 downto 0)
            slotCtx(i).kid := klassCacheKidLh(idx)(63 downto 32)
            slotState(i) := overall_state.states(2)

            io.ToCopySurvivor.isTypeArray := klassCacheKidLh(idx)(63 downto 32) === TypeArrayKlassID
            io.ToCopySurvivor.DoneOwner := slotCtx(i).owner
          }
        }

        is(overall_state.states(2)){
          when(slotCtx(i).lh.asSInt > S(0)){
            slotCtx(i).size := (slotCtx(i).lh >> U(3)).resize(32)
          }.otherwise{
            val temp = ((slotCtx(i).srcLength << slotCtx(i).lh(7 downto 0)) + slotCtx(i).lh(23 downto 16)).resize(32)
            slotCtx(i).size := Mux(temp(2 downto 0) =/= U(0), (temp >> U(3)) + U(1), temp >> U(3)).resize(32)
          }

          slotCtx(i).plabForceOld := False

          val regionAttrType = slotCtx(i).srcRegionAttr(15 downto 8)
          when(regionAttrType === 0){
            slotCtx(i).age := ((slotCtx(i).markWord >> U(3)) & U(x"1111", GCElementWidth bits)).resize(32)
            slotState(i) := overall_state.states(3)
          }.otherwise{
            checkDestRegionAttrValid(i)
          }
        }

        is(overall_state.states(3)){
          when(slotCtx(i).age < io.ConfigIO.AgeThreshold){
            slotCtx(i).destAttrPtr := slotCtx(i).regionAttrPtr
            slotCtx(i).destRegionAttr := slotCtx(i).srcRegionAttr
            slotCtx(i).plabTargetIdx := slotCtx(i).srcRegionAttr(15 downto 8).resize(1)
            slotState(i) := overall_state.states(5)
          }.otherwise{
            checkDestRegionAttrValid(i)
          }
        }

        // access dest attr
        is(overall_state.states(4)){

        }

        is(overall_state.states(5)){
          val plabIdx = slotCtx(i).plabTargetIdx
          when(plabCacheValid(plabIdx)) {
            slotState(i) := overall_state.states(8)
          }.elsewhen(plabRefillBusy(plabIdx)){
            // wait
          }.elsewhen(plabCacheBufferValid(plabIdx)){
            plabRefillBusy(plabIdx) := True
            plabRefillOwner(plabIdx) := U(i, 1 bits)
            slotState(i) := overall_state.states(7)
          }.otherwise{
            plabRefillBusy(plabIdx) := True
            plabRefillOwner(plabIdx) := U(i, 1 bits)
          }
        }

        // access plab buffer
        is(overall_state.states(6)){

        }

        // access plab top and end
        is(overall_state.states(7)){

        }

        // get the dest obj or write regionTop and send the allocate
        is(overall_state.states(8)){
          allocateCacheWithIdx(i)
        }

        // send trace/copy and write src oop
        is(overall_state.states(9)){
          val needTrace = slotCtx(i).kid =/= U(TypeArrayKlassID, 32 bits)
          val copyIssuedDone = slotCtx(i).copyIssued
          val traceIssuedDone = slotCtx(i).traceIssued || !needTrace
          when(copyIssuedDone && traceIssuedDone && slotCtx(i).writeSrcOopPtrDone){
            slotState(i) := overall_state.states(10)
          }
        }

        // write dest oop
        is(overall_state.states(10)){

        }

        is(overall_state.states(11)){
          val needTrace = slotCtx(i).kid =/= U(TypeArrayKlassID, 32 bits)
          //val copyFinished = io.ToCopy.Done && copyOwner === i || slotCtx(i).copyDone || !needTrace
          //val traceFinished = io.ToTrace.Done && traceOwner === i || slotCtx(i).traceDone || !needTrace
          val copyFinished = slotCtx(i).copyDone || !needTrace
          val traceFinished = slotCtx(i).traceDone || !needTrace

          when(copyFinished && traceFinished){
            finishSlot(i)
            io.ToCopySurvivor.Done := True
            io.ToCopySurvivor.DoneOwner := slotCtx(i).owner
            io.ToCopySurvivor.DestOopPtr := slotCtx(i).destOopPtr
          }
        }

        is(overall_state.states(12)){
          when(slotCtx(i).allocDone){
            slotCtx(i).allocDone := False
            when(slotCtx(i).destOopPtr === 0){
              slotCtx(i).plabTargetIdx := 1
              slotCtx(i).plabForceOld := True
              slotState(i) := overall_state.states(5)
            }.otherwise{
              slotState(i) := Mux(slotCtx(i).allocSelect, overall_state.states(13), overall_state.states(9))
            }
          }
        }
      }
    }
  }

  // MMU request generation conditions
  val needReadKlassMeta = Vec(Bool(), 2)
  val needReadDestAttr  = Vec(Bool(), 2)
  val needReadPlabBufPtr = Vec(Bool(), 2)
  val needReadPlabBuf = Vec(Bool(), 2)
  val needReadPlabTopEnd = Vec(Bool(), 2)
  val needWriteSrcFwd = Vec(Bool(), 2)
  val needWriteDestMark = Vec(Bool(), 2)
  val needWriteRegionTop = Vec(Bool(), 2)
  val needWriteForceOld = Vec(Bool(), 2)

  for(i <- 0 until 2){
    needReadKlassMeta(i) := slotValid(i) && slotState(i) === overall_state.states(1) && !klassCacheHitOf(i)
    needReadDestAttr(i) := slotValid(i) && slotState(i) === overall_state.states(4)
    needReadPlabBufPtr(i) := slotValid(i) && (slotState(i) === overall_state.states(5)) && !plabCacheBufferValid(slotCtx(i).plabTargetIdx) && plabRefillBusy(slotCtx(i).plabTargetIdx) === True && plabRefillOwner(slotCtx(i).plabTargetIdx) === i
    needReadPlabBuf(i) := slotValid(i) && slotState(i) === overall_state.states(6)
    needReadPlabTopEnd(i) := slotValid(i) && slotState(i) === overall_state.states(7)
    needWriteDestMark(i) := slotValid(i) && (slotState(i) === overall_state.states(10))
    needWriteForceOld(i) := slotValid(i) && (slotState(i) === overall_state.states(13))

    val idx = slotCtx(i).plabTargetIdx
    val cond = (plabCacheEnd(idx) - plabCacheTop(idx)) / U(8) < slotCtx(i).size
    needWriteRegionTop(i) := slotValid(i) && slotState(i) === overall_state.states(8) && cond && !slotCtx(i).waitWriteRegionTop
    needWriteSrcFwd(i) := slotValid(i) && (slotState(i) === overall_state.states(9)) && !slotCtx(i).writeSrcOopPtrDone
  }

  // --------------------------------------------------------------------------
  // MMU arbitration (fixed priority: slot0 > slot1)
  // --------------------------------------------------------------------------
  when(!mmuBusy) {
    when(needReadKlassMeta(0)) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.READ_KLASS
    } elsewhen needReadDestAttr(0) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.READ_DEST_ATTR
    } elsewhen needReadPlabBufPtr(0) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.READ_PLAB_PTR
    } elsewhen needReadPlabBuf(0) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.READ_PLAB_BUF
    } elsewhen needReadPlabTopEnd(0) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.READ_PLAB_TOPEND
    } elsewhen needWriteRegionTop(0) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.WRITE_REGION_TOP
    } elsewhen needWriteSrcFwd(0) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.WRITE_SRC_FWD
    } elsewhen needWriteDestMark(0) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.WRITE_MARK
    } elsewhen needWriteForceOld(0) {
      mmuBusy := True
      mmuOwner := U(0)
      mmuOp := MmuOp.WRITE_FORCE_OLD
    } elsewhen needReadKlassMeta(1) {
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.READ_KLASS
    } elsewhen needReadDestAttr(1) {
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.READ_DEST_ATTR
    } elsewhen needReadPlabBufPtr(1) {
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.READ_PLAB_PTR
    } elsewhen needReadPlabBuf(1) {
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.READ_PLAB_BUF
    } elsewhen needReadPlabTopEnd(1) {
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.READ_PLAB_TOPEND
    } elsewhen needWriteRegionTop(1){
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.WRITE_REGION_TOP
    }elsewhen needWriteSrcFwd(1) {
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.WRITE_SRC_FWD
    } elsewhen needWriteDestMark(1) {
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.WRITE_MARK
    } elsewhen needWriteForceOld(1) {
      mmuBusy := True
      mmuOwner := U(1)
      mmuOp := MmuOp.WRITE_FORCE_OLD
    }
  }

  when(mmuBusy){
    switch(mmuOp){
      is(MmuOp.READ_KLASS){
        val compressedKlassPtr = (io.ConfigIO.CompressedKlassPointerBase + slotCtx(mmuOwner).klassPtr(31 downto 0) << io.ConfigIO.CompressedKlassPointerShift).resize(GCElementWidth)
        val newKlassPtr = Mux(io.ConfigIO.UseCompressedKlassPointer, compressedKlassPtr, slotCtx(mmuOwner).klassPtr)
        val addr = newKlassPtr + U(8)
        issueReq(io.Mreq, addr, False, U(8), U(0), mmuIssued(mmuOwner)) { rd =>
          slotCtx(mmuOwner).klassPtr := newKlassPtr
          slotCtx(mmuOwner).lh := rd(31 downto 0)
          slotCtx(mmuOwner).kid := rd(63 downto 32)

          klassCacheValid(klassCacheReplacePtr) := True
          klassCachePtr(klassCacheReplacePtr) := newKlassPtr
          klassCacheKidLh(klassCacheReplacePtr) := rd(63 downto 0)
          klassCacheReplacePtr := klassCacheReplacePtr + 1

          slotState(mmuOwner) := overall_state.states(2)

          io.ToCopySurvivor.isTypeArray := rd(63 downto 32) === TypeArrayKlassID
          io.ToCopySurvivor.DoneOwner := slotCtx(mmuOwner).owner

          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.READ_DEST_ATTR){
        val destAttrBase = (io.ConfigIO.ParScanThreadStatePtr + U"x178").resize(GCElementWidth)
        issueReq(io.Mreq, destAttrBase, False, U(2), U(0), mmuIssued(mmuOwner)) { rd =>
          destAttrRegionValid := True
          destAttrRegionCache := rd(31 downto 0)
          slotCtx(mmuOwner).destRegionAttr := Mux(slotCtx(mmuOwner).destAttrPtr === destAttrBase + U(2), rd(31 downto 16), rd(15 downto 0))
          slotCtx(mmuOwner).plabTargetIdx := Mux(slotCtx(mmuOwner).destAttrPtr === destAttrBase + U(2), rd(31 downto 24), rd(15 downto 8)).resize(1)
          slotState(mmuOwner) := overall_state.states(5)
          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.READ_PLAB_PTR) {
        val addr = (io.ConfigIO.PlabAllocatorPtr + U"x10" + slotCtx(mmuOwner).plabTargetIdx * U(8)).resize(MMUAddrWidth)
        issueReq(io.Mreq, addr, False, U(8), U(0), mmuIssued(mmuOwner)) { rd =>
          plabCacheBufferPtr(slotCtx(mmuOwner).plabTargetIdx) := rd(GCElementWidth - 1 downto 0)
          slotState(mmuOwner) := overall_state.states(6)
          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.READ_PLAB_BUF) {
        issueReq(io.Mreq, plabCacheBufferPtr(slotCtx(mmuOwner).plabTargetIdx), False, U(8), U(0), mmuIssued(mmuOwner)) { rd =>
          plabCacheBuffer(slotCtx(mmuOwner).plabTargetIdx) := rd(GCElementWidth - 1 downto 0)
          plabCacheBufferValid(slotCtx(mmuOwner).plabTargetIdx) := True
          slotState(mmuOwner) := overall_state.states(7)
          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.READ_PLAB_TOPEND) {
        val addr = plabCacheBuffer(slotCtx(mmuOwner).plabTargetIdx) + U"x30"
        issueReq(io.Mreq, addr, False, U(16), U(0), mmuIssued(mmuOwner)) { rd =>
          plabCacheTop(slotCtx(mmuOwner).plabTargetIdx) := rd(GCElementWidth - 1 downto 0)
          plabCacheEnd(slotCtx(mmuOwner).plabTargetIdx) := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          plabCacheValid(slotCtx(mmuOwner).plabTargetIdx) := True
          plabRefillBusy(slotCtx(mmuOwner).plabTargetIdx) := False

          when(slotCtx(mmuOwner).plabForceOld){
            slotCtx(mmuOwner).destRegionAttr := (slotCtx(mmuOwner).destRegionAttr & U(x"00ff", 16 bits)) | U(x"0100", 16 bits)
          }

          slotState(mmuOwner) := overall_state.states(8)
          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.WRITE_REGION_TOP){
        val addr = plabCacheBuffer(slotCtx(mmuOwner).plabTargetIdx) + U"x30"
        issueReq(io.Mreq, addr, True, U(8), plabCacheTop(slotCtx(mmuOwner).plabTargetIdx), mmuIssued(mmuOwner)) { _ =>}
        when(mmuIssued(mmuOwner)){
          mmuOp := MmuOp.NONE
          mmuBusy := False
          mmuIssued(mmuOwner) := False
          slotCtx(mmuOwner).waitWriteRegionTop := True
        }
      }

      is(MmuOp.WRITE_SRC_FWD){
        val addr = slotCtx(mmuOwner).srcOopPtr
        val writeValue = Cat(slotCtx(mmuOwner).destOopPtr(GCElementWidth - 1 downto 2), U(3, 2 bits)).resize(GCElementWidth).asUInt
        issueReq(io.Mreq, addr, True, U(8), writeValue, mmuIssued(mmuOwner)) { _ =>}
        when(mmuIssued(mmuOwner)){
          mmuOp := MmuOp.NONE
          mmuBusy := False
          mmuIssued(mmuOwner) := False
          slotCtx(mmuOwner).writeSrcOopPtrDone := True

          io.ToFetch.valid := True
          io.ToFetch.srcOopPtr := slotCtx(mmuOwner).srcOopPtr
          io.ToFetch.writeValue := writeValue
        }
      }

      is(MmuOp.WRITE_MARK){
        val cond = slotCtx(mmuOwner).destRegionAttr(15 downto 8) === U(0)
        val temp = (slotCtx(mmuOwner).markWord & ~(U"x1111" << 3).resize(GCElementWidth)) | ((Mux(slotCtx(mmuOwner).age + U(1) < U(15), slotCtx(mmuOwner).age + U(1), slotCtx(mmuOwner).age) & U(x"1111", 32 bits)) << U(3)).resize(GCElementWidth)
        val addr = slotCtx(mmuOwner).destOopPtr
        val writeValue = Mux(cond, temp, slotCtx(mmuOwner).markWord)
        issueReq(io.Mreq, addr, True, U(8), writeValue, mmuIssued(mmuOwner)) { _ =>}
        when(mmuIssued(mmuOwner)){
          mmuOp := MmuOp.NONE
          mmuBusy := False
          mmuIssued(mmuOwner) := False
          slotState(mmuOwner) := overall_state.states(11)
        }
      }

      is(MmuOp.WRITE_FORCE_OLD){
        val addr = slotCtx(mmuOwner).destAttrPtr + U(1)
        issueReq(io.Mreq, addr, True, U(1), U(1), mmuIssued(mmuOwner)) { _ => }
        when(mmuIssued(mmuOwner)) {
          mmuOp := MmuOp.NONE
          mmuBusy := False
          mmuIssued(mmuOwner) := False
          slotState(mmuOwner) := overall_state.states(9)
        }
      }
    }
  }

  // copy arbitration
  val wantCopy = Vec(Bool(), 2)
  for(i <- 0 until 2) {
    wantCopy(i) := slotValid(i) && slotState(i) === overall_state.states(9) && !slotCtx(i).copyIssued
  }

  when(!copyBusy) {
    when(wantCopy(0)) {
      val totalSize = (slotCtx(0).size * U(8)).resize(slotCtx(0).size.getWidth)
      val compressedSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(16), U(20))

      io.ToCopy.Valid := True
      io.ToCopy.Size := Mux(slotCtx(0).kid === U(ObjectArrayKlassID, 32 bits), totalSize - compressedSize, totalSize - U(8))
      io.ToCopy.SrcOopPtr := Mux(slotCtx(0).kid === U(ObjectArrayKlassID, 32 bits), slotCtx(0).srcOopPtr + compressedSize, slotCtx(0).srcOopPtr + U(8))
      io.ToCopy.DestOopPtr := Mux(slotCtx(0).kid === U(ObjectArrayKlassID, 32 bits), slotCtx(0).destOopPtr + compressedSize, slotCtx(0).destOopPtr + U(8))

      when(io.ToCopy.Ready) {
        slotCtx(0).copyIssued := True
        copyBusy := True
        copyOwner := U(0)
      }
    } elsewhen(wantCopy(1)) {
      val totalSize = (slotCtx(1).size * U(8)).resize(slotCtx(1).size.getWidth)
      val compressedSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(16), U(20))

      io.ToCopy.Valid := True
      io.ToCopy.Size := Mux(slotCtx(1).kid === U(ObjectArrayKlassID, 32 bits), totalSize - compressedSize, totalSize - U(8))
      io.ToCopy.SrcOopPtr := Mux(slotCtx(1).kid === U(ObjectArrayKlassID, 32 bits), slotCtx(1).srcOopPtr + compressedSize, slotCtx(1).srcOopPtr + U(8))
      io.ToCopy.DestOopPtr := Mux(slotCtx(1).kid === U(ObjectArrayKlassID, 32 bits), slotCtx(1).destOopPtr + compressedSize, slotCtx(1).destOopPtr + U(8))

      when(io.ToCopy.Ready) {
        slotCtx(1).copyIssued := True
        copyBusy := True
        copyOwner := U(1)
      }
    }
  }

  // trace arbitration
  val wantTrace = Vec(Bool(), 2)
  for(i <- 0 until 2) {
    wantTrace(i) := slotValid(i) && slotState(i) === overall_state.states(9) && !slotCtx(i).traceIssued && slotCtx(i).kid =/= U(TypeArrayKlassID, 32 bits)
  }

  when(!traceBusy) {
    when(wantTrace(0)) {
      io.ToTrace.Valid := True
      io.ToTrace.OopType := U(NotArrayOop)
      io.ToTrace.KlassPtr := slotCtx(0).klassPtr
      io.ToTrace.SrcOopPtr := slotCtx(0).srcOopPtr
      io.ToTrace.DestOopPtr := slotCtx(0).destOopPtr
      io.ToTrace.Kid := slotCtx(0).kid
      io.ToTrace.ScanningInYoung := slotCtx(0).destRegionAttr(15 downto 8) === U(0, 8 bits)
      io.ToTrace.ArrayLength := slotCtx(0).srcLength
      io.ToTrace.PartialArrayStart := U(0)
      io.ToTrace.StepIndex := (slotCtx(0).srcLength % io.ConfigIO.ChunkSize).resize(32)
      io.ToTrace.StepNCreate := Mux(slotCtx(0).srcLength > (slotCtx(0).srcLength % io.ConfigIO.ChunkSize), U(1), U(0)).resize(32)

      when(io.ToTrace.Ready) {
        slotCtx(0).traceIssued := True
        traceBusy := True
        traceOwner := U(0)
      }
    } elsewhen(wantTrace(1)) {
      io.ToTrace.Valid := True
      io.ToTrace.OopType := U(NotArrayOop)
      io.ToTrace.KlassPtr := slotCtx(1).klassPtr
      io.ToTrace.SrcOopPtr := slotCtx(1).srcOopPtr
      io.ToTrace.DestOopPtr := slotCtx(1).destOopPtr
      io.ToTrace.Kid := slotCtx(1).kid
      io.ToTrace.ScanningInYoung := slotCtx(1).destRegionAttr(15 downto 8) === U(0, 8 bits)
      io.ToTrace.ArrayLength := slotCtx(1).srcLength
      io.ToTrace.PartialArrayStart := U(0)
      io.ToTrace.StepIndex := (slotCtx(1).srcLength % io.ConfigIO.ChunkSize).resize(32)
      io.ToTrace.StepNCreate := Mux(slotCtx(1).srcLength > (slotCtx(1).srcLength % io.ConfigIO.ChunkSize), U(1), U(0)).resize(32)

      when(io.ToTrace.Ready) {
        slotCtx(1).traceIssued := True
        traceBusy := True
        traceOwner := U(1)
      }
    }
  }

  // allocate arbitration
  val wantAlloc = Vec(Bool(), 2)
  for(i <- 0 until 2) {
    val idx = slotCtx(i).plabTargetIdx
    val cond = (plabCacheEnd(idx) - plabCacheTop(idx)) / U(8) < slotCtx(i).size
    wantAlloc(i) := slotValid(i) && slotState(i) === overall_state.states(8) && !slotCtx(i).allocIssued && cond
  }

  when(!allocBusy) {
    when(wantAlloc(0)) {
      io.ToAllocate.Valid := True
      io.ToAllocate.Size := slotCtx(0).size
      io.ToAllocate.DestAttrType := Mux(slotCtx(0).allocSelect, U(1, 8 bits), slotCtx(0).destRegionAttr(15 downto 8))
      when(io.ToAllocate.Ready) {
        slotCtx(0).allocIssued := True
        allocBusy := True
        allocOwner := U(0)
      }
    } elsewhen(wantAlloc(1)) {
      io.ToAllocate.Valid := True
      io.ToAllocate.Size := slotCtx(1).size
      io.ToAllocate.DestAttrType := Mux(slotCtx(1).allocSelect, U(1, 8 bits), slotCtx(1).destRegionAttr(15 downto 8))
      when(io.ToAllocate.Ready) {
        slotCtx(1).allocIssued := True
        allocBusy := True
        allocOwner := U(1)
      }
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}