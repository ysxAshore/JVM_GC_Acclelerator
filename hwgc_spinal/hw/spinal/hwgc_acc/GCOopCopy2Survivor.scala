package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class CopySurvivorSlotCtx() extends Bundle with GCTopParameters {
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

  val monitor_mw = UInt(GCElementWidth bits)

  val writeDestOopPtrDone = Bool()

  val copyIssued = Bool()
  val traceIssued = Bool()
  val copyDone = Bool()
  val traceDone = Bool()

  val allocIssued = Bool()
  val allocDone = Bool()
  val plab_refill_failed = Bool()

  val forwardPtr = UInt(GCElementWidth bits)

  // Added for cross-slot PLAB safety.
  val afterAllocCache = Bool()
  val usingPlabCacheBuffer = Bool()
  val heldPlabCacheBuffer = UInt(GCElementWidth bits)
  val forwardDecided = Bool()
}

class GCOopCopy2Survivor extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val Mreq0 = master(new LocalMMUIO)
    val Mreq1 = master(new LocalMMUIO)

    val ToCopy = master(new GCToCopy)
    val ToFetch = master(new GCWriteSrcOopPtr)
    val ToTrace = master(new GCToTrace)
    val ToAllocate = master(new GCToAllocate)

    val ToCopySurvivor = slave(new GCToSurvivor)
    val TaskDone = in Bool()
    val ConfigIO = slave(new GCCopy2SurvivorConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // defaults
  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid := False
    m.Request.payload.clearAll()

    m.Response.ready := True
  }
  clearMreq(io.Mreq0)
  clearMreq(io.Mreq1)

  io.ToCopy.clearIn()
  io.ToFetch.clearIn()
  io.ToTrace.clearIn()
  io.ToAllocate.clearIn()

  io.ToCopySurvivor.Done := False
  io.ToCopySurvivor.DoneOwner := U(0)
  io.ToCopySurvivor.DestOopPtr := U(0)
  io.ToCopySurvivor.isTypeArray := False

  def slotMreq(i: Int): LocalMMUIO = if (i == 0) io.Mreq0 else io.Mreq1

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCOopCopy2Survivor<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  // slot context
  val slotValid = Vec.fill(2)(RegInit(False))
  val slotCtx = Vec.fill(2)(Reg(CopySurvivorSlotCtx()) init CopySurvivorSlotCtx().getZero)

  val mmuIssued = Vec.fill(2)(RegInit(False))

  val slotStart = Vec.fill(2)(Bool())
  val slotGotoIdle = Vec.fill(2)(Bool())
  val slotPlabRefillGrant = Vec.fill(2)(Bool())

  val slotCopyAccept = Vec.fill(2)(Bool())
  val slotTraceAccept = Vec.fill(2)(Bool())
  val slotAllocAccept = Vec.fill(2)(Bool())

  val slotAllocCacheHazard = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    slotStart(i) := False
    slotGotoIdle(i) := False
    slotPlabRefillGrant(i) := False

    slotCopyAccept(i) := False
    slotTraceAccept(i) := False
    slotAllocAccept(i) := False
  }

  // shared caches
  val destAttrRegionValid = RegInit(False)
  val destAttrRegionCache = RegInit(U(0, 32 bits))

  val plabCacheBuffer = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheBufferPtr = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheBufferValid = Vec.fill(2)(RegInit(False))
  val plabCacheBottom = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheTop = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheEnd = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheHardEnd = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val plabCacheValid = Vec.fill(2)(RegInit(False))
  val plabRefillBusy = Vec.fill(2)(RegInit(False))
  val plabRefillOwner = Vec.fill(2)(RegInit(U(0, 1 bits)))

  val KlassCacheEntries = 16
  val klassCacheValid = Vec.fill(KlassCacheEntries)(RegInit(False))
  val klassCachePtr = Vec.fill(KlassCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val klassCacheKidLh = Vec.fill(KlassCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val klassCacheReplacePtr = RegInit(U(0, log2Up(KlassCacheEntries) bits))

  // shared resource owners
  val copyBusy = RegInit(False)
  val copyOwner = RegInit(U(0, 1 bits))

  val traceBusy = RegInit(False)
  val traceOwner = RegInit(U(0, 1 bits))

  val allocBusy = RegInit(False)
  val allocOwner = RegInit(U(0, 1 bits))

  // pending single-output pulses
  val typeArrayPending = Vec.fill(2)(RegInit(False))
  val typeArrayOwner = Vec.fill(2)(RegInit(U(0, 1 bits)))

  val survivorDonePending = Vec.fill(2)(RegInit(False))
  val survivorDoneOwner = Vec.fill(2)(RegInit(U(0, 1 bits)))
  val survivorDoneDest = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))

  val toFetchPending = Vec.fill(2)(RegInit(False))
  val toFetchSrcOopPtr = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))
  val toFetchWriteValue = Vec.fill(2)(RegInit(U(0, GCElementWidth bits)))

  // helpers
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
    slotCtx(i).writeDestOopPtrDone := False
    slotCtx(i).copyIssued := False
    slotCtx(i).traceIssued := False
    slotCtx(i).copyDone := False
    slotCtx(i).traceDone := False
    slotCtx(i).allocIssued := False
    slotCtx(i).allocDone := False
    slotCtx(i).plab_refill_failed := False
    slotCtx(i).forwardPtr := 0
    slotCtx(i).monitor_mw := 0

    slotCtx(i).afterAllocCache := False
    slotCtx(i).usingPlabCacheBuffer := False
    slotCtx(i).heldPlabCacheBuffer := 0
    slotCtx(i).forwardDecided := False

    mmuIssued(i) := False
  }

  def allocSlot(i: Int): Unit = {
    slotValid(i) := True

    slotCtx(i).owner := io.ToCopySurvivor.Owner
    slotCtx(i).srcOopPtr := io.ToCopySurvivor.SrcOopPtr
    slotCtx(i).markWord := io.ToCopySurvivor.MarkWord
    slotCtx(i).klassPtr := io.ToCopySurvivor.KlassPtr
    slotCtx(i).srcLength := io.ToCopySurvivor.SrcLength
    slotCtx(i).srcRegionAttr := io.ToCopySurvivor.SrcRegionAttr
    slotCtx(i).regionAttrPtr := io.ToCopySurvivor.RegionAttrPtr

    clearSlotRuntime(i)
    slotStart(i) := True

    dbg(Seq("Allocate task to slot", i.toString, ", src=", io.ToCopySurvivor.SrcOopPtr))
  }

  def finishSlot(i: Int): Unit = {
    slotValid(i) := False
    clearSlotRuntime(i)
    slotGotoIdle(i) := True

    dbg(Seq("Finish slot", i.toString))
  }

  def calcSize(lh: UInt, kid: UInt, srcLength: UInt): UInt = {
    val size = UInt(32 bits)
    val temp = ((srcLength << lh(7 downto 0)) + lh(23 downto 16)).resize(32)

    when(lh.asSInt < S(0)) {
      size := Mux(temp(2 downto 0) =/= U(0), (temp >> U(3)) + U(1), temp >> U(3)).resize(32)
    }.elsewhen(lh.asSInt > S(0) && !lh(0)) {
      size := (lh >> U(3)).resize(32)
    } otherwise {
      size := (lh >> U(3)).resize(32)
    }

    size
  }

  def compressedKlassPtrOf(i: Int): UInt =
    (io.ConfigIO.CompressedKlassPointerBase + (slotCtx(i).klassPtr(31 downto 0) << io.ConfigIO.CompressedKlassPointerShift)).resize(GCElementWidth)

  def lookupKlassPtrOf(i: Int): UInt = Mux(io.ConfigIO.UseCompressedKlassPointer, compressedKlassPtrOf(i), slotCtx(i).klassPtr)

  def selectDestAttrOfRegionType(regionType: UInt): UInt =
    Mux(regionType === U(1), destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0)).resize(16)

  def gotoPlabSelectHelper(i: Int, markWord: UInt): Unit = {
    val new_age = (markWord >> 3).resize(4).resize(32)
    slotCtx(i).age := new_age
    when(new_age < io.ConfigIO.AgeThreshold) {
      slotCtx(i).destRegionAttr := slotCtx(i).srcRegionAttr
      slotCtx(i).destAttrPtr := slotCtx(i).regionAttrPtr
      slotCtx(i).plabTargetIdx := slotCtx(i).srcRegionAttr(15 downto 8).resize(1)
    }
  }

  // klass cache lookup
  val klassLookupPtr = Vec.fill(2)(UInt(GCElementWidth bits))
  val klassCacheHit = Vec.fill(2)(Bool())
  val klassCacheHitIndex = Vec.fill(2)(UInt(log2Up(KlassCacheEntries) bits))

  for (i <- 0 until 2) {
    klassLookupPtr(i) := lookupKlassPtrOf(i)

    val hitVec = Vec.fill(KlassCacheEntries)(Bool())
    for (j <- 0 until KlassCacheEntries) {
      hitVec(j) := klassCacheValid(j) && klassCachePtr(j) === klassLookupPtr(i)
    }

    klassCacheHit(i) := hitVec.orR
    klassCacheHitIndex(i) := OHToUInt(hitVec.asBits)
  }

  // shared fill requests
  val klassFillValid = Vec.fill(2)(Bool())
  val klassFillPtr = Vec.fill(2)(UInt(GCElementWidth bits))
  val klassFillKidLh = Vec.fill(2)(UInt(GCElementWidth bits))

  val destAttrFillValid = Vec.fill(2)(Bool())
  val destAttrFillData = Vec.fill(2)(UInt(32 bits))

  val plabPtrFillValid = Vec.fill(2)(Bool())
  val plabPtrFillIdx = Vec.fill(2)(UInt(1 bits))
  val plabPtrFillData = Vec.fill(2)(UInt(GCElementWidth bits))

  val plabBufFillValid = Vec.fill(2)(Bool())
  val plabBufFillIdx = Vec.fill(2)(UInt(1 bits))
  val plabBufFillData = Vec.fill(2)(UInt(GCElementWidth bits))

  val plabTopEndFillValid = Vec.fill(2)(Bool())
  val plabTopEndFillIdx = Vec.fill(2)(UInt(1 bits))
  val plabBottomFillData = Vec.fill(2)(UInt(GCElementWidth bits))
  val plabTopFillData = Vec.fill(2)(UInt(GCElementWidth bits))
  val plabEndFillData = Vec.fill(2)(UInt(GCElementWidth bits))
  val plabHardEndFillData = Vec.fill(2)(UInt(GCElementWidth bits))

  for (i <- 0 until 2) {
    klassFillValid(i) := False
    klassFillPtr(i) := U(0, GCElementWidth bits)
    klassFillKidLh(i) := U(0, GCElementWidth bits)

    destAttrFillValid(i) := False
    destAttrFillData(i) := U(0, 32 bits)

    plabPtrFillValid(i) := False
    plabPtrFillIdx(i) := U(0, 1 bits)
    plabPtrFillData(i) := U(0, GCElementWidth bits)

    plabBufFillValid(i) := False
    plabBufFillIdx(i) := U(0, 1 bits)
    plabBufFillData(i) := U(0, GCElementWidth bits)

    plabTopEndFillValid(i) := False
    plabTopEndFillIdx(i) := U(0, 1 bits)
    plabTopFillData(i) := U(0, GCElementWidth bits)
    plabEndFillData(i) := U(0, GCElementWidth bits)
    plabBottomFillData(i) := U(0, GCElementWidth bits)
    plabHardEndFillData(i) := U(0, GCElementWidth bits)
  }

  // task done invalidates shared caches
  when(io.TaskDone) {
    destAttrRegionValid := False
    plabCacheValid(0) := False
    plabCacheValid(1) := False
    plabCacheBufferValid(0) := False
    plabCacheBufferValid(1) := False
  }

  // downstream done capture
  when(copyBusy && io.ToCopy.Done) {
    copyBusy := False
    slotCtx(copyOwner).copyDone := True
    dbg(Seq("copy done for slot ", copyOwner))
  }

  when(traceBusy && io.ToTrace.Done) {
    traceBusy := False
    slotCtx(traceOwner).traceDone := True
    dbg(Seq("trace done for slot ", traceOwner))
  }

  when(allocBusy && io.ToAllocate.Done) {
    allocBusy := False
    slotCtx(allocOwner).allocDone := True
    slotCtx(allocOwner).destOopPtr := io.ToAllocate.DestObjPtr
    slotCtx(allocOwner).plab_refill_failed := io.ToAllocate.PlabRefillFailed

    dbg(Seq("allocate done for slot ", allocOwner))
  }

  // input admission
  val hasFreeSlot = !slotValid(0) || !slotValid(1)
  io.ToCopySurvivor.Ready := hasFreeSlot

  when(io.ToCopySurvivor.Valid && io.ToCopySurvivor.Ready) {
    when(!slotValid(0)) {
      allocSlot(0)
    } otherwise {
      allocSlot(1)
    }
  }

  // slot state visibility
  val slotIsPlabSelect = Vec.fill(2)(Bool())
  val slotIsAllocCache = Vec.fill(2)(Bool())
  val slotIsSendWork = Vec.fill(2)(Bool())
  val slotPlabAllocGrant = Vec.fill(2)(Bool())

  val slotStateDebug = Vec.fill(2)(UInt(5 bits))

  for (i <- 0 until 2) {
    val m = slotMreq(i)

    val slotFsm = new StateMachine {
      val IDLE = new State with EntryPoint
      val READ_KLASS = new State
      val SIZE_DEICIDE = new State
      val DEST_ATTR_DECIDE = new State
      val AGE_DECIDE = new State
      val PLAB_SELECT = new State
      val READ_PLAB_PTR = new State
      val READ_PLAB_BUF = new State
      val READ_PLAB_TOPEND = new State
      val ALLOC_CACHE = new State
      val WAIT_ALLOC = new State
      val WRITE_FORCE_OLD = new State
      val DECIDE_FORWARD_PTR = new State
      val SEND_WORK = new State
      val GET_MONITOR_MW = new State
      val WRITE_MONITOR_MW = new State
      val WAIT_COPY_TRACE = new State
      val WRITE_FORWARDPTR_NOT_ZERO = new State

      def doPlabCacheAllocDirect(): Unit = {
        val idx = slotCtx(i).plabTargetIdx
        val oldTop = plabCacheTop(idx)
        val newTop = (oldTop + slotCtx(i).size * U(8)).resize(GCElementWidth)
        val otherIdx = idx ^ U(1, 1 bits)

        slotCtx(i).destOopPtr := oldTop

        slotCtx(i).afterAllocCache := True
        slotCtx(i).usingPlabCacheBuffer := True
        slotCtx(i).heldPlabCacheBuffer := plabCacheBuffer(idx)
        slotCtx(i).forwardDecided := False
        slotCtx(i).forwardPtr := 0

        when(plabCacheValid(otherIdx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
          plabCacheTop(otherIdx) := newTop
        }

        issueReq(m, (plabCacheBuffer(idx) + U"x30").resize(MMUAddrWidth), True, U(8), newTop, False, False, mmuIssued(i)) { _ => }

        when(mmuIssued(i)) {
          plabCacheTop(idx) := newTop
          mmuIssued(i) := False
          goto(DECIDE_FORWARD_PTR)
        }
      }


      IDLE.whenIsActive {
      }

      READ_KLASS.whenIsActive {
        when(klassCacheHit(i)){
          val idx = klassCacheHitIndex(i)
          val lhVal = klassCacheKidLh(idx)(31 downto 0)
          val kidVal = klassCacheKidLh(idx)(63 downto 32)

          slotCtx(i).klassPtr := klassLookupPtr(i)
          slotCtx(i).lh := lhVal
          slotCtx(i).kid := kidVal

          when(kidVal === U(InstanceMirrorKlassID, 32 bits) && (lhVal.asSInt === S(0) || (lhVal.asSInt > S(0) && lhVal(0)))) {
            goto(SIZE_DEICIDE) // mirror 慢路径仍然读 src+0x20
          } otherwise {
            slotCtx(i).size := calcSize(lhVal, kidVal, slotCtx(i).srcLength)
            goto(DEST_ATTR_DECIDE)
          }
        }.otherwise{
          val newKlassPtr = klassLookupPtr(i)
          val addr = (newKlassPtr + U(8)).resize(MMUAddrWidth)

          issueReq(m, addr, False, U(8), U(0), True, False, mmuIssued(i)) { rd =>
            val lhVal = rd(31 downto 0)
            val kidVal = rd(63 downto 32)

            slotCtx(i).klassPtr := newKlassPtr
            slotCtx(i).lh := lhVal
            slotCtx(i).kid := kidVal

            klassFillValid(i) := True
            klassFillPtr(i) := newKlassPtr
            klassFillKidLh(i) := rd(63 downto 0)

            when(kidVal === U(InstanceMirrorKlassID, 32 bits) && (lhVal.asSInt === S(0) || (lhVal.asSInt > S(0) && lhVal(0)))) {
              goto(SIZE_DEICIDE) // mirror 慢路径仍然读 src+0x20
            } otherwise {
              slotCtx(i).size := calcSize(lhVal, kidVal, slotCtx(i).srcLength)
              goto(DEST_ATTR_DECIDE)
            }
          }

        }
      }

      SIZE_DEICIDE.whenIsActive {
        when(slotCtx(i).lh.asSInt < S(0)) {
          val temp = ((slotCtx(i).srcLength << slotCtx(i).lh(7 downto 0)) + slotCtx(i).lh(23 downto 16)).resize(32)
          slotCtx(i).size := Mux(temp(2 downto 0) =/= U(0), (temp >> U(3)) + U(1), temp >> U(3)).resize(32)
        }.elsewhen(slotCtx(i).lh.asSInt > S(0) && !slotCtx(i).lh(0)) {
          slotCtx(i).size := (slotCtx(i).lh >> U(3)).resize(32)
        } otherwise {
          when(slotCtx(i).kid =/= U(InstanceMirrorKlassID, 32 bits)) {
            slotCtx(i).size := (slotCtx(i).lh >> U(3)).resize(32)
          }.otherwise {
            issueReq(m, slotCtx(i).srcOopPtr + U"x20", False, U(8), U(0), True, False, mmuIssued(i)) { rd =>
              slotCtx(i).size := rd(31 downto 0)
              goto(DEST_ATTR_DECIDE)
            }
          }
        }

        when(slotCtx(i).lh.asSInt === S(0) || (slotCtx(i).lh.asSInt > S(0) && slotCtx(i).lh(0))) {
        } otherwise {
          goto(DEST_ATTR_DECIDE)
        }
      }

      DEST_ATTR_DECIDE.whenIsActive {
        slotCtx(i).plabForceOld := False
        when(slotCtx(i).kid === U(TypeArrayKlassID, 32 bits)) {
          typeArrayPending(i) := True
          typeArrayOwner(i) := slotCtx(i).owner
        }

        val srcType = slotCtx(i).srcRegionAttr(15 downto 8)
        val highSelected = srcType === U(1)
        val destAttrBase = (io.ConfigIO.ParScanThreadStatePtr + U"x178").resize(MMUAddrWidth)

        val cachedDestAttr = Mux(highSelected, destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0)).resize(16)
        val cachedPlabIdx = Mux(highSelected, destAttrRegionCache(31 downto 24), destAttrRegionCache(15 downto 8)).resize(1)

        when(!destAttrRegionValid) {
          issueReq(m, destAttrBase, False, U(4), U(0), True, False, mmuIssued(i)) { rd =>
            slotCtx(i).destAttrPtr := destAttrBase + Mux(highSelected, U(2), U(0))
            slotCtx(i).destRegionAttr := Mux(highSelected, rd(31 downto 16), rd(15 downto 0))
            slotCtx(i).plabTargetIdx := Mux(highSelected, rd(31 downto 24), rd(15 downto 8)).resize(1)

            destAttrFillValid(i) := True
            destAttrFillData(i) := rd(31 downto 0)

            goto(AGE_DECIDE) // miss 情况保守处理
          }
        } otherwise {
          slotCtx(i).destAttrPtr := destAttrBase + Mux(highSelected, U(2), U(0))
          slotCtx(i).destRegionAttr := cachedDestAttr
          slotCtx(i).plabTargetIdx := cachedPlabIdx

          when(srcType =/= U(0)) {
            goto(PLAB_SELECT)
          }.elsewhen(slotCtx(i).markWord(0)) {
            gotoPlabSelectHelper(i, slotCtx(i).markWord)
            goto(PLAB_SELECT)
          } otherwise {
            val addr = Mux(slotCtx(i).markWord(1), slotCtx(i).markWord ^ U"x2".resize(GCElementWidth), slotCtx(i).markWord)

            issueReq(m, addr, False, U(8), U(0), True, False, mmuIssued(i)) { rd =>
              gotoPlabSelectHelper(i, rd(GCElementWidth - 1 downto 0))
              goto(PLAB_SELECT)
            }
          }
        }
      }

      AGE_DECIDE.whenIsActive {
        val src_region_attr_type = slotCtx(i).srcRegionAttr(15 downto 8)
        when(src_region_attr_type =/= U(0)) {
          goto(PLAB_SELECT)
        }.otherwise {
          when(slotCtx(i).markWord(0)) {
            gotoPlabSelectHelper(i, slotCtx(i).markWord)
            goto(PLAB_SELECT)
          }.otherwise {
            val addr = Mux(slotCtx(i).markWord(1), slotCtx(i).markWord ^ U"x2".resize(GCElementWidth), slotCtx(i).markWord)

            issueReq(m, addr, False, U(8), U(0), True, False, mmuIssued(i)) { rd =>
              gotoPlabSelectHelper(i, rd(GCElementWidth - 1 downto 0))
              goto(PLAB_SELECT)
            }
          }
        }
      }

      PLAB_SELECT.whenIsActive {
        val idx = slotCtx(i).plabTargetIdx
        val enough = ((plabCacheEnd(idx) - plabCacheTop(idx)) / U(8)) >= slotCtx(i).size

        when(plabCacheValid(idx)) {
          when(!slotAllocCacheHazard(i) && enough && slotPlabAllocGrant(i)){
            doPlabCacheAllocDirect()
          }elsewhen (!slotAllocCacheHazard(i) && !enough){
            goto(ALLOC_CACHE)
          }
        } elsewhen plabRefillBusy(idx) {
        } elsewhen slotPlabRefillGrant(i) {
          when(plabCacheBufferValid(idx)) {
            goto(READ_PLAB_TOPEND)
          } otherwise {
            goto(READ_PLAB_PTR)
          }
        }
      }

      READ_PLAB_PTR.whenIsActive {
        val addr = (io.ConfigIO.PlabAllocatorPtr + U"x10" + slotCtx(i).plabTargetIdx * U(8)).resize(MMUAddrWidth)
        issueReq(m, addr, False, U(8), U(0), True, False, mmuIssued(i)) { rd =>
          plabPtrFillValid(i) := True
          plabPtrFillIdx(i) := slotCtx(i).plabTargetIdx
          plabPtrFillData(i) := rd(GCElementWidth - 1 downto 0)

          goto(READ_PLAB_BUF)
        }
      }

      READ_PLAB_BUF.whenIsActive {
        val idx = slotCtx(i).plabTargetIdx
        issueReq(m, plabCacheBufferPtr(idx).resize(MMUAddrWidth), False, U(8), U(0), True, False, mmuIssued(i)) { rd =>
          plabBufFillValid(i) := True
          plabBufFillIdx(i) := idx
          plabBufFillData(i) := rd(GCElementWidth - 1 downto 0)

          goto(READ_PLAB_TOPEND)
        }
      }

      READ_PLAB_TOPEND.whenIsActive {
        val idx = slotCtx(i).plabTargetIdx
        val addr = (plabCacheBuffer(idx) + U"x28").resize(MMUAddrWidth)

        issueReq(m, addr, False, U(32), U(0), True, False, mmuIssued(i)) { rd =>
          plabTopEndFillValid(i) := True
          plabTopEndFillIdx(i) := idx
          plabBottomFillData(i) := rd(GCElementWidth - 1 downto 0)
          plabTopFillData(i) := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          plabEndFillData(i) := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          plabHardEndFillData(i) := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)

          goto(PLAB_SELECT)
        }
      }

      ALLOC_CACHE.whenIsActive {
        val idx = slotCtx(i).plabTargetIdx
        val otherIdx =  1 - idx

        slotCtx(i).destOopPtr := U(0)

        when(plabCacheValid(otherIdx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
          plabCacheValid(otherIdx) := False
        }
        plabCacheValid(idx) := False

        when(slotCtx(i).allocIssued || slotAllocAccept(i)) {
          slotCtx(i).allocIssued := False
          slotCtx(i).afterAllocCache := True
          slotCtx(i).usingPlabCacheBuffer := False
          slotCtx(i).heldPlabCacheBuffer := 0
          slotCtx(i).forwardDecided := False
          slotCtx(i).forwardPtr := 0

          goto(WAIT_ALLOC)
        }
      }

      WAIT_ALLOC.whenIsActive {
        when(slotCtx(i).allocDone) {
          when(slotCtx(i).destOopPtr === U(0, GCElementWidth bits)) {
            slotCtx(i).allocDone := False
            slotCtx(i).plabTargetIdx := U(1, 1 bits)
            slotCtx(i).plabForceOld := True

            goto(PLAB_SELECT)
          } otherwise {
            slotCtx(i).afterAllocCache := True
            slotCtx(i).usingPlabCacheBuffer := False
            slotCtx(i).heldPlabCacheBuffer := 0
            slotCtx(i).forwardDecided := False
            slotCtx(i).forwardPtr := 0

            when(slotCtx(i).plabForceOld) {
              when(slotCtx(i).plab_refill_failed) {
                issueReq(m, io.ConfigIO.ParScanThreadStatePtr + U"x17c", True, U(4), U(1), False, False, mmuIssued(i)) { _ => }
              }

              when(mmuIssued(i) || !slotCtx(i).plab_refill_failed) {
                slotCtx(i).allocDone := False
                mmuIssued(i) := False

                goto(WRITE_FORCE_OLD)
              }
            } otherwise {
              slotCtx(i).allocDone := False
              goto(DECIDE_FORWARD_PTR)
            }
          }
        }
      }

      WRITE_FORCE_OLD.whenIsActive {
        val addr = (slotCtx(i).destAttrPtr + U(1)).resize(MMUAddrWidth)
        issueReq(m, addr, True, U(1), U(1), False, False, mmuIssued(i)) { _ => }

        when(mmuIssued(i)) {
          mmuIssued(i) := False
          slotCtx(i).destRegionAttr(15 downto 8) := 1

          when(slotCtx(i).destAttrPtr === slotCtx(i).regionAttrPtr) {
            slotCtx(i).srcRegionAttr(15 downto 8) := 1
          }.elsewhen(slotCtx(i).destAttrPtr === io.ConfigIO.ParScanThreadStatePtr + U"x178") {
            destAttrRegionCache(15 downto 8) := 1
          }.elsewhen(slotCtx(i).destAttrPtr === io.ConfigIO.ParScanThreadStatePtr + U"x17a") {
            destAttrRegionCache(31 downto 24) := 1
          }

          goto(DECIDE_FORWARD_PTR)
        }
      }

      DECIDE_FORWARD_PTR.whenIsActive {
        val new_mw = Cat(slotCtx(i).destOopPtr(GCElementWidth - 1 downto 2), U(3, 2 bits)).asUInt.resize(GCElementWidth)

        // @todo atomic cmpxchg
        issueReq(m, slotCtx(i).srcOopPtr, True, U(8), new_mw, True, True, mmuIssued(i)) { rd =>
          slotCtx(i).forwardDecided := True

          when(slotCtx(i).markWord === slotCtx(i).markWord) {
            slotCtx(i).forwardPtr := 0

            toFetchPending(i) := True
            toFetchSrcOopPtr(i) := slotCtx(i).srcOopPtr
            toFetchWriteValue(i) := new_mw

            goto(SEND_WORK)
          } otherwise {
            slotCtx(i).forwardPtr := slotCtx(i).markWord & ~U"x3".resize(GCElementWidth bits)
            goto(WRITE_FORWARDPTR_NOT_ZERO)
          }

        }
      }

      SEND_WORK.whenIsActive {
        val needTrace = slotCtx(i).kid =/= U(TypeArrayKlassID, 32 bits)

        when(!slotCtx(i).writeDestOopPtrDone) {
          val addr = slotCtx(i).destOopPtr.resize(MMUAddrWidth)

          val new_age = Mux(slotCtx(i).age + 1 < 15, slotCtx(i).age + 1, slotCtx(i).age)
          val writeValue = Mux(slotCtx(i).destRegionAttr(15 downto 8) === 0 && slotCtx(i).markWord(0),
            (slotCtx(i).markWord & ~(U"x1111" << 3).resize(GCElementWidth)) | (new_age(3 downto 0).resize(8) << 3).resize(GCElementWidth),
            slotCtx(i).markWord)

          issueReq(m, addr, True, U(8), writeValue, False, False, mmuIssued(i)) { _ => }

          when(mmuIssued(i)) {
            mmuIssued(i) := False
            slotCtx(i).writeDestOopPtrDone := True
          }
        }

        val copyIssuedDone = slotCtx(i).copyIssued || slotCopyAccept(i)
        val traceIssuedDone = slotCtx(i).traceIssued || slotTraceAccept(i) || !needTrace

        when(copyIssuedDone && traceIssuedDone && slotCtx(i).writeDestOopPtrDone) {
          when(slotCtx(i).destOopPtr(15 downto 0) === 0 && !slotCtx(i).markWord(0)) {
            goto(GET_MONITOR_MW)
          }.otherwise {
            goto(WAIT_COPY_TRACE)
          }
        }
      }

      GET_MONITOR_MW.whenIsActive {
        val addr = Mux(slotCtx(i).markWord(1), slotCtx(i).markWord ^ U"x2".resize(GCElementWidth), slotCtx(i).markWord)

        issueReq(m, addr, False, U(8), U(0), True, False, mmuIssued(i)) { rd =>
          slotCtx(i).monitor_mw := rd(GCElementWidth - 1 downto 0)
          goto(WRITE_MONITOR_MW)
        }
      }

      WRITE_MONITOR_MW.whenIsActive {
        val addr = Mux(slotCtx(i).markWord(1), slotCtx(i).markWord ^ U"x2".resize(GCElementWidth), slotCtx(i).markWord)

        val new_age = Mux(slotCtx(i).age + 1 < 15, slotCtx(i).age + 1, slotCtx(i).age)
        val writeValue = (slotCtx(i).monitor_mw & ~(U"x1111" << 3).resize(GCElementWidth)) | (new_age(3 downto 0).resize(8) << 3).resize(GCElementWidth)

        issueReq(m, addr, True, U(8), writeValue, False, False, mmuIssued(i)) { _ => }

        when(mmuIssued(i)) {
          mmuIssued(i) := False
          goto(WAIT_COPY_TRACE)
        }
      }

      WAIT_COPY_TRACE.whenIsActive {
        val needTrace = slotCtx(i).kid =/= U(TypeArrayKlassID, 32 bits)

        val copyFinished = slotCtx(i).copyDone
        val traceFinished = slotCtx(i).traceDone || !needTrace

        when(copyFinished && traceFinished) {
          survivorDonePending(i) := True
          survivorDoneOwner(i) := slotCtx(i).owner
          survivorDoneDest(i) := slotCtx(i).destOopPtr

          finishSlot(i)
        }
      }

      WRITE_FORWARDPTR_NOT_ZERO.whenIsActive {
        val idx = slotCtx(i).plabTargetIdx

        when(slotCtx(i).destOopPtr >= plabCacheBottom(idx) && slotCtx(i).destOopPtr < plabCacheHardEnd(idx)) {
          val otherIdx = idx ^ 1

          when(plabCacheValid(idx)) {
            plabCacheTop(idx) := slotCtx(i).destOopPtr
          }

          when(plabCacheValid(otherIdx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
            plabCacheTop(otherIdx) := slotCtx(i).destOopPtr
          }

          issueReq(m, plabCacheBuffer(idx) + U"x30", True, U(8), slotCtx(i).destOopPtr, False, False, mmuIssued(i)) { _ => }
        }.otherwise {
          val words = slotCtx(i).size >> 3
          val headSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(2), U(3))
          val cond = words >= headSize
          val temp_klass_ptr = Mux(cond, io.ConfigIO.intArrayKlassObj, io.ConfigIO.objectKlassObj)

          val writeOff0 = U(1, 64 bits)

          val writeOff8 = Mux(io.ConfigIO.UseCompressedKlassPointer,
            ((temp_klass_ptr - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64),
            temp_klass_ptr)

          val writeOff12_16 = ((words - headSize) * 2).resize(32)

          val writeValue = Mux(io.ConfigIO.UseCompressedKlassPointer,
            Cat(writeOff12_16, writeOff8.resize(32), writeOff0).resize(MMUDataWidth),
            Cat(writeOff12_16, writeOff8, writeOff0).resize(MMUDataWidth)).asUInt

          val writeSize = Mux(io.ConfigIO.UseCompressedKlassPointer && cond,
              U(16),
              Mux(io.ConfigIO.UseCompressedKlassPointer, U(12), Mux(cond, U(20), U(16)))).resize(LineBytesNumBitSize)

          issueReq(m, slotCtx(i).destOopPtr, True, writeSize, writeValue, False, False, mmuIssued(i)) { _ => }
        }

        when(mmuIssued(i)) {
          survivorDonePending(i) := True
          survivorDoneOwner(i) := slotCtx(i).owner
          survivorDoneDest(i) := slotCtx(i).forwardPtr

          finishSlot(i)
        }
      }

      always {
        when(slotGotoIdle(i)) {
          goto(IDLE)
        } elsewhen(slotStart(i)) {
          goto(READ_KLASS)
        }
      }
    }

    slotIsPlabSelect(i) := slotFsm.isActive(slotFsm.PLAB_SELECT)
    slotIsAllocCache(i) := slotFsm.isActive(slotFsm.ALLOC_CACHE)
    slotIsSendWork(i) := slotFsm.isActive(slotFsm.SEND_WORK)

    if(DebugEnable) {
      slotStateDebug(i) := U(0)
      when(slotFsm.isActive(slotFsm.IDLE)) {
        slotStateDebug(i) := U(0)
      }.elsewhen(slotFsm.isActive(slotFsm.READ_KLASS)) {
        slotStateDebug(i) := U(1)
      }.elsewhen(slotFsm.isActive(slotFsm.SIZE_DEICIDE)) {
        slotStateDebug(i) := U(2)
      }.elsewhen(slotFsm.isActive(slotFsm.DEST_ATTR_DECIDE)) {
        slotStateDebug(i) := U(3)
      }.elsewhen(slotFsm.isActive(slotFsm.AGE_DECIDE)) {
        slotStateDebug(i) := U(4)
      }.elsewhen(slotFsm.isActive(slotFsm.PLAB_SELECT)) {
        slotStateDebug(i) := U(5)
      }.elsewhen(slotFsm.isActive(slotFsm.READ_PLAB_PTR)) {
        slotStateDebug(i) := U(6)
      }.elsewhen(slotFsm.isActive(slotFsm.READ_PLAB_BUF)) {
        slotStateDebug(i) := U(7)
      }.elsewhen(slotFsm.isActive(slotFsm.READ_PLAB_TOPEND)) {
        slotStateDebug(i) := U(8)
      }.elsewhen(slotFsm.isActive(slotFsm.ALLOC_CACHE)) {
        slotStateDebug(i) := U(9)
      }.elsewhen(slotFsm.isActive(slotFsm.WAIT_ALLOC)) {
        slotStateDebug(i) := U(10)
      }.elsewhen(slotFsm.isActive(slotFsm.WRITE_FORCE_OLD)) {
        slotStateDebug(i) := U(11)
      }.elsewhen(slotFsm.isActive(slotFsm.DECIDE_FORWARD_PTR)) {
        slotStateDebug(i) := U(12)
      }.elsewhen(slotFsm.isActive(slotFsm.SEND_WORK)) {
        slotStateDebug(i) := U(13)
      }.elsewhen(slotFsm.isActive(slotFsm.GET_MONITOR_MW)) {
        slotStateDebug(i) := U(14)
      }.elsewhen(slotFsm.isActive(slotFsm.WRITE_MONITOR_MW)) {
        slotStateDebug(i) := U(15)
      }.elsewhen(slotFsm.isActive(slotFsm.WAIT_COPY_TRACE)) {
        slotStateDebug(i) := U(16)
      }.elsewhen(slotFsm.isActive(slotFsm.WRITE_FORWARDPTR_NOT_ZERO)) {
        slotStateDebug(i) := U(17)
      }
    }
  }

  // Cross-slot PLAB safety hazard
  for (i <- 0 until 2) {
    val other = 1 - i
    val myIdx = slotCtx(i).plabTargetIdx

    val samePlabBuffer = slotValid(other) && slotCtx(other).usingPlabCacheBuffer &&
        plabCacheValid(myIdx) && slotCtx(other).heldPlabCacheBuffer === plabCacheBuffer(myIdx)

    val otherNeedBlock = slotValid(other) && slotCtx(other).afterAllocCache && samePlabBuffer &&
        (!slotCtx(other).forwardDecided || slotCtx(other).forwardPtr =/= U(0, GCElementWidth bits))

    slotAllocCacheHazard(i) := otherNeedBlock
  }

  // PLAB cache allocation arbitration fixed priority: slot0 > slot1
  val plabAllocReq = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    val idx = slotCtx(i).plabTargetIdx
    val enough = ((plabCacheEnd(idx) - plabCacheTop(idx)) / U(8)) >= slotCtx(i).size

    plabAllocReq(i) := slotValid(i) && slotIsPlabSelect(i) && plabCacheValid(idx) && enough && !slotAllocCacheHazard(i)
  }

  val grantPlabAlloc0 = plabAllocReq(0)
  val grantPlabAlloc1 = !plabAllocReq(0) && plabAllocReq(1)

  slotPlabAllocGrant(0) := grantPlabAlloc0
  slotPlabAllocGrant(1) := grantPlabAlloc1

  // PLAB refill request generation
  val plabRefillReq = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    val idx = slotCtx(i).plabTargetIdx

    plabRefillReq(i) :=
      slotValid(i) &&
        slotIsPlabSelect(i) &&
        !plabCacheValid(idx) &&
        !plabRefillBusy(idx)
  }

  // shared cache fill: klass cache
  val grantKlassFill0 = klassFillValid(0)
  val grantKlassFill1 = !klassFillValid(0) && klassFillValid(1)

  when(grantKlassFill0 || grantKlassFill1) {
    val fillPtr = Mux(grantKlassFill0, klassFillPtr(0), klassFillPtr(1))
    val fillKidLh = Mux(grantKlassFill0, klassFillKidLh(0), klassFillKidLh(1))

    klassCacheValid(klassCacheReplacePtr) := True
    klassCachePtr(klassCacheReplacePtr) := fillPtr
    klassCacheKidLh(klassCacheReplacePtr) := fillKidLh
    klassCacheReplacePtr := klassCacheReplacePtr + 1
  }

  // shared dest attr cache fill
  val grantDestAttrFill0 = destAttrFillValid(0)
  val grantDestAttrFill1 = !destAttrFillValid(0) && destAttrFillValid(1)

  when(grantDestAttrFill0 || grantDestAttrFill1) {
    val fillData = Mux(grantDestAttrFill0, destAttrFillData(0), destAttrFillData(1))

    destAttrRegionValid := True
    destAttrRegionCache := fillData
  }

  // PLAB refill-start arbitration
  val grantPlabRefill0 = plabRefillReq(0)
  val grantPlabRefill1 = !plabRefillReq(0) && plabRefillReq(1)

  when(grantPlabRefill0) {
    val idx = slotCtx(0).plabTargetIdx

    plabRefillBusy(idx) := True
    plabRefillOwner(idx) := U(0, 1 bits)
    slotPlabRefillGrant(0) := True
  } elsewhen grantPlabRefill1 {
    val idx = slotCtx(1).plabTargetIdx

    plabRefillBusy(idx) := True
    plabRefillOwner(idx) := U(1, 1 bits)
    slotPlabRefillGrant(1) := True
  }

  // PLAB pointer fill
  for (j <- 0 until 2) {
    when(plabPtrFillValid(0) && plabPtrFillIdx(0) === U(j, 1 bits)) {
      plabCacheBufferPtr(j) := plabPtrFillData(0)
    } elsewhen(plabPtrFillValid(1) && plabPtrFillIdx(1) === U(j, 1 bits)) {
      plabCacheBufferPtr(j) := plabPtrFillData(1)
    }
  }

  // PLAB buffer fill
  for (j <- 0 until 2) {
    when(plabBufFillValid(0) && plabBufFillIdx(0) === U(j, 1 bits)) {
      plabCacheBuffer(j) := plabBufFillData(0)
      plabCacheBufferValid(j) := True
    } elsewhen(plabBufFillValid(1) && plabBufFillIdx(1) === U(j, 1 bits)) {
      plabCacheBuffer(j) := plabBufFillData(1)
      plabCacheBufferValid(j) := True
    }
  }

  // PLAB top/end fill
  for (j <- 0 until 2) {
    when(plabTopEndFillValid(0) && plabTopEndFillIdx(0) === U(j, 1 bits)) {
      plabCacheTop(j) := plabTopFillData(0)
      plabCacheEnd(j) := plabEndFillData(0)
      plabCacheBottom(j) := plabBottomFillData(0)
      plabCacheHardEnd(j) := plabHardEndFillData(0)
      plabCacheValid(j) := True
      plabRefillBusy(j) := False
    } elsewhen(plabTopEndFillValid(1) && plabTopEndFillIdx(1) === U(j, 1 bits)) {
      plabCacheTop(j) := plabTopFillData(1)
      plabCacheEnd(j) := plabEndFillData(1)
      plabCacheBottom(j) := plabBottomFillData(1)
      plabCacheHardEnd(j) := plabHardEndFillData(1)
      plabCacheValid(j) := True
      plabRefillBusy(j) := False
    }
  }

  // ToCopySurvivor.isTypeArray pulse arbitration
  val grantTypeArray0 = typeArrayPending(0)
  val grantTypeArray1 = !typeArrayPending(0) && typeArrayPending(1)

  when(grantTypeArray0 || grantTypeArray1) {
    io.ToCopySurvivor.isTypeArray := True
    io.ToCopySurvivor.DoneOwner := Mux(grantTypeArray0, typeArrayOwner(0), typeArrayOwner(1))

    when(grantTypeArray0) {
      typeArrayPending(0) := False
    } otherwise {
      typeArrayPending(1) := False
    }
  }

  // ToCopySurvivor.Done pulse arbitration
  val grantDone0 = survivorDonePending(0)
  val grantDone1 = !survivorDonePending(0) && survivorDonePending(1)

  when(grantDone0 || grantDone1) {
    io.ToCopySurvivor.Done := True
    io.ToCopySurvivor.DoneOwner := Mux(grantDone0, survivorDoneOwner(0), survivorDoneOwner(1))
    io.ToCopySurvivor.DestOopPtr := Mux(grantDone0, survivorDoneDest(0), survivorDoneDest(1))

    when(grantDone0) {
      survivorDonePending(0) := False
    } otherwise {
      survivorDonePending(1) := False
    }
  }

  // ToFetch forwarding pulse arbitration
  val grantFetch0 = toFetchPending(0)
  val grantFetch1 = !toFetchPending(0) && toFetchPending(1)

  when(grantFetch0 || grantFetch1) {
    io.ToFetch.valid := True

    io.ToFetch.srcOopPtr := Mux(grantFetch0, toFetchSrcOopPtr(0), toFetchSrcOopPtr(1))
    io.ToFetch.writeValue := Mux(grantFetch0, toFetchWriteValue(0), toFetchWriteValue(1))

    when(grantFetch0) {
      toFetchPending(0) := False
    } otherwise {
      toFetchPending(1) := False
    }
  }

  // copy arbitration
  val wantCopy = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    wantCopy(i) := slotValid(i) && slotIsSendWork(i) && !slotCtx(i).copyIssued
  }

  when(!copyBusy) {
    when(wantCopy(0)) {
      val totalSize = (slotCtx(0).size * U(8)).resize(32)
      val compressedSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(16), U(20))

      io.ToCopy.Valid := True
      io.ToCopy.Size := Mux(slotCtx(0).kid === U(ObjectArrayKlassID, 32 bits),
          totalSize - compressedSize,
          totalSize - U(8))
      io.ToCopy.SrcOopPtr := Mux(slotCtx(0).kid === U(ObjectArrayKlassID, 32 bits),
          slotCtx(0).srcOopPtr + compressedSize,
          slotCtx(0).srcOopPtr + U(8))
      io.ToCopy.DestOopPtr := Mux(slotCtx(0).kid === U(ObjectArrayKlassID, 32 bits),
          slotCtx(0).destOopPtr + compressedSize,
          slotCtx(0).destOopPtr + U(8))

      when(io.ToCopy.Ready) {
        slotCtx(0).copyIssued := True
        slotCopyAccept(0) := True
        copyBusy := True
        copyOwner := U(0, 1 bits)
      }
    } elsewhen(wantCopy(1)) {
      val totalSize = (slotCtx(1).size * U(8)).resize(32)
      val compressedSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(16), U(20))

      io.ToCopy.Valid := True
      io.ToCopy.Size := Mux(slotCtx(1).kid === U(ObjectArrayKlassID, 32 bits),
          totalSize - compressedSize,
          totalSize - U(8))
      io.ToCopy.SrcOopPtr := Mux(slotCtx(1).kid === U(ObjectArrayKlassID, 32 bits),
          slotCtx(1).srcOopPtr + compressedSize,
          slotCtx(1).srcOopPtr + U(8))
      io.ToCopy.DestOopPtr := Mux(slotCtx(1).kid === U(ObjectArrayKlassID, 32 bits),
          slotCtx(1).destOopPtr + compressedSize,
          slotCtx(1).destOopPtr + U(8))

      when(io.ToCopy.Ready) {
        slotCtx(1).copyIssued := True
        slotCopyAccept(1) := True
        copyBusy := True
        copyOwner := U(1, 1 bits)
      }
    }
  }

  // trace arbitration
  val wantTrace = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    wantTrace(i) := slotValid(i) && slotIsSendWork(i) &&
        !slotCtx(i).traceIssued && slotCtx(i).kid =/= U(TypeArrayKlassID, 32 bits)
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
        slotTraceAccept(0) := True
        traceBusy := True
        traceOwner := U(0, 1 bits)
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
        slotTraceAccept(1) := True
        traceBusy := True
        traceOwner := U(1, 1 bits)
      }
    }
  }

  // allocate arbitration
  val wantAlloc = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    wantAlloc(i) := slotValid(i) && slotIsAllocCache(i) && !slotCtx(i).allocIssued && !slotAllocCacheHazard(i)
  }

  when(!allocBusy) {
    when(wantAlloc(0)) {
      io.ToAllocate.Valid := True
      io.ToAllocate.Size := slotCtx(0).size
      io.ToAllocate.DestAttrType := Mux(slotCtx(0).plabForceOld, U(1, 8 bits), slotCtx(0).destRegionAttr(15 downto 8))

      when(io.ToAllocate.Ready) {
        slotCtx(0).allocIssued := True
        slotAllocAccept(0) := True
        allocBusy := True
        allocOwner := U(0, 1 bits)
      }
    } elsewhen wantAlloc(1) {
      io.ToAllocate.Valid := True
      io.ToAllocate.Size := slotCtx(1).size
      io.ToAllocate.DestAttrType := Mux(slotCtx(1).plabForceOld, U(1, 8 bits), slotCtx(1).destRegionAttr(15 downto 8))

      when(io.ToAllocate.Ready) {
        slotCtx(1).allocIssued := True
        slotAllocAccept(1) := True
        allocBusy := True
        allocOwner := U(1, 1 bits)
      }
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}