package hwgc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

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
}

/**
 * Single-slot / single-Mreq version of GCOopCopy2Survivor.
 *
 * Main differences from the original dual-slot design:
 *   - io has only one LocalMMUIO: Mreq
 *   - slotValid / slotCtx / mmuIssued are scalar registers
 *   - fixed-priority arbitration between slot0/slot1 is removed
 *   - copy / trace / allocate owner registers are removed
 *   - pending output pulses are scalar
 *
 * Note:
 *   The original WRITE_FORWARDPTR_NOT_ZERO state had incomplete code:
 *
 *     val myEnd = destOopPtr + size * 8
 *     val other_i = i ^ 1
 *     when(slotCtx(other_i).)
 *
 *   In this single-slot version there is no competing slot, so the rollback
 *   logic simply rewinds PLAB top to destOopPtr when the allocated object
 *   belongs to the cached PLAB range.
 */
class GCOopCopy2Survivor extends Module with HWParameters with GCParameters {
  val io = new Bundle {
    val Mreq = master(new LocalMMUIO)

    val ToCopy = master(new GCToCopy)
    val ToFetch = master(new GCWriteSrcOopPtr)
    val ToTrace = master(new GCToTrace)
    val ToStack = master(new GCUpdatedRegion)
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

    m.RequestSize.valid := False
    m.RequestSize.payload.clearAll()

    m.Response.ready := True
  }

  clearMreq(io.Mreq)

  io.ToCopy.clearIn()
  io.ToFetch.clearIn()
  io.ToTrace.clearIn()
  io.ToAllocate.clearIn()

  io.ToCopySurvivor.Done := False
  io.ToCopySurvivor.DoneOwner := U(0)
  io.ToCopySurvivor.DestOopPtr := U(0)
  io.ToCopySurvivor.isTypeArray := False

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCOopCopy2SurvivorSingle<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  // single slot context
  val slotValid = RegInit(False)
  val slotCtx = Reg(CopySurvivorSlotCtx()) init (CopySurvivorSlotCtx().getZero)
  val mmuIssued = RegInit(False)

  val slotStart = Bool()
  val slotGotoIdle = Bool()
  val slotPlabRefillGrant = Bool()
  val slotPlabAllocGrant = Bool()

  slotStart := False
  slotGotoIdle := False
  slotPlabRefillGrant := False

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

  val KlassCacheEntries = 16
  val klassCacheValid = Vec.fill(KlassCacheEntries)(RegInit(False))
  val klassCachePtr = Vec.fill(KlassCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val klassCacheKidLh = Vec.fill(KlassCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val klassCacheReplacePtr = RegInit(U(0, log2Up(KlassCacheEntries) bits))

  // shared resource busy flags
  val copyBusy = RegInit(False)
  val traceBusy = RegInit(False)
  val allocBusy = RegInit(False)

  // pending single-output pulses
  val typeArrayPending = RegInit(False)
  val typeArrayOwner = RegInit(U(0, 1 bits))

  val survivorDonePending = RegInit(False)
  val survivorDoneOwner = RegInit(U(0, 1 bits))
  val survivorDoneDest = RegInit(U(0, GCElementWidth bits))

  val toFetchPending = RegInit(False)
  val toFetchSrcOopPtr = RegInit(U(0, GCElementWidth bits))
  val toFetchWriteValue = RegInit(U(0, GCElementWidth bits))

  // helpers
  def clearSlotRuntime(): Unit = {
    slotCtx.destOopPtr := 0
    slotCtx.lh := 0
    slotCtx.kid := 0
    slotCtx.size := 0
    slotCtx.age := 0
    slotCtx.destAttrPtr := 0
    slotCtx.destRegionAttr := 0
    slotCtx.plabTargetIdx := 0
    slotCtx.plabForceOld := False
    slotCtx.writeDestOopPtrDone := False
    slotCtx.copyIssued := False
    slotCtx.traceIssued := False
    slotCtx.copyDone := False
    slotCtx.traceDone := False
    slotCtx.allocIssued := False
    slotCtx.allocDone := False
    slotCtx.plab_refill_failed := False
    slotCtx.forwardPtr := 0
    slotCtx.monitor_mw := 0

    mmuIssued := False
  }

  def allocSlot(): Unit = {
    slotValid := True

    slotCtx.owner := io.ToCopySurvivor.Owner
    slotCtx.srcOopPtr := io.ToCopySurvivor.SrcOopPtr
    slotCtx.markWord := io.ToCopySurvivor.MarkWord
    slotCtx.klassPtr := io.ToCopySurvivor.KlassPtr
    slotCtx.srcLength := io.ToCopySurvivor.SrcLength
    slotCtx.srcRegionAttr := io.ToCopySurvivor.SrcRegionAttr
    slotCtx.regionAttrPtr := io.ToCopySurvivor.RegionAttrPtr

    clearSlotRuntime()
    slotStart := True

    dbg(Seq("Allocate task to single slot, src=", io.ToCopySurvivor.SrcOopPtr))
  }

  def finishSlot(): Unit = {
    slotValid := False
    clearSlotRuntime()
    slotGotoIdle := True

    dbg(Seq("Finish single slot"))
  }

  def compressedKlassPtr: UInt =
    (io.ConfigIO.CompressedKlassPointerBase +
      (slotCtx.klassPtr(31 downto 0) << io.ConfigIO.CompressedKlassPointerShift)).resize(GCElementWidth)

  def lookupKlassPtr: UInt =
    Mux(io.ConfigIO.UseCompressedKlassPointer, compressedKlassPtr, slotCtx.klassPtr)

  def gotoPlabSelectHelper(markWord: UInt): Unit = {
    val new_age = (markWord >> 3).resize(4).resize(32)
    slotCtx.age := new_age

    when(new_age < io.ConfigIO.AgeThreshold) {
      slotCtx.destRegionAttr := slotCtx.srcRegionAttr
      slotCtx.destAttrPtr := slotCtx.regionAttrPtr
      slotCtx.plabTargetIdx := slotCtx.srcRegionAttr(15 downto 8).resize(1)
    }
  }

  // klass cache lookup
  val klassLookupPtr = UInt(GCElementWidth bits)
  val klassCacheHit = Bool()
  val klassCacheHitIndex = UInt(log2Up(KlassCacheEntries) bits)

  klassLookupPtr := lookupKlassPtr

  val hitVec = Vec.fill(KlassCacheEntries)(Bool())
  for (j <- 0 until KlassCacheEntries) {
    hitVec(j) := klassCacheValid(j) && klassCachePtr(j) === klassLookupPtr
  }

  klassCacheHit := hitVec.orR
  klassCacheHitIndex := OHToUInt(hitVec.asBits)

  // shared fill requests
  val klassFillValid = Bool()
  val klassFillPtr = UInt(GCElementWidth bits)
  val klassFillKidLh = UInt(GCElementWidth bits)

  val destAttrFillValid = Bool()
  val destAttrFillData = UInt(32 bits)

  val plabPtrFillValid = Bool()
  val plabPtrFillIdx = UInt(1 bits)
  val plabPtrFillData = UInt(GCElementWidth bits)

  val plabBufFillValid = Bool()
  val plabBufFillIdx = UInt(1 bits)
  val plabBufFillData = UInt(GCElementWidth bits)

  val plabTopEndFillValid = Bool()
  val plabTopEndFillIdx = UInt(1 bits)
  val plabBottomFillData = UInt(GCElementWidth bits) // 0x28
  val plabTopFillData = UInt(GCElementWidth bits)    // 0x30
  val plabEndFillData = UInt(GCElementWidth bits)    // 0x38
  val plabHardEndFillData = UInt(GCElementWidth bits) // 0x40

  klassFillValid := False
  klassFillPtr := U(0, GCElementWidth bits)
  klassFillKidLh := U(0, GCElementWidth bits)

  destAttrFillValid := False
  destAttrFillData := U(0, 32 bits)

  plabPtrFillValid := False
  plabPtrFillIdx := U(0, 1 bits)
  plabPtrFillData := U(0, GCElementWidth bits)

  plabBufFillValid := False
  plabBufFillIdx := U(0, 1 bits)
  plabBufFillData := U(0, GCElementWidth bits)

  plabTopEndFillValid := False
  plabTopEndFillIdx := U(0, 1 bits)
  plabTopFillData := U(0, GCElementWidth bits)
  plabEndFillData := U(0, GCElementWidth bits)
  plabBottomFillData := U(0, GCElementWidth bits)
  plabHardEndFillData := U(0, GCElementWidth bits)

  // task done invalidates shared caches
  when(io.TaskDone) {
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
  when(copyBusy && io.ToCopy.Done) {
    copyBusy := False
    slotCtx.copyDone := True
    dbg(Seq("copy done for single slot"))
  }

  when(traceBusy && io.ToTrace.Done) {
    traceBusy := False
    slotCtx.traceDone := True
    dbg(Seq("trace done for single slot"))
  }

  when(allocBusy && io.ToAllocate.Done) {
    allocBusy := False
    slotCtx.allocDone := True
    slotCtx.destOopPtr := io.ToAllocate.DestObjPtr

    // Kept from original code. If GCToAllocate has an explicit failure flag,
    // replace this with that failure signal.
    slotCtx.plab_refill_failed := io.ToAllocate.Done

    dbg(Seq("allocate done for single slot"))
  }

  // input admission
  io.ToCopySurvivor.Ready := !slotValid

  when(io.ToCopySurvivor.Valid && io.ToCopySurvivor.Ready) {
    allocSlot()
  }

  // slot state visibility
  val slotIsPlabSelect = Bool()
  val slotIsAllocCache = Bool()
  val slotIsSendWork = Bool()

  // per-slot state machine, now scalar
  val m = io.Mreq

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
    val GET_FORWARD_PTR = new State
    val SEND_WORK = new State
    val GET_MONITOR_MW = new State
    val WRITE_MONITOR_MW = new State
    val WAIT_COPY_TRACE = new State
    val WRITE_FORWARDPTR_NOT_ZERO = new State

    IDLE.whenIsActive {
      // wait for slotStart
    }

    READ_KLASS.whenIsActive {
      when(klassCacheHit) {
        val idx = klassCacheHitIndex

        slotCtx.klassPtr := klassLookupPtr
        slotCtx.lh := klassCacheKidLh(idx)(31 downto 0)
        slotCtx.kid := klassCacheKidLh(idx)(63 downto 32)

        when(klassCacheKidLh(idx)(63 downto 32) === U(TypeArrayKlassID, 32 bits)) {
          typeArrayPending := True
          typeArrayOwner := slotCtx.owner
        }

        goto(SIZE_DEICIDE)

      } otherwise {
        val newKlassPtr = klassLookupPtr
        val addr = (newKlassPtr + U(8)).resize(MMUAddrWidth)

        issueReq(m, addr, False, U(8), U(0), mmuIssued) { rd =>
          slotCtx.klassPtr := newKlassPtr
          slotCtx.lh := rd(31 downto 0)
          slotCtx.kid := rd(63 downto 32)

          klassFillValid := True
          klassFillPtr := newKlassPtr
          klassFillKidLh := rd(63 downto 0)

          when(rd(63 downto 32) === U(TypeArrayKlassID, 32 bits)) {
            typeArrayPending := True
            typeArrayOwner := slotCtx.owner
          }

          goto(SIZE_DEICIDE)
        }
      }
    }

    SIZE_DEICIDE.whenIsActive {
      when(slotCtx.lh.asSInt < S(0)) {
        val temp = ((slotCtx.srcLength << slotCtx.lh(7 downto 0)) + slotCtx.lh(23 downto 16)).resize(32)
        slotCtx.size := Mux(temp(2 downto 0) =/= U(0), (temp >> U(3)) + U(1), temp >> U(3)).resize(32)

      }.elsewhen(slotCtx.lh.asSInt > S(0) && !slotCtx.lh(0)) {
        slotCtx.size := (slotCtx.lh >> U(3)).resize(32)

      } otherwise {
        when(slotCtx.kid =/= U(InstanceMirrorKlassID, 32 bits)) {
          slotCtx.size := (slotCtx.lh >> U(3)).resize(32)

        } otherwise {
          issueReq(m, slotCtx.srcOopPtr + U"x20", False, U(8), U(0), mmuIssued) { rd =>
            slotCtx.size := rd(31 downto 0)
            goto(DEST_ATTR_DECIDE)
          }
        }
      }

      when(slotCtx.lh.asSInt === S(0) || (slotCtx.lh.asSInt > S(0) && slotCtx.lh(0))) {
        // instance mirror waits for read completion above
      } otherwise {
        goto(DEST_ATTR_DECIDE)
      }
    }

    DEST_ATTR_DECIDE.whenIsActive {
      slotCtx.plabForceOld := False

      val src_region_attr_type = slotCtx.srcRegionAttr(15 downto 8)
      val highSelected = src_region_attr_type === U(1)
      val destAttrBase = (io.ConfigIO.ParScanThreadStatePtr + U"x178").resize(MMUAddrWidth)

      slotCtx.destAttrPtr := destAttrBase + Mux(highSelected, U(2), U(0))

      when(!destAttrRegionValid) {
        issueReq(m, destAttrBase, False, U(4), U(0), mmuIssued) { rd =>
          slotCtx.destRegionAttr := Mux(highSelected, rd(31 downto 16), rd(15 downto 0))
          slotCtx.plabTargetIdx := Mux(highSelected, rd(31 downto 24), rd(15 downto 8)).resize(1)

          destAttrFillValid := True
          destAttrFillData := rd(31 downto 0)

          goto(AGE_DECIDE)
        }

      } otherwise {
        slotCtx.destRegionAttr := Mux(highSelected, destAttrRegionCache(31 downto 16), destAttrRegionCache(15 downto 0))
        slotCtx.plabTargetIdx := Mux(highSelected, destAttrRegionCache(31 downto 24), destAttrRegionCache(15 downto 8)).resize(1)
        goto(AGE_DECIDE)
      }
    }

    AGE_DECIDE.whenIsActive {
      val src_region_attr_type = slotCtx.srcRegionAttr(15 downto 8)

      when(src_region_attr_type =/= U(0)) {
        goto(PLAB_SELECT)

      } otherwise {
        when(slotCtx.markWord(0)) {
          gotoPlabSelectHelper(slotCtx.markWord)
          goto(PLAB_SELECT)

        } otherwise {
          val addr = Mux(slotCtx.markWord(1), slotCtx.markWord ^ U"x2".resize(GCElementWidth), slotCtx.markWord)

          issueReq(m, addr, False, U(8), U(0), mmuIssued) { rd =>
            gotoPlabSelectHelper(rd(GCElementWidth - 1 downto 0))
            goto(PLAB_SELECT)
          }
        }
      }
    }

    PLAB_SELECT.whenIsActive {
      val plabIdx = slotCtx.plabTargetIdx

      when(plabCacheValid(plabIdx)) {
        goto(ALLOC_CACHE)

      } elsewhen(plabRefillBusy(plabIdx)) {
        // wait current refill

      } elsewhen(slotPlabRefillGrant) {
        when(plabCacheBufferValid(plabIdx)) {
          goto(READ_PLAB_TOPEND)
        } otherwise {
          goto(READ_PLAB_PTR)
        }
      }
    }

    READ_PLAB_PTR.whenIsActive {
      val addr = (io.ConfigIO.PlabAllocatorPtr + U"x10" + slotCtx.plabTargetIdx * U(8)).resize(MMUAddrWidth)

      issueReq(m, addr, False, U(8), U(0), mmuIssued) { rd =>
        plabPtrFillValid := True
        plabPtrFillIdx := slotCtx.plabTargetIdx
        plabPtrFillData := rd(GCElementWidth - 1 downto 0)

        goto(READ_PLAB_BUF)
      }
    }

    READ_PLAB_BUF.whenIsActive {
      val idx = slotCtx.plabTargetIdx

      issueReq(m, plabCacheBufferPtr(idx).resize(MMUAddrWidth), False, U(8), U(0), mmuIssued) { rd =>
        plabBufFillValid := True
        plabBufFillIdx := idx
        plabBufFillData := rd(GCElementWidth - 1 downto 0)

        goto(READ_PLAB_TOPEND)
      }
    }

    READ_PLAB_TOPEND.whenIsActive {
      val idx = slotCtx.plabTargetIdx
      val addr = (plabCacheBuffer(idx) + U"x28").resize(MMUAddrWidth)

      issueReq(m, addr, False, U(32), U(0), mmuIssued) { rd =>
        plabTopEndFillValid := True
        plabTopEndFillIdx := idx
        plabBottomFillData := rd(GCElementWidth - 1 downto 0)
        plabTopFillData := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        plabEndFillData := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        plabHardEndFillData := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)

        goto(ALLOC_CACHE)
      }
    }

    ALLOC_CACHE.whenIsActive {
      val idx = slotCtx.plabTargetIdx
      val enough = ((plabCacheEnd(idx) - plabCacheTop(idx)) / U(8)) >= slotCtx.size

      when(enough) {
        when(slotPlabAllocGrant) {
          slotCtx.destOopPtr := plabCacheTop(idx)

          val new_top = (plabCacheTop(idx) + slotCtx.size * U(8)).resize(GCElementWidth)
          val other_idx = idx ^ 1

          when(plabCacheValid(other_idx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
            plabCacheTop(other_idx) := new_top
          }

          issueReq(m, plabCacheBuffer(idx) + U"x30", True, U(8), U(0), mmuIssued) { _ => }

          when(mmuIssued) {
            plabCacheTop(idx) := new_top

            mmuIssued := False
            goto(GET_FORWARD_PTR)
          }
        }

      } otherwise {
        slotCtx.destOopPtr := U(0)

        val other_idx = idx ^ 1

        when(plabCacheValid(other_idx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
          plabCacheValid(other_idx) := False
        }

        plabCacheValid(idx) := False

        when(slotCtx.allocIssued) {
          slotCtx.allocIssued := False
          goto(WAIT_ALLOC)
        }
      }
    }

    WAIT_ALLOC.whenIsActive {
      when(slotCtx.allocDone) {
        when(slotCtx.destOopPtr === U(0, GCElementWidth bits)) {
          slotCtx.allocDone := False

          slotCtx.plabTargetIdx := U(1, 1 bits)
          slotCtx.plabForceOld := True

          goto(PLAB_SELECT)

        } otherwise {
          when(slotCtx.plabForceOld) {
            when(slotCtx.plab_refill_failed) {
              issueReq(m, io.ConfigIO.ParScanThreadStatePtr + U"x17c", True, U(4), U(1), mmuIssued) { _ => }
            }

            when(mmuIssued || !slotCtx.plab_refill_failed) {
              slotCtx.allocDone := False
              mmuIssued := False

              goto(WRITE_FORCE_OLD)
            }

          } otherwise {
            slotCtx.allocDone := False
            goto(GET_FORWARD_PTR)
          }
        }
      }
    }

    WRITE_FORCE_OLD.whenIsActive {
      val addr = (slotCtx.destAttrPtr + U(1)).resize(MMUAddrWidth)

      issueReq(m, addr, True, U(1), U(1), mmuIssued) { _ => }

      when(mmuIssued) {
        mmuIssued := False
        slotCtx.destRegionAttr(15 downto 8) := 1

        when(slotCtx.destAttrPtr === slotCtx.regionAttrPtr) {
          slotCtx.srcRegionAttr(15 downto 8) := 1

        }.elsewhen(slotCtx.destAttrPtr === io.ConfigIO.ParScanThreadStatePtr + U"x178") {
          destAttrRegionCache(15 downto 8) := 1

        }.elsewhen(slotCtx.destAttrPtr === io.ConfigIO.ParScanThreadStatePtr + U"x17a") {
          destAttrRegionCache(31 downto 24) := 1
        }

        goto(GET_FORWARD_PTR)
      }
    }

    GET_FORWARD_PTR.whenIsActive {
      // Kept semantically close to original code:
      // original condition was slotCtx.markWord === slotCtx.markWord, therefore always success.
      val new_mw = Cat(slotCtx.destOopPtr(GCElementWidth - 1 downto 2), U(3, 2 bits)).asUInt.resize(GCElementWidth)

      issueReq(m, slotCtx.srcOopPtr, True, U(8), new_mw, mmuIssued) { _ => }

      when(mmuIssued) {
        mmuIssued := False
        slotCtx.forwardPtr := 0
        goto(SEND_WORK)

        toFetchPending := True
        toFetchSrcOopPtr := slotCtx.srcOopPtr
        toFetchWriteValue := new_mw
      }
    }

    SEND_WORK.whenIsActive {
      val needTrace = slotCtx.kid =/= U(TypeArrayKlassID, 32 bits)

      when(!slotCtx.writeDestOopPtrDone) {
        val addr = slotCtx.destOopPtr.resize(MMUAddrWidth)

        val new_age = Mux(slotCtx.age + 1 < 15, slotCtx.age + 1, slotCtx.age)
        val writeValue = Mux(
          slotCtx.destRegionAttr(15 downto 8) === 0 && slotCtx.markWord(0),
          (slotCtx.markWord & ~(U"x1111" << 3).resize(GCElementWidth)) |
            (new_age(3 downto 0).resize(8) << 3).resize(GCElementWidth),
          slotCtx.markWord
        )

        issueReq(m, addr, True, U(8), writeValue, mmuIssued) { _ => }

        when(mmuIssued) {
          mmuIssued := False
          slotCtx.writeDestOopPtrDone := True
        }
      }

      val copyIssuedDone = slotCtx.copyIssued
      val traceIssuedDone = slotCtx.traceIssued || !needTrace

      when(copyIssuedDone && traceIssuedDone && slotCtx.writeDestOopPtrDone) {
        when(slotCtx.destOopPtr(15 downto 0) === 0 && !slotCtx.markWord(0)) {
          goto(GET_MONITOR_MW)
        } otherwise {
          goto(WAIT_COPY_TRACE)
        }
      }
    }

    GET_MONITOR_MW.whenIsActive {
      val addr = Mux(slotCtx.markWord(1), slotCtx.markWord ^ U"x2".resize(GCElementWidth), slotCtx.markWord)

      issueReq(m, addr, False, U(8), U(0), mmuIssued) { rd =>
        slotCtx.monitor_mw := rd(GCElementWidth - 1 downto 0)
        goto(WRITE_MONITOR_MW)
      }
    }

    WRITE_MONITOR_MW.whenIsActive {
      val addr = Mux(slotCtx.markWord(1), slotCtx.markWord ^ U"x2".resize(GCElementWidth), slotCtx.markWord)
      val new_age = Mux(slotCtx.age + 1 < 15, slotCtx.age + 1, slotCtx.age)
      val writeValue =
        (slotCtx.monitor_mw & ~(U"x1111" << 3).resize(GCElementWidth)) |
          (new_age(3 downto 0).resize(8) << 3).resize(GCElementWidth)

      issueReq(m, addr, True, U(8), writeValue, mmuIssued) { _ => }

      when(mmuIssued) {
        mmuIssued := False
        goto(WAIT_COPY_TRACE)
      }
    }

    WAIT_COPY_TRACE.whenIsActive {
      val needTrace = slotCtx.kid =/= U(TypeArrayKlassID, 32 bits)

      val copyFinished = slotCtx.copyDone
      val traceFinished = slotCtx.traceDone || !needTrace

      when(copyFinished && traceFinished) {
        survivorDonePending := True
        survivorDoneOwner := slotCtx.owner
        survivorDoneDest := slotCtx.destOopPtr

        finishSlot()
      }
    }

    WRITE_FORWARDPTR_NOT_ZERO.whenIsActive {
      val idx = slotCtx.plabTargetIdx

      when(slotCtx.destOopPtr >= plabCacheBottom(idx) && slotCtx.destOopPtr < plabCacheHardEnd(idx)) {
        val other_idx = idx ^ 1

        when(plabCacheValid(idx)) {
          plabCacheTop(idx) := slotCtx.destOopPtr
        }

        when(plabCacheValid(other_idx) && plabCacheBuffer(0) === plabCacheBuffer(1)) {
          plabCacheTop(other_idx) := slotCtx.destOopPtr
        }

        issueReq(m, plabCacheBuffer(idx) + U"x30", True, U(8), slotCtx.destOopPtr, mmuIssued) { _ => }

      } otherwise {
        val words = slotCtx.size >> 3
        val headSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(2), U(3))
        val cond = words >= headSize

        val temp_klass_ptr = Mux(cond, io.ConfigIO.intArrayKlassObj, io.ConfigIO.objectKlassObj)

        val writeOff0 = U(1, 64 bits) // markWord
        val writeOff8 =
          Mux(
            io.ConfigIO.UseCompressedKlassPointer,
            ((temp_klass_ptr - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64),
            temp_klass_ptr
          ) // klass
        val writeOff12_16 = ((words - headSize) * 2).resize(32) // arrayLen

        val writeValue =
          Mux(
            io.ConfigIO.UseCompressedKlassPointer,
            Cat(writeOff12_16, writeOff8.resize(32), writeOff0).resize(MMUDataWidth),
            Cat(writeOff12_16, writeOff8, writeOff0).resize(MMUDataWidth)
          ).asUInt

        val writeSize =
          Mux(
            io.ConfigIO.UseCompressedKlassPointer && cond,
            U(16),
            Mux(
              io.ConfigIO.UseCompressedKlassPointer,
              U(12),
              Mux(cond, U(20), U(16))
            )
          ).resize(LineBytesNumBitSize)

        issueReq(m, slotCtx.destOopPtr, True, writeSize, writeValue, mmuIssued) { _ => }
      }

      when(mmuIssued) {
        survivorDonePending := True
        survivorDoneOwner := slotCtx.owner
        survivorDoneDest := slotCtx.forwardPtr

        finishSlot()
      }
    }

    always {
      when(slotGotoIdle) {
        goto(IDLE)
      } elsewhen(slotStart) {
        goto(READ_KLASS)
      }
    }
  }

  slotIsPlabSelect := slotFsm.isActive(slotFsm.PLAB_SELECT)
  slotIsAllocCache := slotFsm.isActive(slotFsm.ALLOC_CACHE)
  slotIsSendWork := slotFsm.isActive(slotFsm.SEND_WORK)

  // ==========================================================================
  // PLAB cache allocation grant
  // ==========================================================================
  val allocIdx = slotCtx.plabTargetIdx
  val plabEnough =
    ((plabCacheEnd(allocIdx) - plabCacheTop(allocIdx)) / U(8)) >= slotCtx.size

  val plabAllocReq =
    slotValid &&
      slotIsAllocCache &&
      plabEnough

  slotPlabAllocGrant := plabAllocReq

  // ==========================================================================
  // PLAB refill request generation
  // ==========================================================================
  val refillIdx = slotCtx.plabTargetIdx
  val plabRefillReq =
    slotValid &&
      slotIsPlabSelect &&
      !plabCacheValid(refillIdx) &&
      !plabRefillBusy(refillIdx)

  when(plabRefillReq) {
    plabRefillBusy(refillIdx) := True
    slotPlabRefillGrant := True
  }

  // ==========================================================================
  // shared cache fill: klass cache
  // ==========================================================================
  when(klassFillValid) {
    klassCacheValid(klassCacheReplacePtr) := True
    klassCachePtr(klassCacheReplacePtr) := klassFillPtr
    klassCacheKidLh(klassCacheReplacePtr) := klassFillKidLh
    klassCacheReplacePtr := klassCacheReplacePtr + 1
  }

  // ==========================================================================
  // shared dest attr cache fill
  // ==========================================================================
  when(destAttrFillValid) {
    destAttrRegionValid := True
    destAttrRegionCache := destAttrFillData
  }

  // ==========================================================================
  // PLAB pointer fill
  // ==========================================================================
  for (j <- 0 until 2) {
    when(plabPtrFillValid && plabPtrFillIdx === U(j, 1 bits)) {
      plabCacheBufferPtr(j) := plabPtrFillData
    }
  }

  // ==========================================================================
  // PLAB buffer fill
  // ==========================================================================
  for (j <- 0 until 2) {
    when(plabBufFillValid && plabBufFillIdx === U(j, 1 bits)) {
      plabCacheBuffer(j) := plabBufFillData
      plabCacheBufferValid(j) := True
    }
  }

  // ==========================================================================
  // PLAB top/end fill
  // ==========================================================================
  for (j <- 0 until 2) {
    when(plabTopEndFillValid && plabTopEndFillIdx === U(j, 1 bits)) {
      plabCacheTop(j) := plabTopFillData
      plabCacheEnd(j) := plabEndFillData
      plabCacheBottom(j) := plabBottomFillData
      plabCacheHardEnd(j) := plabHardEndFillData
      plabCacheValid(j) := True
      plabRefillBusy(j) := False
    }
  }

  // ==========================================================================
  // ToCopySurvivor.isTypeArray pulse
  // ==========================================================================
  when(typeArrayPending) {
    io.ToCopySurvivor.isTypeArray := True
    io.ToCopySurvivor.DoneOwner := typeArrayOwner
    typeArrayPending := False
  }

  // ==========================================================================
  // ToCopySurvivor.Done pulse
  // ==========================================================================
  when(survivorDonePending) {
    io.ToCopySurvivor.Done := True
    io.ToCopySurvivor.DoneOwner := survivorDoneOwner
    io.ToCopySurvivor.DestOopPtr := survivorDoneDest
    survivorDonePending := False
  }

  // ==========================================================================
  // ToFetch forwarding pulse
  // ==========================================================================
  when(toFetchPending) {
    io.ToFetch.valid := True
    io.ToFetch.srcOopPtr := toFetchSrcOopPtr
    io.ToFetch.writeValue := toFetchWriteValue
    toFetchPending := False
  }

  // ==========================================================================
  // copy issue
  // ==========================================================================
  val wantCopy =
    slotValid &&
      slotIsSendWork &&
      !slotCtx.copyIssued

  when(!copyBusy && wantCopy) {
    val totalSize = (slotCtx.size * U(8)).resize(32)
    val compressedSize = Mux(io.ConfigIO.UseCompressedKlassPointer, U(16), U(20))

    io.ToCopy.Valid := True

    io.ToCopy.Size :=
      Mux(
        slotCtx.kid === U(ObjectArrayKlassID, 32 bits),
        totalSize - compressedSize,
        totalSize - U(8)
      )

    io.ToCopy.SrcOopPtr :=
      Mux(
        slotCtx.kid === U(ObjectArrayKlassID, 32 bits),
        slotCtx.srcOopPtr + compressedSize,
        slotCtx.srcOopPtr + U(8)
      )

    io.ToCopy.DestOopPtr :=
      Mux(
        slotCtx.kid === U(ObjectArrayKlassID, 32 bits),
        slotCtx.destOopPtr + compressedSize,
        slotCtx.destOopPtr + U(8)
      )

    when(io.ToCopy.Ready) {
      slotCtx.copyIssued := True
      copyBusy := True
    }
  }

  // ==========================================================================
  // trace issue
  // ==========================================================================
  val wantTrace =
    slotValid &&
      slotIsSendWork &&
      !slotCtx.traceIssued &&
      slotCtx.kid =/= U(TypeArrayKlassID, 32 bits)

  when(!traceBusy && wantTrace) {
    io.ToTrace.Valid := True
    io.ToTrace.OopType := U(NotArrayOop)
    io.ToTrace.KlassPtr := slotCtx.klassPtr
    io.ToTrace.SrcOopPtr := slotCtx.srcOopPtr
    io.ToTrace.DestOopPtr := slotCtx.destOopPtr
    io.ToTrace.Kid := slotCtx.kid
    io.ToTrace.ScanningInYoung := slotCtx.destRegionAttr(15 downto 8) === U(0, 8 bits)
    io.ToTrace.ArrayLength := slotCtx.srcLength
    io.ToTrace.PartialArrayStart := U(0)
    io.ToTrace.StepIndex := (slotCtx.srcLength % io.ConfigIO.ChunkSize).resize(32)
    io.ToTrace.StepNCreate :=
      Mux(
        slotCtx.srcLength > (slotCtx.srcLength % io.ConfigIO.ChunkSize),
        U(1),
        U(0)
      ).resize(32)

    when(io.ToTrace.Ready) {
      slotCtx.traceIssued := True
      traceBusy := True
    }
  }

  // ==========================================================================
  // allocate issue
  // ==========================================================================
  val needAlloc =
    ((plabCacheEnd(allocIdx) - plabCacheTop(allocIdx)) / U(8)) < slotCtx.size

  val wantAlloc =
    slotValid &&
      slotIsAllocCache &&
      !slotCtx.allocIssued &&
      needAlloc

  when(!allocBusy && wantAlloc) {
    io.ToAllocate.Valid := True
    io.ToAllocate.Size := slotCtx.size
    io.ToAllocate.DestAttrType :=
      Mux(
        slotCtx.plabForceOld,
        U(1, 8 bits),
        slotCtx.destRegionAttr(15 downto 8)
      )

    when(io.ToAllocate.Ready) {
      slotCtx.allocIssued := True
      allocBusy := True
    }
  }
}

object GCOopCopy2SurvivorSingleVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}
