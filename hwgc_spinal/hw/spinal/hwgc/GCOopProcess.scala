package hwgc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class SlotCtx() extends Bundle with GCParameters {
  val task                 = UInt(GCElementWidth bits)
  val srcOopPtr            = UInt(GCElementWidth bits)
  val markWord             = UInt(GCElementWidth bits)
  val klassPtr             = UInt(GCElementWidth bits)
  val srcLength            = UInt(32 bits)

  val srcRegionAttr        = UInt(16 bits)
  val destOopPtr           = UInt(GCElementWidth bits)
  val heapRegion           = UInt(GCElementWidth bits)
  val heapRegionHumongous  = Bool()

  val fromMarkWord         = Bool()
  val accessDestRegionAttr = Bool()
  val destRegionAttr       = UInt(16 bits)
}

class GCOopProcess extends Module with HWParameters with GCParameters {
  val io = new Bundle {
    val Mreq0                = master(new LocalMMUIO)
    val Mreq1                = master(new LocalMMUIO)

    val Process2Aop          = master(new GCToAop)
    val Fetch2Process        = slave(new GCToProcessUnit)
    val Process2CopySurvivor = master(new GCToSurvivor)

    val ConfigIO             = slave(new GCOopProcessConfigIO)
    val DebugTimeStamp       = in UInt(64 bits)
  }

  // defaults
  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid := False
    m.Request.payload.clearAll()

    m.RequestSize.valid := False
    m.RequestSize.payload.clearAll()

    m.Response.ready := True
  }

  clearMreq(io.Mreq0)
  clearMreq(io.Mreq1)

  io.Process2CopySurvivor.clearIn()
  io.Process2Aop.clearIn()

  def slotMreq(i: Int): LocalMMUIO = if (i == 0) io.Mreq0 else io.Mreq1

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCOopProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  // slot registers
  val slotValid = Vec.fill(2)(RegInit(False))
  val slotCtx   = Vec.fill(2)(Reg(SlotCtx()) init (SlotCtx().getZero))

  // issueReq 内部使用的 per-slot request 状态
  val slotIssued = Vec.fill(2)(RegInit(False))

  // copy return bookkeeping
  val slotCopy2SurvivorDone          = Vec.fill(2)(RegInit(False))
  val slotCopy2SurvivorInflight      = Vec.fill(2)(RegInit(False))
  val slotCopy2SurvivorTypeArraySeen = Vec.fill(2)(RegInit(False))
  val slotCopy2SurvivorBypassGranted = Vec.fill(2)(RegInit(False))

  // admission credits
  val slot0Credit = RegInit(False)
  val slot1Credit = RegInit(False)

  // shared small caches 两个 slot 共用一份 cache。cache fill 写口采用固定优先级：slot0 > slot1。
  // 当slot0和slot1同周期都miss并且都要fill时，只fill slot0，slot1虽然也拿到数据了但是不更新cache。这样设计可以简化控制逻辑，避免同周期多次更新cache带来的复杂性
  val regionAttrCacheEntries = 8
  val regionAttrCacheValid   = Vec.fill(regionAttrCacheEntries)(RegInit(False))
  val regionAttrCacheTag     = Vec.fill(regionAttrCacheEntries)(RegInit(U(0, MMUAddrWidth bits)))
  val regionAttrCache        = Vec.fill(regionAttrCacheEntries)(RegInit(U(0, 16 bits)))
  val regionAttrCacheReplacePtr = RegInit(U(0, log2Up(regionAttrCacheEntries) bits))

  val heapRegionCacheEntries = 4
  val heapRegionCacheValid   = Vec.fill(heapRegionCacheEntries)(RegInit(False))
  val heapRegionCacheTag     = Vec.fill(heapRegionCacheEntries)(RegInit(U(0, MMUAddrWidth bits)))
  val heapRegionCache        = Vec.fill(heapRegionCacheEntries)(RegInit(False))
  val heapRegionCacheReplacePtr = RegInit(U(0, log2Up(heapRegionCacheEntries) bits))

  // helpers
  def regionAttrAddrOf(i: Int): UInt = (io.ConfigIO.RegionAttrBiasedBase + (slotCtx(i).srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth)

  def destRegionAttrAddrOf(i: Int): UInt = (io.ConfigIO.RegionAttrBiasedBase + (slotCtx(i).destOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth)

  def heapRegionLookupAddrOf(i: Int): UInt = (io.ConfigIO.HeapRegionBiasedBase + (slotCtx(i).task >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth)

  def writeBackObjOf(i: Int): UInt = {
    Mux(io.ConfigIO.UseCompressedOop,
      ((slotCtx(i).destOopPtr - io.ConfigIO.CompressedOopBase) >> io.ConfigIO.CompressedOopShift).resize(GCElementWidth),
      slotCtx(i).destOopPtr)
  }

  def writeBackSize(): UInt = Mux(io.ConfigIO.UseCompressedOop, U(4), U(8))

  // cross-FSM pulses
  val slotStart           = Vec.fill(2)(Bool())
  val slotGotoIdle        = Vec.fill(2)(Bool())
  val slotCopyReqAccepted = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    slotStart(i)           := False
    slotGotoIdle(i)        := False
    slotCopyReqAccepted(i) := False
  }

  def clearSlotRuntime(i: Int): Unit = {
    slotCtx(i).srcRegionAttr        := 0
    slotCtx(i).destOopPtr           := 0
    slotCtx(i).heapRegion           := 0
    slotCtx(i).heapRegionHumongous  := False
    slotCtx(i).fromMarkWord         := False
    slotCtx(i).accessDestRegionAttr := False
    slotCtx(i).destRegionAttr       := 0

    slotIssued(i) := False

    slotCopy2SurvivorDone(i)          := False
    slotCopy2SurvivorInflight(i)      := False
    slotCopy2SurvivorTypeArraySeen(i) := False
    slotCopy2SurvivorBypassGranted(i) := False
  }

  def allocToSlot(i: Int): Unit = {
    slotValid(i) := True

    slotCtx(i).task      := io.Fetch2Process.Task
    slotCtx(i).markWord  := io.Fetch2Process.MarkWord
    slotCtx(i).klassPtr  := io.Fetch2Process.KlassPtr
    slotCtx(i).srcOopPtr := io.Fetch2Process.SrcOopPtr
    slotCtx(i).srcLength := io.Fetch2Process.SrcLength

    clearSlotRuntime(i)
    slotStart(i) := True

    dbg(Seq("Allocate task to slot", i.toString, ", srcOopPtr=", io.Fetch2Process.SrcOopPtr))
  }

  def finishSlot(i: Int): Unit = {
    slotValid(i) := False
    clearSlotRuntime(i)
    slotGotoIdle(i) := True

    // clear the downstream admission token generated by this slot
    if (i == 0) {
      slot1Credit := False
    } else {
      slot0Credit := False
    }

    dbg(Seq("Finish slot", i.toString))
  }

  def givePeerCredit(i: Int): Unit = {
    if (i == 0) {
      slot1Credit := True
      dbg(Seq("slot0 gives credit to slot1"))
    } else {
      slot0Credit := True
      dbg(Seq("slot1 gives credit to slot0"))
    }
  }

  def givePeerCreditDyn(i: UInt): Unit = {
    when(i === U(0)) {
      slot1Credit := True
      dbg(Seq("slot0 gives credit to slot1"))
    } otherwise {
      slot0Credit := True
      dbg(Seq("slot1 gives credit to slot0"))
    }
  }

  // cache lookup wires
  val srcRegionAttrAddr      = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val destRegionAttrAddr     = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val heapRegionLookupAddr   = Vec.fill(2)(UInt(MMUAddrWidth bits))

  val srcRegionAttrHit       = Vec.fill(2)(Bool())
  val srcRegionAttrHitIndex  = Vec.fill(2)(UInt(log2Up(regionAttrCacheEntries) bits))

  val heapRegionHit          = Vec.fill(2)(Bool())
  val heapRegionHitIndex     = Vec.fill(2)(UInt(log2Up(heapRegionCacheEntries) bits))

  for (i <- 0 until 2) {
    srcRegionAttrAddr(i)    := regionAttrAddrOf(i)
    destRegionAttrAddr(i)   := destRegionAttrAddrOf(i)
    heapRegionLookupAddr(i) := heapRegionLookupAddrOf(i)

    val regionHitVec = Vec.fill(regionAttrCacheEntries)(Bool())
    for (j <- 0 until regionAttrCacheEntries) {
      regionHitVec(j) := regionAttrCacheValid(j) && regionAttrCacheTag(j) === srcRegionAttrAddr(i)
    }

    srcRegionAttrHit(i)      := regionHitVec.orR
    srcRegionAttrHitIndex(i) := OHToUInt(regionHitVec.asBits)

    val heapHitVec = Vec.fill(heapRegionCacheEntries)(Bool())
    for (j <- 0 until heapRegionCacheEntries) {
      heapHitVec(j) := heapRegionCacheValid(j) && heapRegionCacheTag(j) === heapRegionLookupAddr(i)
    }

    heapRegionHit(i)      := heapHitVec.orR
    heapRegionHitIndex(i) := OHToUInt(heapHitVec.asBits)
  }

  // shared cache fill requests
  val regionAttrFillValid = Vec.fill(2)(Bool())
  val regionAttrFillAddr  = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val regionAttrFillData  = Vec.fill(2)(UInt(16 bits))

  val heapRegionFillValid = Vec.fill(2)(Bool())
  val heapRegionFillAddr  = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val heapRegionFillData  = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    regionAttrFillValid(i) := False
    regionAttrFillAddr(i)  := U(0, MMUAddrWidth bits)
    regionAttrFillData(i)  := U(0, 16 bits)

    heapRegionFillValid(i) := False
    heapRegionFillAddr(i)  := U(0, MMUAddrWidth bits)
    heapRegionFillData(i)  := False
  }

  // Fetch done pulse sources
  val fetchDoneFromTypeArrayBypass  = Bool()
  val fetchDoneFromNegativeRegion   = Vec.fill(2)(Bool())
  val fetchDoneFromFromMarkWord     = Vec.fill(2)(Bool())
  val fetchDoneFromCopySurvivorDone = Vec.fill(2)(Bool())

  fetchDoneFromTypeArrayBypass := False

  for (i <- 0 until 2) {
    fetchDoneFromNegativeRegion(i)   := False
    fetchDoneFromFromMarkWord(i)     := False
    fetchDoneFromCopySurvivorDone(i) := False
  }

  when(io.Process2CopySurvivor.isTypeArray) {
    val isTypeArrayIdx = io.Process2CopySurvivor.DoneOwner

    when(slotCopy2SurvivorInflight(isTypeArrayIdx) && !slotCopy2SurvivorBypassGranted(isTypeArrayIdx)) {
      givePeerCreditDyn(isTypeArrayIdx)

      slotCopy2SurvivorBypassGranted(isTypeArrayIdx) := True
      fetchDoneFromTypeArrayBypass := True
    }
  }

  when(io.Process2CopySurvivor.Done) {
    val doneOwner = io.Process2CopySurvivor.DoneOwner

    slotCtx(doneOwner).destOopPtr := io.Process2CopySurvivor.DestOopPtr
    slotCopy2SurvivorDone(doneOwner) := True
    slotCopy2SurvivorInflight(doneOwner) := False

    dbg(Seq("Copy2Survivor done for slot", doneOwner))
  }

  // admission
  val pipeEmpty = !slotValid(0) && !slotValid(1)

  val canAllocSlot0 = !slotValid(0) && (pipeEmpty || (slotValid(1) && slot0Credit))

  val canAllocSlot1 = !slotValid(1) && slotValid(0) && slot1Credit

  io.Fetch2Process.Ready := canAllocSlot0 || canAllocSlot1
  io.Fetch2Process.Done := fetchDoneFromTypeArrayBypass || fetchDoneFromNegativeRegion.orR || fetchDoneFromFromMarkWord.orR || fetchDoneFromCopySurvivorDone.orR

  when(io.Fetch2Process.Valid && io.Fetch2Process.Ready) {
    when(canAllocSlot0) {
      allocToSlot(0)
      slot0Credit := False
    } elsewhen(canAllocSlot1) {
      allocToSlot(1)
      slot1Credit := False
    }
  }

  // slot FSM visibility for shared-output arbitration
  val slotIsCopyReq = Vec.fill(2)(Bool())
  val slotIsWaitAop = Vec.fill(2)(Bool())

  // Per-slot StateMachines
  for (i <- 0 until 2) {
    val m = slotMreq(i)

    val slotFsm = new StateMachine {
      val IDLE              = new State with EntryPoint
      val READ_SRC_ATTR     = new State
      val DECIDE            = new State
      val COPY_SURV_REQ     = new State
      val READ_HEAP_PTR     = new State
      val READ_HUMONGOUS    = new State
      val WAIT_COPY_OR_MARK = new State
      val WRITE_BACK        = new State
      val READ_DEST_ATTR    = new State
      val WAIT_AOP          = new State

      IDLE.whenIsActive {
        // wait for slotStart(i)
      }

      READ_SRC_ATTR.whenIsActive {
        when(srcRegionAttrHit(i)) {
          slotCtx(i).srcRegionAttr := regionAttrCache(srcRegionAttrHitIndex(i))
          goto(DECIDE)

        } otherwise {
          issueReq(m, srcRegionAttrAddr(i), False, U(2), U(0), slotIssued(i)) { rd =>
            slotCtx(i).srcRegionAttr := rd(15 downto 0)

            regionAttrFillValid(i) := True
            regionAttrFillAddr(i)  := srcRegionAttrAddr(i)
            regionAttrFillData(i)  := rd(15 downto 0)

            goto(DECIDE)
          }
        }
      }

      DECIDE.whenIsActive {
        val srcRegionAttrType = slotCtx(i).srcRegionAttr.asSInt

        when(srcRegionAttrType < 0) {
          fetchDoneFromNegativeRegion(i) := True
          finishSlot(i)

        } otherwise {
          val doCopy2Survivor = (slotCtx(i).markWord & U(3, GCElementWidth bits)) =/= U(3, GCElementWidth bits)

          when(!doCopy2Survivor) {
            slotCtx(i).destOopPtr   := slotCtx(i).markWord & ~U(3, GCElementWidth bits)
            slotCtx(i).fromMarkWord := True

            givePeerCredit(i)
            fetchDoneFromFromMarkWord(i) := True

            goto(READ_HEAP_PTR)

            dbg(Seq("slot", i.toString, " use fromMarkWord path"))

          } otherwise {
            slotCtx(i).fromMarkWord := False

            goto(COPY_SURV_REQ)

            dbg(Seq("slot", i.toString, " go copy2survivor path"))
          }
        }
      }

      COPY_SURV_REQ.whenIsActive {
        // request is driven by centralized Process2CopySurvivor arbitration
      }

      READ_HEAP_PTR.whenIsActive {
        when(heapRegionHit(i)) {
          slotCtx(i).heapRegionHumongous := heapRegionCache(heapRegionHitIndex(i))
          goto(WAIT_COPY_OR_MARK)

        } otherwise {
          issueReq(m, heapRegionLookupAddr(i), False, U(8), U(0), slotIssued(i)) { rd =>
            slotCtx(i).heapRegion := rd(GCElementWidth - 1 downto 0)
            goto(READ_HUMONGOUS)
          }
        }
      }

      READ_HUMONGOUS.whenIsActive {
        val humAddr = (slotCtx(i).heapRegion.resize(MMUAddrWidth) + U"xbc").resize(MMUAddrWidth)

        issueReq(m, humAddr, False, U(4), U(0), slotIssued(i)) { rd =>
          val hum = (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)

          slotCtx(i).heapRegionHumongous := hum

          heapRegionFillValid(i) := True
          heapRegionFillAddr(i)  := heapRegionLookupAddr(i)
          heapRegionFillData(i)  := hum

          goto(WAIT_COPY_OR_MARK)
        }
      }

      WAIT_COPY_OR_MARK.whenIsActive {
        when(slotCtx(i).fromMarkWord) {
          slotCtx(i).fromMarkWord := False
          goto(WRITE_BACK)

        } elsewhen(slotCopy2SurvivorDone(i)) {
          slotCopy2SurvivorDone(i) := False

          fetchDoneFromCopySurvivorDone(i) := !slotCopy2SurvivorBypassGranted(i)
          slotCopy2SurvivorBypassGranted(i) := False

          goto(WRITE_BACK)
        }
      }

      WRITE_BACK.whenIsActive {
        issueReq(m, slotCtx(i).task.resize(MMUAddrWidth), True, writeBackSize(), writeBackObjOf(i), slotIssued(i)) { _ =>
          // write response ignored, same behavior as original code
        }

        when(slotIssued(i)) {
          slotIssued(i) := False

          val sameRegion = ((slotCtx(i).task ^ slotCtx(i).destOopPtr) >> io.ConfigIO.LogOfHRGrainBytes) === U(0)

          when(sameRegion || slotCtx(i).heapRegionHumongous) {
            finishSlot(i)

          } otherwise {
            slotCtx(i).accessDestRegionAttr := False
            goto(READ_DEST_ATTR)
          }
        }
      }

      READ_DEST_ATTR.whenIsActive {
        issueReq(m, destRegionAttrAddr(i), False, U(2), U(0), slotIssued(i)) { rd =>
          slotCtx(i).accessDestRegionAttr := True
          slotCtx(i).destRegionAttr       := rd(15 downto 0)

          goto(WAIT_AOP)
        }
      }

      WAIT_AOP.whenIsActive {
        // request is driven by centralized AOP arbitration
      }

      always {
        when(slotGotoIdle(i)) {
          goto(IDLE)

        } elsewhen(slotStart(i)) {
          goto(READ_SRC_ATTR)

        } elsewhen(slotCopyReqAccepted(i)) {
          goto(READ_HEAP_PTR)
        }
      }
    }

    slotIsCopyReq(i) := slotFsm.isActive(slotFsm.COPY_SURV_REQ)
    slotIsWaitAop(i) := slotFsm.isActive(slotFsm.WAIT_AOP)
  }

  // ==========================================================================
  // Shared regionAttrCache fill
  // Fixed priority: slot0 > slot1
  // 同周期 slot0/slot1 都 fill 时，只 fill slot0。
  // slot1 当前返回数据已经写入 slotCtx(1)，只是这次不更新 cache。
  // ==========================================================================
  val grantRegionFill0 = regionAttrFillValid(0)
  val grantRegionFill1 = !regionAttrFillValid(0) && regionAttrFillValid(1)

  when(grantRegionFill0 || grantRegionFill1) {
    val fillAddr = Mux(grantRegionFill0, regionAttrFillAddr(0), regionAttrFillAddr(1))
    val fillData = Mux(grantRegionFill0, regionAttrFillData(0), regionAttrFillData(1))

    regionAttrCacheValid(regionAttrCacheReplacePtr) := True
    regionAttrCacheTag(regionAttrCacheReplacePtr)   := fillAddr
    regionAttrCache(regionAttrCacheReplacePtr)      := fillData

    regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + 1
  }

  // 同regionAttrCache
  val grantHeapFill0 = heapRegionFillValid(0)
  val grantHeapFill1 = !heapRegionFillValid(0) && heapRegionFillValid(1)

  when(grantHeapFill0 || grantHeapFill1) {
    val fillAddr = Mux(grantHeapFill0, heapRegionFillAddr(0), heapRegionFillAddr(1))
    val fillData = Mux(grantHeapFill0, heapRegionFillData(0), heapRegionFillData(1))

    heapRegionCacheValid(heapRegionCacheReplacePtr) := True
    heapRegionCacheTag(heapRegionCacheReplacePtr)   := fillAddr
    heapRegionCache(heapRegionCacheReplacePtr)      := fillData

    heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + 1
  }

  // ==========================================================================
  // Centralized Copy2Survivor arbitration
  // fixed priority: slot0 > slot1
  // ==========================================================================
  val slotWantCopySurvivor = Vec.fill(2)(Bool())

  slotWantCopySurvivor(0) := slotValid(0) && slotIsCopyReq(0) && !slotCopy2SurvivorInflight(0)
  slotWantCopySurvivor(1) := slotValid(1) && slotIsCopyReq(1) && !slotCopy2SurvivorInflight(1)

  val grantCopy0 = slotWantCopySurvivor(0)
  val grantCopy1 = !slotWantCopySurvivor(0) && slotWantCopySurvivor(1)

  when(grantCopy0 || grantCopy1) {
    io.Process2CopySurvivor.Valid := True
    io.Process2CopySurvivor.Owner := Mux(grantCopy0, U(0), U(1))
    io.Process2CopySurvivor.MarkWord := Mux(grantCopy0, slotCtx(0).markWord, slotCtx(1).markWord)
    io.Process2CopySurvivor.KlassPtr := Mux(grantCopy0, slotCtx(0).klassPtr, slotCtx(1).klassPtr)
    io.Process2CopySurvivor.SrcOopPtr := Mux(grantCopy0, slotCtx(0).srcOopPtr, slotCtx(1).srcOopPtr)
    io.Process2CopySurvivor.SrcLength := Mux(grantCopy0, slotCtx(0).srcLength, slotCtx(1).srcLength)
    io.Process2CopySurvivor.SrcRegionAttr := Mux(grantCopy0, slotCtx(0).srcRegionAttr, slotCtx(1).srcRegionAttr)
    io.Process2CopySurvivor.RegionAttrPtr := Mux(grantCopy0, srcRegionAttrAddr(0).resize(GCElementWidth), srcRegionAttrAddr(1).resize(GCElementWidth))

    when(io.Process2CopySurvivor.Ready) {
      when(grantCopy0) {
        slotCopy2SurvivorInflight(0)      := True
        slotCopy2SurvivorTypeArraySeen(0) := False
        slotCopy2SurvivorBypassGranted(0) := False
        slotCopyReqAccepted(0) := True
      }

      when(grantCopy1) {
        slotCopy2SurvivorInflight(1)      := True
        slotCopy2SurvivorTypeArraySeen(1) := False
        slotCopy2SurvivorBypassGranted(1) := False
        slotCopyReqAccepted(1) := True
      }
    }
  }

  // Centralized AOP arbitration fixed priority: slot0 > slot1
  val slotWantAop = Vec.fill(2)(Bool())

  slotWantAop(0) := slotValid(0) && slotIsWaitAop(0) && slotCtx(0).accessDestRegionAttr
  slotWantAop(1) := slotValid(1) && slotIsWaitAop(1) && slotCtx(1).accessDestRegionAttr

  val grantAop0 = slotWantAop(0)
  val grantAop1 = !slotWantAop(0) && slotWantAop(1)

  when(grantAop0 || grantAop1) {
    io.Process2Aop.Valid := True
    io.Process2Aop.Task := Mux(grantAop0, slotCtx(0).task, slotCtx(1).task)
    io.Process2Aop.RegionAttr := Mux(grantAop0, slotCtx(0).destRegionAttr, slotCtx(1).destRegionAttr)

    when(io.Process2Aop.Ready) {
      when(grantAop0) {
        finishSlot(0)
      }
      when(grantAop1) {
        finishSlot(1)
      }
    }
  }
}

object GCOopProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCOopProcess())
}