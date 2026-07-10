package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, MyStateMachine}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class SlotCtx() extends Bundle with GCTopParameters {
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

class GCOopProcess extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val Mreq0                = master(new LocalMMUIO)
    val Mreq1                = master(new LocalMMUIO)

    val Process2Aop          = master(new GCToAop)
    val Fetch2Process        = slave(new GCToProcessUnit)
    val Process2CopySurvivor = master(new GCToSurvivor)

    val ConfigIO             = slave(new GCOopProcessConfigIO)
    val DebugTimeStamp       = in UInt(64 bits)

    val SlotIsEmpty          = out Bool()
  }

  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid := False
    m.Request.payload.clearAll()
    m.Response.ready := True
  }

  clearMreq(io.Mreq0)
  clearMreq(io.Mreq1)

  io.Process2CopySurvivor.clearOut()
  io.Process2Aop.clearOut()

  // slot 状态 放前面 不然 会报错NullPointer
  val slotValid = Vec.fill(2)(RegInit(False))
  val slotCtx   = Vec.fill(2)(Reg(SlotCtx()) init SlotCtx().getZero)

  val slotStart           = Vec.fill(2)(Bool())
  val slotGotoIdle        = Vec.fill(2)(Bool())
  val slotCopyReqAccepted = Vec.fill(2)(Bool())
  val slotReleaseFetch    = Vec.fill(2)(Bool()) // 当前Slot 允许前级 Fetch 释放任务
  // 这个默认赋值放条件赋值前面 不然会 和条件 冲突 ASSIGN OVERFLEAP
  for (i <- 0 until 2) {
    slotStart(i)           := False
    slotGotoIdle(i)        := False
    slotCopyReqAccepted(i) := False
    slotReleaseFetch(i)    := False
  }


  def slotMreq(i: Int): LocalMMUIO = if (i == 0) io.Mreq0 else io.Mreq1
  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCOopProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  def regionAttrAddrOf(i: Int): UInt = (io.ConfigIO.RegionAttrBiasedBase + (slotCtx(i).srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) << U(1)).resize(MMUAddrWidth)
  def destRegionAttrAddrOf(i: Int): UInt = (io.ConfigIO.RegionAttrBiasedBase + (slotCtx(i).destOopPtr >> io.ConfigIO.RegionAttrShiftBy) << U(1)).resize(MMUAddrWidth)
  def heapRegionLookupAddrOf(i: Int): UInt = (io.ConfigIO.HeapRegionBiasedBase + (slotCtx(i).task >> io.ConfigIO.HeapRegionShiftBy) << U(3)).resize(MMUAddrWidth)
  def writeBackObjOf(i: Int): UInt = {
    Mux(io.ConfigIO.UseCompressedOop,
      ((slotCtx(i).destOopPtr - io.ConfigIO.CompressedOopBase) >> io.ConfigIO.CompressedOopShift).resize(GCElementWidth),
      slotCtx(i).destOopPtr)
  }
  def writeBackSize(): UInt = Mux(io.ConfigIO.UseCompressedOop, U(4), U(8))
  def clearSlotRuntime(i: Int): Unit = {
    slotCtx(i).srcRegionAttr        := 0
    slotCtx(i).destOopPtr           := 0
    slotCtx(i).heapRegion           := 0
    slotCtx(i).heapRegionHumongous  := False
    slotCtx(i).fromMarkWord         := False
    slotCtx(i).accessDestRegionAttr := False
    slotCtx(i).destRegionAttr       := 0

    slotCopy2SurvivorDone(i)          := False
    slotCopy2SurvivorInflight(i)      := False
    slotCopy2SurvivorBypassGranted(i) := False
  }
  def allocToSlot(i: Int): Unit = {
    slotValid(i) := True

    slotCtx(i).task      := io.Fetch2Process.cmd.payload.Task
    slotCtx(i).markWord  := io.Fetch2Process.cmd.payload.MarkWord
    slotCtx(i).klassPtr  := io.Fetch2Process.cmd.payload.KlassPtr
    slotCtx(i).srcOopPtr := io.Fetch2Process.cmd.payload.SrcOopPtr
    slotCtx(i).srcLength := io.Fetch2Process.cmd.payload.SrcLength

    clearSlotRuntime(i)
    slotStart(i) := True

    dbg(Seq("Allocate task to slot", i.toString, ", srcOopPtr=", io.Fetch2Process.cmd.payload.SrcOopPtr))
  }
  def finishSlot(i: Int): Unit = {
    slotValid(i) := False
    clearSlotRuntime(i)
    slotGotoIdle(i) := True

    dbg(Seq("Finish slot", i.toString))
  }
  def releaseFetchFromSlotDyn(i: UInt): Unit = {
    when(i === U(0)) {
      slotReleaseFetch(0) := True
    } otherwise {
      slotReleaseFetch(1) := True
    }
  }

  // shared small caches
  // 两个 slot 共用一份 cache。 cache fill 写口采用固定优先级：slot0 > slot1。
  // 当 slot0 和 slot1 同周期都 miss 且都要 fill 时，只 fill slot0。 slot1 的当前返回数据已经写入 slotCtx(1)，只是这次不更新 cache。
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

  val srcRegionAttrAddr     = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val destRegionAttrAddr    = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val heapRegionLookupAddr  = Vec.fill(2)(UInt(MMUAddrWidth bits))
  val srcRegionAttrHit      = Vec.fill(2)(Bool())
  val srcRegionAttrHitIndex = Vec.fill(2)(UInt(log2Up(regionAttrCacheEntries) bits))

  val heapRegionHit         = Vec.fill(2)(Bool())
  val heapRegionHitIndex    = Vec.fill(2)(UInt(log2Up(heapRegionCacheEntries) bits))

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

  val slotCopy2SurvivorDone          = Vec.fill(2)(RegInit(False)) // CopySurvivor 已经返回 最终的 destOopPtr
  val slotCopy2SurvivorInflight      = Vec.fill(2)(RegInit(False)) // 已经向 CopySurvivor 发出请求，正在等待返回
  val slotCopy2SurvivorBypassGranted = Vec.fill(2)(RegInit(False)) // 某些 type array 可以提前释放 Fetch，避免阻塞前级

  // allowSecondInFlight = True 表示已经有某个 slot 提前对 Fetch 发过 Done， 因此允许 Fetch 再送一个任务进入另一个空 slot
  val allowSecondInFlight = RegInit(False)

  val pipeEmpty = !slotValid.orR
  val hasFreeSlot = !slotValid.andR
  val fetchReleasePulse = slotReleaseFetch.orR
  val fetchAccept = io.Fetch2Process.cmd.fire

  // 如果两个 slot 都空，直接 ready; 如果 pipeline 非空, 必须 allowSecondInFlight=True， 才允许 Fetch 再发一个任务进来
  io.SlotIsEmpty := pipeEmpty
  io.Fetch2Process.cmd.ready := hasFreeSlot && (pipeEmpty || allowSecondInFlight)
  io.Fetch2Process.Done := fetchReleasePulse

  // Process2CopySurvivor isTypeArray capture (不需要复制 可以提前释放)
  when(io.Process2CopySurvivor.done.payload.isTypeArray) {
    val isTypeArrayIdx = io.Process2CopySurvivor.done.payload.DoneOwner

    when(slotCopy2SurvivorInflight(isTypeArrayIdx) && !slotCopy2SurvivorBypassGranted(isTypeArrayIdx)) {
      releaseFetchFromSlotDyn(isTypeArrayIdx)
      slotCopy2SurvivorBypassGranted(isTypeArrayIdx) := True
    }
  }

  // Process2CopySurvivor Done capture
  when(io.Process2CopySurvivor.done.valid) {
    val doneOwner = io.Process2CopySurvivor.done.payload.DoneOwner

    slotCtx(doneOwner).destOopPtr := io.Process2CopySurvivor.done.payload.DestOopPtr
    slotCopy2SurvivorDone(doneOwner) := True
    slotCopy2SurvivorInflight(doneOwner) := False

    dbg(Seq("Copy2Survivor done for slot", doneOwner))
  }


  when(fetchAccept) {
    when(!slotValid(0)) {
      allocToSlot(0)
    } otherwise {
      allocToSlot(1)
    }
  }

  // allowSecondInFlight 的集中更新 (应该不会有同周期的 fetchReleasePulse 和 fetchAccept 均有效)
  // 如果本周期有 slotReleaseFetch，则允许 Fetch 后续再发一个任务
  // 如果本周期只是 Fetch 被接收，则消耗这个 token
  when(fetchReleasePulse) {
    allowSecondInFlight := True
  } elsewhen fetchAccept {
    allowSecondInFlight := False
  }

  // slot FSM visibility for shared-output arbitration
  val slotIsCopyReq = Vec.fill(2)(Bool())
  val slotIsWaitAop = Vec.fill(2)(Bool())

  for (i <- 0 until 2) {
    val m = slotMreq(i)

    val slotFsm = new MyStateMachine {
      val IDLE              = new State with EntryPoint
      val READ_SRC_ATTR     = new State
      val DECIDE            = new State
      val COPY_SURV_REQ     = new State
      val READ_HEAP_PTR     = new State
      val READ_HUMONGOUS    = new State
      val WAIT_COPY_OR_MARK = new State
      val WRITE_BACK        = new State
      val READ_DEST_ATTR    = new State
      val SEND_AOP          = new State

      always {
        when(slotGotoIdle(i)) {
          goto(IDLE)

        }.elsewhen(slotStart(i)) {
          goto(READ_SRC_ATTR)

        }.elsewhen(slotCopyReqAccepted(i)) {
          goto(READ_HEAP_PTR)
        }
      }

      READ_SRC_ATTR.whenIsActive {
        when(srcRegionAttrHit(i)) {
          slotCtx(i).srcRegionAttr := regionAttrCache(srcRegionAttrHitIndex(i))
          goto(DECIDE)

        } otherwise {
          issueDirectRead(m, srcRegionAttrAddr(i), U(2), DECIDE) { rd =>
            slotCtx(i).srcRegionAttr := rd(15 downto 0)

            regionAttrFillValid(i) := True
            regionAttrFillAddr(i)  := srcRegionAttrAddr(i)
            regionAttrFillData(i)  := rd(15 downto 0)
          }
        }
      }

      DECIDE.whenIsActive {
        val srcRegionAttrType = slotCtx(i).srcRegionAttr(15 downto 8).asSInt

        when(srcRegionAttrType < S(0, 8 bits)) {
          releaseFetchFromSlotDyn(i)
          finishSlot(i)

        } otherwise {
          val doCopy2Survivor = (slotCtx(i).markWord & U(3, GCElementWidth bits)) =/= U(3, GCElementWidth bits)

          when(!doCopy2Survivor) { // 不需要复制 可以提前结束
            slotCtx(i).destOopPtr   := slotCtx(i).markWord & ~U(3, GCElementWidth bits)
            slotCtx(i).fromMarkWord := True

            releaseFetchFromSlotDyn(i)

            goto(WRITE_BACK)

            dbg(Seq("slot", i.toString, " use fromMarkWord path"))

          }.otherwise {
            slotCtx(i).fromMarkWord := False

            goto(COPY_SURV_REQ)

            dbg(Seq("slot", i.toString, " go copy2survivor path"))
          }
        }
      }

      READ_HEAP_PTR.whenIsActive {
        when(heapRegionHit(i)) {
          slotCtx(i).heapRegionHumongous := heapRegionCache(heapRegionHitIndex(i))
          when(slotCtx(i).fromMarkWord) {
            slotCtx(i).fromMarkWord := False
            when(heapRegionCache(heapRegionHitIndex(i))){ // 已经Release了 这里 Finish就可以
              finishSlot(i)
            }.otherwise{
              slotCtx(i).accessDestRegionAttr := False
              goto(READ_DEST_ATTR)
            }
          }.otherwise{ goto(WAIT_COPY_OR_MARK) }

        } otherwise {
          issueDirectRead(m, heapRegionLookupAddr(i), U(8), READ_HUMONGOUS) { rd =>
            slotCtx(i).heapRegion := rd(GCElementWidth - 1 downto 0)
          }
        }
      }

      READ_HUMONGOUS.whenIsActive {
        val humAddr = (slotCtx(i).heapRegion.resize(MMUAddrWidth) + U"xbc").resize(MMUAddrWidth)

        issueReq(m, humAddr, False, U(4), U(0), True, False, issued) { rd =>
          val hum = (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)

          slotCtx(i).heapRegionHumongous := hum

          heapRegionFillValid(i) := True
          heapRegionFillAddr(i)  := heapRegionLookupAddr(i)
          heapRegionFillData(i)  := hum

          when(slotCtx(i).fromMarkWord) {
            slotCtx(i).fromMarkWord := False
            when(hum){
              finishSlot(i)
            }.otherwise{
              slotCtx(i).accessDestRegionAttr := False
              goto(READ_DEST_ATTR)
            }
          }.otherwise{ goto(WAIT_COPY_OR_MARK) }
        }
      }

      WAIT_COPY_OR_MARK.whenIsActive {
        when (slotCopy2SurvivorDone(i)) {
          val needRelease = !slotCopy2SurvivorBypassGranted(i)

          when(needRelease) { // 没有提前因为TypeArray Bypass release掉
            releaseFetchFromSlotDyn(i)
          }

          slotCopy2SurvivorDone(i) := False
          slotCopy2SurvivorBypassGranted(i) := False

          goto(WRITE_BACK)
        }
      }

      WRITE_BACK.whenIsActive {
        issueReq(m, slotCtx(i).task.resize(MMUAddrWidth), True, writeBackSize(), writeBackObjOf(i), False, False, issued) { _ =>}

        when(issued) {
          issued := False

          val sameRegion = ((slotCtx(i).task ^ slotCtx(i).destOopPtr) >> io.ConfigIO.LogOfHRGrainBytes) === U(0)

          when(slotCtx(i).fromMarkWord){
            when(sameRegion){
              finishSlot(i)
            }.otherwise{
              goto(READ_HEAP_PTR)
            }

          }.otherwise {
            when(sameRegion || slotCtx(i).heapRegionHumongous) {
              finishSlot(i)

            } otherwise {
              slotCtx(i).accessDestRegionAttr := False
              goto(READ_DEST_ATTR)
            }
          }
        }
      }

      READ_DEST_ATTR.whenIsActive {
        issueDirectRead(m, destRegionAttrAddr(i), U(2), SEND_AOP) { rd =>
          slotCtx(i).accessDestRegionAttr := True
          slotCtx(i).destRegionAttr       := rd(15 downto 0)
        }
      }
    }

    slotIsCopyReq(i) := slotFsm.isActive(slotFsm.COPY_SURV_REQ)
    slotIsWaitAop(i) := slotFsm.isActive(slotFsm.SEND_AOP)
  }

  // Shared regionAttrCache fill Fixed priority: slot0 > slot1
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

  // Shared heapRegionCache fill Fixed priority: slot0 > slot1
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

  // Centralized Copy2Survivor arbitration fixed priority: slot0 > slot1
  val slotWantCopySurvivor = Vec.fill(2)(Bool())

  slotWantCopySurvivor(0) := slotValid(0) && slotIsCopyReq(0) && !slotCopy2SurvivorInflight(0)
  slotWantCopySurvivor(1) := slotValid(1) && slotIsCopyReq(1) && !slotCopy2SurvivorInflight(1)

  val grantCopy0 = slotWantCopySurvivor(0)
  val grantCopy1 = !slotWantCopySurvivor(0) && slotWantCopySurvivor(1)

  when(grantCopy0 || grantCopy1) {
    io.Process2CopySurvivor.cmd.valid := True
    io.Process2CopySurvivor.cmd.payload.Owner := Mux(grantCopy0, U(0, 1 bits), U(1, 1 bits))
    io.Process2CopySurvivor.cmd.payload.MarkWord := Mux(grantCopy0, slotCtx(0).markWord, slotCtx(1).markWord)
    io.Process2CopySurvivor.cmd.payload.KlassPtr := Mux(grantCopy0, slotCtx(0).klassPtr, slotCtx(1).klassPtr)
    io.Process2CopySurvivor.cmd.payload.SrcOopPtr := Mux(grantCopy0, slotCtx(0).srcOopPtr, slotCtx(1).srcOopPtr)
    io.Process2CopySurvivor.cmd.payload.SrcLength := Mux(grantCopy0, slotCtx(0).srcLength, slotCtx(1).srcLength)
    io.Process2CopySurvivor.cmd.payload.SrcRegionAttr := Mux(grantCopy0, slotCtx(0).srcRegionAttr, slotCtx(1).srcRegionAttr)
    io.Process2CopySurvivor.cmd.payload.RegionAttrPtr := Mux(grantCopy0,
        srcRegionAttrAddr(0).resize(GCElementWidth),
        srcRegionAttrAddr(1).resize(GCElementWidth))

    when(io.Process2CopySurvivor.cmd.fire) {
      when(grantCopy0) {
        slotCopy2SurvivorInflight(0)      := True
        slotCopy2SurvivorBypassGranted(0) := False
        slotCopyReqAccepted(0) := True
      }

      when(grantCopy1) {
        slotCopy2SurvivorInflight(1)      := True
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
    io.Process2Aop.cmd.valid := True
    io.Process2Aop.cmd.payload.Task := Mux(grantAop0, slotCtx(0).task, slotCtx(1).task)
    io.Process2Aop.cmd.payload.RegionAttr := Mux(grantAop0, slotCtx(0).destRegionAttr, slotCtx(1).destRegionAttr)

    when(io.Process2Aop.cmd.fire) {
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