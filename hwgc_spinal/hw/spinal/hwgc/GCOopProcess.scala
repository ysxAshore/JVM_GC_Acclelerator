package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class SlotCtx() extends Bundle with GCParameters {
  val task = UInt(GCElementWidth bits)
  val srcOopPtr = UInt(GCElementWidth bits)
  val markWord = UInt(GCElementWidth bits)
  val klassPtr = UInt(GCElementWidth bits)
  val srcLength = UInt(32 bits)

  val srcRegionAttr = UInt(16 bits)
  val destOopPtr = UInt(GCElementWidth bits)
  val heapRegion = UInt(GCElementWidth bits)
  val heapRegionHumongous = Bool()

  val fromMarkWord = Bool()
  val accessDestRegionAttr = Bool()
  val destRegionAttr = UInt(16 bits)
}

class GCOopProcess extends Module with HWParameters with GCParameters {
  val io = new Bundle {
    val Mreq = master(new LocalMMUIO)
    val Process2Aop = master(new GCToAop)
    val Fetch2Process = slave(new GCToProcessUnit)
    val Process2CopySurvivor = master(new GCToSurvivor)
    val ConfigIO = slave(new GCOopProcessConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // --------------------------------------------------------------------------
  // defaults
  // --------------------------------------------------------------------------
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := True

  io.Process2CopySurvivor.clearIn()
  io.Process2Aop.clearIn()

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCOopProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  // --------------------------------------------------------------------------
  // state
  // --------------------------------------------------------------------------
  object overall_state extends SpinalEnum {
    val states = Array.tabulate(9)(_ => newElement())
    for ((state, i) <- states.zipWithIndex) {
      state.setName(s"s$i")
    }
  }

  object MmuOp extends SpinalEnum {
    val NONE, READ_SRC_ATTR, READ_HEAP_PTR, READ_HUMONGOUS, WRITE_BACK, READ_DEST_ATTR = newElement()
  }

  // --------------------------------------------------------------------------
  // two complete slots
  // --------------------------------------------------------------------------
  val slotValid = Vec(RegInit(False), 2)
  val slotState = Vec(RegInit(overall_state.states(0)), 2)
  val slotCtx = Vec.fill(2)(Reg(SlotCtx()) init (SlotCtx().getZero))

  // per-slot issueReq state
  val slotIssued = Vec(RegInit(False), 2)

  // copy return bookkeeping
  val slotCopy2SurvivorDone = Vec(RegInit(False), 2)

  // admission credits
  // slot0 can accept iff:
  //   slot0 empty && (pipeEmpty || (slot1 valid && slot0Credit))
  // slot1 can accept iff:
  //   slot1 empty && slot0 valid && slot1Credit
  val slot0Credit = RegInit(False)
  val slot1Credit = RegInit(False)

  val copy2SurvivorBusy = RegInit(False)
  val copy2SurvivorOwner = Reg(UInt(1 bits)) init(0)

  val mmuBusy = RegInit(False)
  val mmuOwner = Reg(UInt(1 bits)) init(0)
  val mmuOp = RegInit(MmuOp.NONE)

  val regionAttrCacheEntries = 8
  val regionAttrCacheValid = Vec(RegInit(False), regionAttrCacheEntries)
  val regionAttrCacheTag = Vec(RegInit(U(0, MMUAddrWidth bits)), regionAttrCacheEntries)
  val regionAttrCache = Vec(RegInit(U(0, 16 bits)), regionAttrCacheEntries)
  val regionAttrCacheReplacePtr = RegInit(U(0, log2Up(regionAttrCacheEntries) bits))

  val heapRegionCacheEntries = 4
  val heapRegionCacheValid = Vec(RegInit(False), heapRegionCacheEntries)
  val heapRegionCacheTag = Vec(RegInit(U(0, MMUAddrWidth bits)), heapRegionCacheEntries)
  val heapRegionCache = Vec(RegInit(False), heapRegionCacheEntries) // humongous flag
  val heapRegionCacheReplacePtr = RegInit(U(0, log2Up(heapRegionCacheEntries) bits))

  def regionAttrAddrOf(i: Int): UInt = (io.ConfigIO.RegionAttrBiasedBase + (slotCtx(i).srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth)

  def destRegionAttrAddrOf(i: Int): UInt = (io.ConfigIO.RegionAttrBiasedBase + (slotCtx(i).destOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth)

  def heapRegionLookupAddrOf(i: Int): UInt = (io.ConfigIO.HeapRegionBiasedBase + (slotCtx(i).task >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth)

  def writeBackObjOf(i: UInt): UInt =
    Mux(io.ConfigIO.UseCompressedOop, ((slotCtx(i).destOopPtr - io.ConfigIO.CompressedOopBase) >> io.ConfigIO.CompressedOopShift).resize(GCElementWidth), slotCtx(i).destOopPtr)

  def writeBackSize(): UInt = Mux(io.ConfigIO.UseCompressedOop, U(4), U(8))

  def clearSlotRuntime(i: Int): Unit = {
    slotCtx(i).srcRegionAttr := 0
    slotCtx(i).destOopPtr := 0
    slotCtx(i).heapRegion := 0
    slotCtx(i).heapRegionHumongous := False
    slotCtx(i).fromMarkWord := False
    slotCtx(i).accessDestRegionAttr := False
    slotCtx(i).destRegionAttr := 0
    slotIssued(i) := False
    slotCopy2SurvivorDone(i) := False
  }

  def allocToSlot(i: Int): Unit = {
    slotValid(i) := True
    slotState(i) := overall_state.states(1)

    slotCtx(i).task := io.Fetch2Process.Task
    slotCtx(i).markWord := io.Fetch2Process.MarkWord
    slotCtx(i).klassPtr := io.Fetch2Process.KlassPtr
    slotCtx(i).srcOopPtr := io.Fetch2Process.SrcOopPtr
    slotCtx(i).srcLength := io.Fetch2Process.SrcLength

    clearSlotRuntime(i)

    dbg(Seq("Allocate task to slot", i.toString, ", srcOopPtr=", io.Fetch2Process.SrcOopPtr))
  }

  def finishSlot(i: Int): Unit = {
    slotValid(i) := False
    slotState(i) := overall_state.states(0)
    clearSlotRuntime(i)

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
      dbg(Seq("slot0 hits fromMarkWord, give credit to slot1"))
    } else {
      slot0Credit := True
      dbg(Seq("slot1 hits fromMarkWord, give credit to slot0"))
    }
  }

  val srcRegionAttrAddr = Vec(UInt(MMUAddrWidth bits), 2)
  val destRegionAttrAddr = Vec(UInt(MMUAddrWidth bits), 2)
  val heapRegionLookupAddr = Vec(UInt(MMUAddrWidth bits), 2)

  val srcRegionAttrHit = Vec(Bool(), 2)
  val srcRegionAttrHitIndex = Vec(UInt(log2Up(regionAttrCacheEntries) bits), 2)

  val heapRegionHit = Vec(Bool(), 2)
  val heapRegionHitIndex = Vec(UInt(log2Up(heapRegionCacheEntries) bits), 2)

  for (i <- 0 until 2) {
    srcRegionAttrAddr(i) := regionAttrAddrOf(i)
    destRegionAttrAddr(i) := destRegionAttrAddrOf(i)
    heapRegionLookupAddr(i) := heapRegionLookupAddrOf(i)

    val regionAttrHitVec = Vec(Bool(), regionAttrCacheEntries)
    for (j <- 0 until regionAttrCacheEntries) {
      regionAttrHitVec(j) := regionAttrCacheValid(j) && (regionAttrCacheTag(j) === srcRegionAttrAddr(i))
    }
    srcRegionAttrHit(i) := regionAttrHitVec.orR
    srcRegionAttrHitIndex(i) := OHToUInt(regionAttrHitVec.asBits)

    val heapRegionHitVec = Vec(Bool(), heapRegionCacheEntries)
    for (j <- 0 until heapRegionCacheEntries) {
      heapRegionHitVec(j) := heapRegionCacheValid(j) && (heapRegionCacheTag(j) === heapRegionLookupAddr(i))
    }
    heapRegionHit(i) := heapRegionHitVec.orR
    heapRegionHitIndex(i) := OHToUInt(heapRegionHitVec.asBits)
  }

  when(copy2SurvivorBusy && io.Process2CopySurvivor.Done) {
    slotCtx(copy2SurvivorOwner).destOopPtr := io.Process2CopySurvivor.DestOopPtr
    slotCopy2SurvivorDone(copy2SurvivorOwner) := True
    copy2SurvivorBusy := False
    dbg(Seq("Copy2Survivor done for slot", copy2SurvivorOwner))
  }

  val pipeEmpty = !slotValid(0) && !slotValid(1)

  val canAllocSlot0 = !slotValid(0) && (pipeEmpty || (slotValid(1) && slot0Credit))

  val canAllocSlot1 = !slotValid(1) && slotValid(0) && slot1Credit

  io.Fetch2Process.Ready := canAllocSlot0 || canAllocSlot1
  io.Fetch2Process.Done := False

  when(io.Fetch2Process.Valid && io.Fetch2Process.Ready) {
    when(canAllocSlot0) {
      allocToSlot(0)
      slot0Credit := False
    } elsewhen (canAllocSlot1) {
      allocToSlot(1)
      slot1Credit := False
    }
  }

  // --------------------------------------------------------------------------
  // local, non-shared transitions in each slot
  // --------------------------------------------------------------------------
  for (i <- 0 until 2) {
    when(slotValid(i)) {
      switch(slotState(i)) {
        // s1 : read src region attr
        is(overall_state.states(1)) {
          when(srcRegionAttrHit(i)) {
            slotCtx(i).srcRegionAttr := regionAttrCache(srcRegionAttrHitIndex(i))
            slotState(i) := overall_state.states(2)
          }
        }

        // s2 : decide path
        is(overall_state.states(2)) {
          val srcRegionAttrType = slotCtx(i).srcRegionAttr.asSInt
          when(srcRegionAttrType < 0) {
            finishSlot(i)
            io.Fetch2Process.Done := True
          } otherwise {
            val doCopy2Survivor = (slotCtx(i).markWord & U(3, GCElementWidth bits)) =/= U(3, GCElementWidth bits)

            when(!doCopy2Survivor) {
              slotCtx(i).destOopPtr := slotCtx(i).markWord & ~U(3, GCElementWidth bits)
              slotCtx(i).fromMarkWord := True
              givePeerCredit(i)
              slotState(i) := overall_state.states(4)
              dbg(Seq("slot", i.toString, " use fromMarkWord path"))
            } otherwise {
              slotCtx(i).fromMarkWord := False
              slotState(i) := overall_state.states(3)
              dbg(Seq("slot", i.toString, " go copy2survivor path"))
            }
          }
        }

        // s4 : heap region lookup
        is(overall_state.states(4)) {
          when(heapRegionHit(i)) {
            slotCtx(i).heapRegionHumongous := heapRegionCache(heapRegionHitIndex(i))
            slotState(i) := overall_state.states(6)
          }
        }

        is(overall_state.states(6)) {
          when(slotCtx(i).fromMarkWord) {
            slotCtx(i).fromMarkWord := False
            slotState(i) := overall_state.states(7)
            io.Fetch2Process.Done := True
          } elsewhen slotCopy2SurvivorDone(i) {
            slotCopy2SurvivorDone(i) := False
            slotState(i) := overall_state.states(7)
            io.Fetch2Process.Done := True
          }
        }
      }
    }
  }

  // centralized copy2survivor arbitration (fixed priority: slot0 > slot1)
  val slotWantCopy2SurvivorReq = Vec(Bool(), 2)
  slotWantCopy2SurvivorReq(0) := slotValid(0) && (slotState(0) === overall_state.states(3)) && !copy2SurvivorBusy
  slotWantCopy2SurvivorReq(1) := slotValid(1) && (slotState(1) === overall_state.states(3)) && !copy2SurvivorBusy

  val grantCopy0 = slotWantCopy2SurvivorReq(0)
  val grantCopy1 = !slotWantCopy2SurvivorReq(0) && slotWantCopy2SurvivorReq(1)

  when(grantCopy0 || grantCopy1) {
    io.Process2CopySurvivor.Valid := True
    io.Process2CopySurvivor.MarkWord := Mux(grantCopy0, slotCtx(0).markWord, slotCtx(1).markWord)
    io.Process2CopySurvivor.KlassPtr := Mux(grantCopy0, slotCtx(0).klassPtr, slotCtx(1).klassPtr)
    io.Process2CopySurvivor.SrcOopPtr := Mux(grantCopy0, slotCtx(0).srcOopPtr, slotCtx(1).srcOopPtr)
    io.Process2CopySurvivor.SrcLength := Mux(grantCopy0, slotCtx(0).srcLength, slotCtx(1).srcLength)
    io.Process2CopySurvivor.SrcRegionAttr := Mux(grantCopy0, slotCtx(0).srcRegionAttr, slotCtx(1).srcRegionAttr)
    io.Process2CopySurvivor.RegionAttrPtr := Mux(grantCopy0, srcRegionAttrAddr(0).resize(GCElementWidth), srcRegionAttrAddr(1).resize(GCElementWidth))

    when(io.Process2CopySurvivor.Ready) {
      copy2SurvivorBusy := True

      when(grantCopy0) {
        copy2SurvivorOwner := U(0, 1 bits)
        slotState(0) := overall_state.states(4)
      }
      when(grantCopy1) {
        copy2SurvivorOwner := U(1, 1 bits)
        slotState(1) := overall_state.states(4)
      }
    }
  }

  // centralized MMU request selection (fixed priority: slot0 > slot1)
  // single outstanding MMU transaction tracked by mmuBusy/mmuOwner/mmuOp
  val needReadSrcAttr = Vec(Bool(), 2)
  val needReadHeapPtr = Vec(Bool(), 2)
  val needReadHum = Vec(Bool(), 2)
  val needWriteBack = Vec(Bool(), 2)
  val needReadDestAttr = Vec(Bool(), 2)
  val slotWantMmu = Vec(Bool(), 2)

  for (i <- 0 until 2) {
    needReadSrcAttr(i) := slotValid(i) && (slotState(i) === overall_state.states(1)) && !srcRegionAttrHit(i)
    needReadHeapPtr(i) := slotValid(i) && (slotState(i) === overall_state.states(4)) && !heapRegionHit(i)
    needReadHum(i) := slotValid(i) && (slotState(i) === overall_state.states(5))
    needWriteBack(i) := slotValid(i) && (slotState(i) === overall_state.states(7))
    needReadDestAttr(i) := slotValid(i) && (slotState(i) === overall_state.states(8)) && !slotCtx(i).accessDestRegionAttr
    slotWantMmu(i) := needReadSrcAttr(i) || needReadHeapPtr(i) || needReadHum(i) || needWriteBack(i) || needReadDestAttr(i)
  }

  when(!mmuBusy) {
    when(slotWantMmu(0)) {
      mmuBusy := True
      mmuOwner := U(0, 1 bits)

      when(needReadSrcAttr(0)) {
        mmuOp := MmuOp.READ_SRC_ATTR
      } elsewhen (needReadHeapPtr(0)) {
        mmuOp := MmuOp.READ_HEAP_PTR
      } elsewhen (needReadHum(0)) {
        mmuOp := MmuOp.READ_HUMONGOUS
      } elsewhen (needWriteBack(0)) {
        mmuOp := MmuOp.WRITE_BACK
      } otherwise {
        mmuOp := MmuOp.READ_DEST_ATTR
      }
    } elsewhen (slotWantMmu(1)) {
      mmuBusy := True
      mmuOwner := U(1, 1 bits)

      when(needReadSrcAttr(1)) {
        mmuOp := MmuOp.READ_SRC_ATTR
      } elsewhen (needReadHeapPtr(1)) {
        mmuOp := MmuOp.READ_HEAP_PTR
      } elsewhen (needReadHum(1)) {
        mmuOp := MmuOp.READ_HUMONGOUS
      } elsewhen (needWriteBack(1)) {
        mmuOp := MmuOp.WRITE_BACK
      } otherwise {
        mmuOp := MmuOp.READ_DEST_ATTR
      }
    }
  }

  // centralized MMU transaction execution
  when(mmuBusy) {
    switch(mmuOp) {
      is(MmuOp.READ_SRC_ATTR) {
        issueReq(io.Mreq, srcRegionAttrAddr(mmuOwner), False, U(2), U(0), slotIssued(mmuOwner)) { rd =>
          slotCtx(mmuOwner).srcRegionAttr := rd(15 downto 0)
          slotState(mmuOwner) := overall_state.states(2)

          regionAttrCacheValid(regionAttrCacheReplacePtr) := True
          regionAttrCacheTag(regionAttrCacheReplacePtr) := srcRegionAttrAddr(mmuOwner)
          regionAttrCache(regionAttrCacheReplacePtr) := rd(15 downto 0)
          regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + 1

          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.READ_HEAP_PTR) {
        issueReq(io.Mreq, heapRegionLookupAddr(mmuOwner), False, U(8), U(0), slotIssued(mmuOwner)) { rd =>
          slotCtx(mmuOwner).heapRegion := rd(GCElementWidth - 1 downto 0)
          slotState(mmuOwner) := overall_state.states(5)

          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.READ_HUMONGOUS) {
        val humAddr = (slotCtx(mmuOwner).heapRegion.resize(MMUAddrWidth) + U"xbc").resize(MMUAddrWidth)
        issueReq(io.Mreq, humAddr, False, U(4), U(0), slotIssued(mmuOwner)) { rd =>
          val hum = (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)
          slotCtx(mmuOwner).heapRegionHumongous := hum
          slotState(mmuOwner) := overall_state.states(6)

          heapRegionCacheValid(heapRegionCacheReplacePtr) := True
          heapRegionCacheTag(heapRegionCacheReplacePtr) := heapRegionLookupAddr(mmuOwner)
          heapRegionCache(heapRegionCacheReplacePtr) := hum
          heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + 1

          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.WRITE_BACK) {
        issueReq(io.Mreq, slotCtx(mmuOwner).task.resize(MMUAddrWidth), True, writeBackSize(), writeBackObjOf(mmuOwner), slotIssued(mmuOwner)) { _ =>
        }
        when(slotIssued(mmuOwner)){
          slotIssued(mmuOwner) := False
          val sameRegion = ((slotCtx(mmuOwner).task ^ slotCtx(mmuOwner).destOopPtr) >> io.ConfigIO.LogOfHRGrainBytes) === U(0)

          when(sameRegion || slotCtx(mmuOwner).heapRegionHumongous) {
            when(mmuOwner === U(0, 1 bits)) {
              finishSlot(0)
            } otherwise {
              finishSlot(1)
            }
          } otherwise {
            slotCtx(mmuOwner).accessDestRegionAttr := False
            slotState(mmuOwner) := overall_state.states(8)
          }

          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      is(MmuOp.READ_DEST_ATTR) {
        issueReq(io.Mreq, destRegionAttrAddr(mmuOwner), False, U(2), U(0), slotIssued(mmuOwner)) { rd =>
          slotCtx(mmuOwner).accessDestRegionAttr := True
          slotCtx(mmuOwner).destRegionAttr := rd(15 downto 0)

          mmuBusy := False
          mmuOp := MmuOp.NONE
        }
      }

      default {
        mmuBusy := False
      }
    }
  }

  // writeBackObjOf needs Int index; use explicit Mux for dynamic owner
  when(mmuBusy && mmuOp === MmuOp.WRITE_BACK) {
    val writeObjDyn = writeBackObjOf(mmuOwner)
    io.Mreq.Request.payload.RequestData := writeObjDyn.resize(MMUDataWidth)
  }

  // --------------------------------------------------------------------------
  // centralized AOP arbitration (fixed priority: slot0 > slot1)
  // --------------------------------------------------------------------------
  val slotWantAop = Vec(Bool(), 2)
  slotWantAop(0) := slotValid(0) && (slotState(0) === overall_state.states(8)) && slotCtx(0).accessDestRegionAttr
  slotWantAop(1) := slotValid(1) && (slotState(1) === overall_state.states(8)) && slotCtx(1).accessDestRegionAttr

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