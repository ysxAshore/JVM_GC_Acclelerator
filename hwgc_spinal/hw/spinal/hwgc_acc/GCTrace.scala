package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCTrace extends Module with GCTopParameters with GCParameters with HWParameters {
  val io = new Bundle {
    val Mreq           = master(new LocalMMUIO)
    val ToAop          = master(new GCToAop)
    val ToTrace        = slave(new GCToTrace)
    val ToStack        = master(new GCToStack)
    val Trace2Fetch    = master Stream UInt(GCElementWidth bits)
    val TaskDone       = in Bool()
    val ConfigIO       = slave(new GCTraceConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // Default outputs
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ToAop.clearIn()
  io.ToTrace.clearOut()

  io.ToStack.Push.valid := False
  io.ToStack.Push.payload.clearAll()
  io.ToStack.LastPush := io.Trace2Fetch.fire

  io.Trace2Fetch.valid := False
  io.Trace2Fetch.payload.clearAll()

  // Request context
  val issued = RegInit(False)

  val Kid               = RegInit(U(0, 32 bits))
  val OopType           = RegInit(U(0, GCOopTypeWidth bits))
  val KlassPtr          = RegInit(U(0, GCElementWidth bits))
  val SrcOopPtr         = RegInit(U(0, GCElementWidth bits))
  val DestOopPtr        = RegInit(U(0, GCElementWidth bits))
  val ScanningInYoung   = RegInit(False)
  val StepIndex         = RegInit(U(0, 32 bits))
  val StepNCreate       = RegInit(U(0, 32 bits))
  val ArrayLength       = RegInit(U(0, 32 bits))
  val PartialArrayStart = RegInit(U(0, 32 bits))

  // Current scanning interval [p, q).
  val p           = RegInit(U(0, GCElementWidth bits))
  val q           = RegInit(U(0, GCElementWidth bits))
  val remainCount = RegInit(U(0, 32 bits))

  // Klass metadata.
  val vtableLen = RegInit(U(0, 32 bits))
  val startMap  = RegInit(U(0, GCElementWidth bits))
  val endMap    = RegInit(U(0, GCElementWidth bits))

  // Current oop slot.
  val src  = RegInit(U(0, GCElementWidth bits))
  val dest = RegInit(U(0, GCElementWidth bits))

  val rawHeapOop = RegInit(U(0, GCElementWidth bits)) // Value loaded from the object field. It may still be compressed.
  val heapOop    = RegInit(U(0, GCElementWidth bits)) // Fully decoded heap address used by region processing.

  val regionAttr = RegInit(U(0, 16 bits))
  val region     = RegInit(U(0, 32 bits))

  val partialTaskCounter = RegInit(U(0, 32 bits))
  val refFieldIndex      = RegInit(U(0, 2 bits))

  // --------------------------------------------------------------------------
  // Pending task buffer
  // Keep the newest task locally. When another task arrives, spill the older
  // task to ToStack. The final task is emitted through Trace2Fetch.
  // --------------------------------------------------------------------------
  val pendingPushValid = RegInit(False)
  val pendingPushPayload = RegInit(U(0, GCElementWidth bits))

  def enqueueTask(task: UInt)(afterAccepted: => Unit): Unit = {
    when(!pendingPushValid) {
      pendingPushValid := True
      pendingPushPayload := task
      afterAccepted
    } otherwise {
      io.ToStack.Push.valid := True
      io.ToStack.Push.payload := pendingPushPayload

      when(io.ToStack.Push.fire) {
        pendingPushPayload := task
        pendingPushValid := True
        afterAccepted
      }
    }
  }

  // Klass metadata cache
  private val KlassCacheEntries    = 16
  private val KlassCacheIndexWidth = log2Up(KlassCacheEntries)

  val klassCacheValid = Vec(RegInit(False), KlassCacheEntries)
  val klassCachePtr = Vec(Reg(UInt(GCElementWidth bits)) init 0, KlassCacheEntries)
  val klassCacheVtable = Vec(Reg(UInt(32 bits)) init 0, KlassCacheEntries)
  val klassCacheStartMap = Vec(Reg(UInt(GCElementWidth bits)) init 0, KlassCacheEntries)
  val klassCacheEndMap = Vec(Reg(UInt(GCElementWidth bits)) init 0, KlassCacheEntries)
  val klassCacheHitVec = Vec(Bool(), KlassCacheEntries)

  for (i <- 0 until KlassCacheEntries) {
    klassCacheHitVec(i) := klassCacheValid(i) && klassCachePtr(i) === KlassPtr
  }
  val klassCacheHit = klassCacheHitVec.orR
  val klassCacheHitIndex = OHToUInt(klassCacheHitVec.asBits)
  val klassCacheReplacePtr = RegInit(U(0, KlassCacheIndexWidth bits))

  // --------------------------------------------------------------------------
  private val HumRegionCacheEntries    = 8
  private val HumRegionCacheIndexWidth = log2Up(HumRegionCacheEntries)

  val humRegionCacheValid = Vec(RegInit(False), HumRegionCacheEntries)
  val humRegionCacheTag = Vec(Reg(UInt(32 bits)) init 0, HumRegionCacheEntries)
  val humRegionCacheReplacePtr = RegInit(U(0, HumRegionCacheIndexWidth bits))
  val regionLookup = ((heapOop - (io.ConfigIO.HeapRegionBias << io.ConfigIO.HeapRegionShiftBy(4 downto 0))) >> io.ConfigIO.LogOfHRGrainBytes).resize(32)
  val humRegionHitVec = Vec(Bool(), HumRegionCacheEntries)

  for (i <- 0 until HumRegionCacheEntries) {
    humRegionHitVec(i) := humRegionCacheValid(i) && humRegionCacheTag(i) === regionLookup
  }
  val humRegionHit = humRegionHitVec.orR

  // Region attribute cache
  private val RegionAttrCacheEntries    = 8
  private val RegionAttrCacheIndexWidth = log2Up(RegionAttrCacheEntries)

  val regionAttrCacheValid = Vec(RegInit(False), RegionAttrCacheEntries)
  val regionAttrCacheTag = Vec(Reg(UInt(MMUAddrWidth bits)) init 0, RegionAttrCacheEntries)
  val regionAttrCache = Vec(Reg(UInt(16 bits)) init 0, RegionAttrCacheEntries)
  val regionAttrCacheReplacePtr = RegInit(U(0, RegionAttrCacheIndexWidth bits))
  val decodedRawHeapOop = Mux(io.ConfigIO.UseCompressedOops,
      (io.ConfigIO.CompressedOopBase + (rawHeapOop << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth),
      rawHeapOop
  )
  val regionAttrAddrLookup = (io.ConfigIO.RegionAttrBiasedBase + (decodedRawHeapOop >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth)
  val regionAttrHitVec = Vec(Bool(), RegionAttrCacheEntries)

  for (i <- 0 until RegionAttrCacheEntries) {
    regionAttrHitVec(i) := regionAttrCacheValid(i) && regionAttrCacheTag(i) === regionAttrAddrLookup
  }
  val regionAttrHit = regionAttrHitVec.orR
  val regionAttrHitIndex = OHToUInt(regionAttrHitVec.asBits)

  // 32-byte oop read window
  val heapWindowValid = RegInit(False)
  val heapWindowData = RegInit(U(0, MMUDataWidth bits))
  val heapWindowAddr = RegInit(U(0, MMUAddrWidth bits))

  // Derived constants
  val oopStride = Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))
  val objArrayHeaderSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(16), U(24))
  val objArrayMarkKlassLenSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(8), U(12))

  def dbg(msg: Seq[Any]): Unit = {
    if (DebugEnable) {
      report(Seq("[GCTrace<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  }

  // Traversal resume selector
  object ResumePoint extends SpinalEnum {
    val Forward, Backward, ReferenceFields = newElement()
  }
  val resumePoint = RegInit(ResumePoint.Forward)

  // State machine
  val fsm = new StateMachine {
    val IDLE                = new State with EntryPoint
    val DISPATCH            = new State
    val PREPARE_ARRAY       = new State
    val READ_KLASS_LAYOUT   = new State
    val SCAN_OOP_MAP        = new State
    val POST_INSTANCE_SCAN  = new State
    val SCAN_FORWARD        = new State
    val SCAN_BACKWARD       = new State
    val READ_OOP            = new State
    val CHECK_OOP           = new State
    val DECIDE_REGION       = new State
    val CHECK_HUMONGOUS     = new State
    val CLEAR_HUM_CANDIDATE = new State
    val MARK_HUM_REGION     = new State
    val SEND_AOP            = new State
    val FINISH              = new State

    // FSM helper methods
    def finishTask(): Unit = {
      io.ToTrace.Done := True
      goto(IDLE)
    }

    def resumeTraversal(): Unit = {
      switch(resumePoint) {
        is(ResumePoint.Forward) {
          goto(SCAN_FORWARD)
        }

        is(ResumePoint.Backward) {
          goto(SCAN_BACKWARD)
        }

        is(ResumePoint.ReferenceFields) {
          goto(POST_INSTANCE_SCAN)
        }
      }
    }

     // Consume one element from a 32-byte read window.
     // remainCount includes the current element when READ_OOP is active.
     // Therefore it is decremented here, when the element is actually consumed.
    def consumeWindow(base: UInt, data: UInt): Unit = {
      when(io.ConfigIO.UseCompressedOops) {
        val index = ((src - base) >> 2).resize(3 bits)
        val words = data.subdivideIn(32 bits)
        rawHeapOop := words(index).resize(GCElementWidth)
      } otherwise {
        val index = ((src - base) >> 3).resize(2 bits)
        val words = data.subdivideIn(GCElementWidth bits)
        rawHeapOop := words(index)
      }

      remainCount := remainCount - 1
      goto(CHECK_OOP)
    }

    def issueElementRead(address: UInt): Unit = {
      val elementBytes = Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))

      issueReq(io.Mreq, address, False, elementBytes, U(0), True, False, issued) { readData =>
        when(io.ConfigIO.UseCompressedOops) {
          rawHeapOop := readData(31 downto 0).resize(GCElementWidth)
        } otherwise {
          rawHeapOop := readData(GCElementWidth - 1 downto 0)
        }

        remainCount := remainCount - 1
        goto(CHECK_OOP)
      }
    }

    def issueWindowRead(address: UInt): Unit = {
      issueReq(io.Mreq, address, False, U(32), U(0), True, False, issued) { readData =>
        heapWindowAddr := address
        heapWindowData := readData
        heapWindowValid := True

        consumeWindow(address, readData)
      }
    }

    // Common region decision logic.
    // attr and oop are passed directly so that cache-hit processing does not
    // depend on reading registers assigned in the same clock cycle.
    def processRegion(attr: UInt, oop: UInt): Unit = {
      val attrType = attr(15 downto 8).asSInt
      val crossesRegion = ((dest ^ oop) >> io.ConfigIO.LogOfHRGrainBytes(5 downto 0)).orR

      when(attrType >= S(0, 8 bits)) {
        val task = dest + Mux(io.ConfigIO.UseCompressedOops, U(1), U(0))
        enqueueTask(task) {
          resumeTraversal()
        }
      } elsewhen crossesRegion {
        when(attrType === S(-2, 8 bits)) {
          goto(CHECK_HUMONGOUS)
        } otherwise {
          goto(SEND_AOP)
        }
      } otherwise {
        resumeTraversal()
      }
    }

    IDLE.whenIsActive {
      io.ToTrace.Ready := True

      when(io.ToTrace.Valid && io.ToTrace.Ready) {
        Kid             := io.ToTrace.Kid
        OopType         := io.ToTrace.OopType
        KlassPtr        := io.ToTrace.KlassPtr
        SrcOopPtr       := io.ToTrace.SrcOopPtr
        DestOopPtr      := io.ToTrace.DestOopPtr
        ScanningInYoung := io.ToTrace.ScanningInYoung
        StepIndex       := io.ToTrace.StepIndex
        StepNCreate     := io.ToTrace.StepNCreate
        ArrayLength     := io.ToTrace.ArrayLength

        PartialArrayStart := Mux(io.ToTrace.OopType === U(PartialArrayOop), io.ToTrace.PartialArrayStart, U(0))

        partialTaskCounter := 0
        refFieldIndex := 0
        remainCount := 0

        heapWindowValid := False
        heapWindowAddr := 0

        issued := False

        goto(DISPATCH)

        dbg(Seq("Receive GCTrace Task",
            ", RegionAttrBase = ", io.ConfigIO.RegionAttrBase,
            ", RegionAttrShiftBy = ", io.ConfigIO.RegionAttrShiftBy,
            ", RegionAttrBiasedBase = ", io.ConfigIO.RegionAttrBiasedBase,
            ", HeapRegionBias = ", io.ConfigIO.HeapRegionBias,
            ", HeapRegionShiftBy = ", io.ConfigIO.HeapRegionShiftBy,
            ", LogOfHRGrainBytes = ", io.ConfigIO.LogOfHRGrainBytes,
            ", OopType = ", io.ToTrace.OopType,
            ", KlassPtr = ", io.ToTrace.KlassPtr,
            ", SrcOopPtr = ", io.ToTrace.SrcOopPtr,
            ", DestOopPtr = ", io.ToTrace.DestOopPtr,
            ", Kid = ", io.ToTrace.Kid,
            ", ArrayLength = ", io.ToTrace.ArrayLength,
            ", PartialArrayStart = ", io.ToTrace.PartialArrayStart,
            ", StepIndex = ", io.ToTrace.StepIndex,
            ", StepNCreate = ", io.ToTrace.StepNCreate
          )
        )
      }
    }

    DISPATCH.whenIsActive {
      when(OopType === U(PartialArrayOop)) {
        val address = DestOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U"xc", U"x10")

        issueReq(io.Mreq, address, True, U(4), ArrayLength, False, False, issued) { _ =>}
        when(issued) {
          issued := False
          goto(PREPARE_ARRAY)
        }
      } elsewhen (Kid === U(ObjectArrayKlassID)) {
        val address = DestOopPtr + U(8)
        val writeValue = Mux(io.ConfigIO.UseCompressedKlassPointers,
            Cat(StepIndex, KlassPtr(31 downto 0)).resize(96),
            Cat(StepIndex, KlassPtr).resize(96)).asUInt

        issueReq(io.Mreq, address, True, objArrayMarkKlassLenSize, writeValue, False, False, issued) { _ =>}
        when(issued) {
          issued := False
          goto(PREPARE_ARRAY)
        }
      } otherwise {
        when(klassCacheHit) {
          vtableLen := klassCacheVtable(klassCacheHitIndex)
          startMap := klassCacheStartMap(klassCacheHitIndex)
          endMap := klassCacheEndMap(klassCacheHitIndex)

          goto(SCAN_OOP_MAP)
        } otherwise {
          val address = KlassPtr + U(160)

          issueReq(io.Mreq, address, False, U(4), U(0), True, False, issued) { readData =>
            vtableLen := readData(31 downto 0)
            goto(READ_KLASS_LAYOUT)
          }
        }
      }
    }

    PREPARE_ARRAY.whenIsActive {
      val low = (DestOopPtr + objArrayHeaderSize + PartialArrayStart * oopStride).resize(GCElementWidth)
      val high = (DestOopPtr + objArrayHeaderSize + StepIndex * oopStride).resize(GCElementWidth)
      val objectBegin = (DestOopPtr + objArrayHeaderSize).resize(GCElementWidth)
      val objectEnd = (objectBegin + ArrayLength * oopStride).resize(GCElementWidth)
      val nextP = Mux(objectBegin < low, low, objectBegin)
      val nextQ = Mux(objectEnd > high, high, objectEnd)
      val needCreateTask = Mux(OopType === U(PartialArrayOop), partialTaskCounter < StepNCreate, ArrayLength > StepIndex)

      p := nextP
      q := nextQ

      remainCount := ((nextQ - nextP) >> Mux(io.ConfigIO.UseCompressedOops, U(2), U(3))).resize(32)

      when(needCreateTask) {
        val createTask = SrcOopPtr + U(2)

        enqueueTask(createTask) {
          when(OopType === U(PartialArrayOop)) {
            partialTaskCounter := partialTaskCounter + 1
          } otherwise {
            goto(SCAN_FORWARD)
          }
        }
      } otherwise {
        goto(SCAN_FORWARD)
      }
    }

    READ_KLASS_LAYOUT.whenIsActive {
      val address = KlassPtr + U(296)

      issueReq(io.Mreq, address, False, U(8), U(0), True, False, issued) { readData =>
        val oopMapOffset = readData(63 downto 32)
        val oopMapCount = readData(31 downto 0)
        val newStartMap = (KlassPtr + U(464) + (vtableLen + oopMapOffset) * U(8)).resize(GCElementWidth)
        val newEndMap = (KlassPtr + U(464) + (vtableLen + oopMapOffset + oopMapCount) * U(8)).resize(GCElementWidth)

        klassCacheValid(klassCacheReplacePtr) := True
        klassCachePtr(klassCacheReplacePtr) := KlassPtr
        klassCacheVtable(klassCacheReplacePtr) := vtableLen
        klassCacheStartMap(klassCacheReplacePtr) := newStartMap
        klassCacheEndMap(klassCacheReplacePtr) := newEndMap
        klassCacheReplacePtr := klassCacheReplacePtr + 1

        startMap := newStartMap
        endMap := newEndMap

        goto(SCAN_OOP_MAP)
      }
    }

    SCAN_OOP_MAP.whenIsActive {
      when(startMap < endMap) {
        val address = endMap - U(8)

        issueReq(io.Mreq, address, False, U(8), U(0), True, False, issued) { readData =>
          val mapOffset = readData(31 downto 0)
          val mapCount = readData(63 downto 32)

          p := (DestOopPtr + mapOffset).resize(GCElementWidth)
          q := (DestOopPtr + mapOffset + mapCount * oopStride).resize(GCElementWidth)
          remainCount := mapCount.resize(32)
          endMap := address

          goto(SCAN_BACKWARD)
        }
      } otherwise {
        refFieldIndex := 0
        goto(POST_INSTANCE_SCAN)
      }
    }

    POST_INSTANCE_SCAN.whenIsActive {
      when(Kid === U(InstanceMirrorKlassID)) {
        val address = SrcOopPtr + U"x24"

        issueReq(io.Mreq, address, False, U(4), U(0), True, False, issued) { readData =>
          val mirrorArrayLength = readData(31 downto 0)

          p := DestOopPtr + U"x70"
          q := (DestOopPtr + U"x70" + mirrorArrayLength * oopStride).resize(GCElementWidth)
          remainCount := mirrorArrayLength.resize(32)

          goto(SCAN_FORWARD)
        }
      } elsewhen (Kid === U(InstanceRefKlassID)) {
         // index 0 -> discovered index 1 -> referent index 2 -> discovered
        when(refFieldIndex === U(2)) {
          goto(FINISH)
        } otherwise {
          val discoveredOffset = Mux(io.ConfigIO.UseCompressedOops && io.ConfigIO.UseCompressedKlassPointers,
              U"x18",
              Mux(io.ConfigIO.UseCompressedOops, U"x1c", U"x28"))

          val referentOffset = Mux(io.ConfigIO.UseCompressedOops && io.ConfigIO.UseCompressedKlassPointers, U"xc", U"x10")
          val fieldOffset = Mux(refFieldIndex === U(0), discoveredOffset, referentOffset)

          src := SrcOopPtr + fieldOffset
          dest := DestOopPtr + fieldOffset
          refFieldIndex := refFieldIndex + 1

          // A Reference special field is exactly one element. This guarantees
          // that READ_OOP selects the scalar-read path.
          remainCount := 1
          resumePoint := ResumePoint.ReferenceFields

          goto(READ_OOP)
        }
      } otherwise {
        goto(FINISH)
      }
    }

    SCAN_FORWARD.whenIsActive {
      when(p < q) {
        src := p - DestOopPtr + SrcOopPtr
        dest := p
        p := p + oopStride
        resumePoint := ResumePoint.Forward
        goto(READ_OOP)
      } otherwise {
        heapWindowValid := False
        goto(FINISH)
      }
    }

    SCAN_BACKWARD.whenIsActive {
      when(p < q) {
        val currentQ = q - oopStride

        src := currentQ - DestOopPtr + SrcOopPtr
        dest := currentQ
        q := currentQ
        resumePoint := ResumePoint.Backward
        goto(READ_OOP)
      } otherwise {
        heapWindowValid := False
        goto(SCAN_OOP_MAP)
      }
    }

    READ_OOP.whenIsActive {
      val alignedBase = src & ~U(31, src.getWidth bits)
      val cacheHit = heapWindowValid && src >= heapWindowAddr && src < heapWindowAddr + LineBytesNum
      val offsetInAlignedWindow = src - alignedBase
      val index32 = (offsetInAlignedWindow >> 2).resize(4 bits)
      val index64 = (offsetInAlignedWindow >> 3).resize(3 bits)
      val backward = resumePoint === ResumePoint.Backward

      val alignedCoverage = UInt(4 bits)
      alignedCoverage := 0

      when(io.ConfigIO.UseCompressedOops) {
        when(backward) {
          alignedCoverage := (index32 + 1).resized
        } otherwise {
          alignedCoverage := (U(8) - index32).resized
        }
      } otherwise {
        when(backward) {
          alignedCoverage := (index64 + 1).resized
        } otherwise {
          alignedCoverage := (U(4) - index64).resized
        }
      }

      // For a sufficiently long backward run, place the current element at the end of the 32-byte window.
      val shiftedBase = UInt(src.getWidth bits)
      shiftedBase := src

      when(backward) {
        shiftedBase := Mux(io.ConfigIO.UseCompressedOops, src - U(28, src.getWidth bits), src - U(24, src.getWidth bits))
      }

      when(cacheHit) {
        consumeWindow(heapWindowAddr, heapWindowData)
      } elsewhen (remainCount === U(1)) {
        issueElementRead(src)
      } elsewhen (remainCount <= alignedCoverage.resize(32)) {
        issueWindowRead(alignedBase)
      } otherwise {
        issueWindowRead(shiftedBase)
      }
    }

    CHECK_OOP.whenIsActive {
      when(rawHeapOop === U(0)) {
        resumeTraversal()
      } elsewhen regionAttrHit {
        val cachedAttr = regionAttrCache(regionAttrHitIndex)

        heapOop := decodedRawHeapOop
        regionAttr := cachedAttr

        processRegion(cachedAttr, decodedRawHeapOop)
      } otherwise {
        issueReq(io.Mreq, regionAttrAddrLookup, False, U(2), U(0), True, False, issued) { readData =>
          val loadedAttr = readData(15 downto 0)

          heapOop := decodedRawHeapOop
          regionAttr := loadedAttr

          regionAttrCacheValid(regionAttrCacheReplacePtr) := True
          regionAttrCacheTag(regionAttrCacheReplacePtr) := regionAttrAddrLookup
          regionAttrCache(regionAttrCacheReplacePtr) := loadedAttr
          regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + 1

          goto(DECIDE_REGION)
        }
      }
    }

    DECIDE_REGION.whenIsActive {
      processRegion(regionAttr, heapOop)
    }

    CHECK_HUMONGOUS.whenIsActive {
      when(humRegionHit) {
        goto(SEND_AOP)
      } otherwise {
        val candidateAddress = (io.ConfigIO.HumongousReclaimCandidatesBoolBase + regionLookup).resize(MMUAddrWidth)

        issueReq(io.Mreq, candidateAddress, False, U(1), U(0), True, False, issued) { readData =>
          val lookedUpRegion = regionLookup

          humRegionCacheValid(humRegionCacheReplacePtr) := True
          humRegionCacheTag(humRegionCacheReplacePtr) := lookedUpRegion
          humRegionCacheReplacePtr := humRegionCacheReplacePtr + 1

          region := lookedUpRegion

          when(readData(7 downto 0) === U(0)) {
            goto(SEND_AOP)
          } otherwise {
            goto(CLEAR_HUM_CANDIDATE)
          }
        }
      }
    }

    CLEAR_HUM_CANDIDATE.whenIsActive {
      val address = (io.ConfigIO.HumongousReclaimCandidatesBoolBase + region).resize(MMUAddrWidth)

      issueReq(io.Mreq, address, True, U(1), U(0), False, False, issued) { _ =>}

      when(issued) {
        issued := False
        goto(MARK_HUM_REGION)
      }
    }

    MARK_HUM_REGION.whenIsActive {
      val address = (io.ConfigIO.RegionAttrBase + region * U(2) + U(1)).resize(MMUAddrWidth)

      issueReq(io.Mreq, address, True, U(1), S(-1, 8 bits).asUInt, False, False, issued) { _ =>}

      when(issued) {
        issued := False

        for (i <- 0 until RegionAttrCacheEntries) {
          when(regionAttrCacheValid(i) && regionAttrCacheTag(i) === regionAttrAddrLookup) {
            regionAttrCache(i) := Cat(U(0xff, 8 bits), regionAttrCache(i)(7 downto 0)).asUInt
          }
        }

        goto(SEND_AOP)
      }
    }

    SEND_AOP.whenIsActive {
      when(ScanningInYoung) {
        resumeTraversal()
      } otherwise {
        io.ToAop.Valid := True
        io.ToAop.Task := dest
        io.ToAop.RegionAttr := regionAttr

        when(io.ToAop.Valid && io.ToAop.Ready) {
          resumeTraversal()
        }
      }
    }

    FINISH.whenIsActive {
      when(pendingPushValid) {
        io.Trace2Fetch.valid := True
        io.Trace2Fetch.payload := pendingPushPayload

        when(io.Trace2Fetch.fire) {
          pendingPushValid := False
          finishTask()
        }
      } otherwise {
        finishTask()
      }
    }
  }

  when(io.TaskDone) {
    for (i <- 0 until KlassCacheEntries) {
      klassCacheValid(i) := False
    }

    for (i <- 0 until HumRegionCacheEntries) {
      humRegionCacheValid(i) := False
    }

    for (i <- 0 until RegionAttrCacheEntries) {
      regionAttrCacheValid(i) := False
    }

    klassCacheReplacePtr := 0
    humRegionCacheReplacePtr := 0
    regionAttrCacheReplacePtr := 0

    heapWindowValid := False
  }
}

object GCTraceVerilog extends App {
  Config.spinal.generateVerilog(new GCTrace())
}