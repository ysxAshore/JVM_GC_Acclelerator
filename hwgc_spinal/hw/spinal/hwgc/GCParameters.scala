package hwgc

import spinal.core._
import spinal.core.sim.SimConfig
import spinal.lib._

import scala.language.postfixOps

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "sim/vsrc",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    )
  )
  def sim = SimConfig.withConfig(spinal).withFstWave
}

trait GCParameters {
  val GCTaskStack_Entry = 64
  val GCTaskStack_SpillNeed = 63
  val GCTaskStack_ReadNeed = 8
  val GCoopWorkStages = 8
  val GCaopWorkStages = 10
  val GCCopyEntry = 64

  /* ----------------- ScannerTask Tag ----------------- */
  val OopTag = 0
  val NarrowOopTag = 1
  val PartialArrayTag = 2
  val CommonOop = 0
  val CompressedOop = 1
  val PartialArrayOop = 2
  val GCOopTypeWidth = 2

  val InstanceKlassID = 0
  val InstanceRefKlassID = 1
  val InstanceMirrorKlassID = 2
  val InstanceClassLoaderKlassID = 3
  val TypeArrayKlassID = 4
  val ObjectArrayKlassID = 5

  // OopDesc
  // --- Common Oop
  val MarkWordOff = 0
  val KlassOff = 8
  val ElementOff = 16
  // --- Array Oop
  val ArrayLenOff = 16
  val ArrayElementOff = 24
  // --- Ref Oop
  val REFERENT_OFFSET = 16
  val DISCOVERED_OFFSET = 40
  // --- mirror Oop
  val OopSizeOff = 36
  val staticOopFieldCountOff = 40

  val LhKidOff = 8
  val VTableLenOff = 160
  val NonstaticOopMapSizeOff = 296
  val ITableLenOff = 300
  val REFERENCE_TYPE = 315
  val VTableOff = 464
  val StaticFieldOff = 184

  val TypeOffSet = 1
  val Type_Young = 0
  val Type_Humongous = -2
  val Type_NoInCset = -1

  /*-------------- MarkWord ------------------------*/
  val LOCK_MASK_IN_PLACE = 3
  val LOCKED_VALUE = 0
  val UNLOCKED_VALUE = 1
  val MONITOR_VALUE = 2
  val MARKED_VALUE = 3
  val AGE_SHIFT = 3
  val AGE_MASK = 15
  val AGE_MASK_IN_PLACE = AGE_MASK << AGE_SHIFT
  val NO_LOCK_IN_PLACE = 1 // UNLOCJED_VALUE
  val NO_HASH_IN_PLACE = 0

  /*-------------- G1ParScanThreadState ------------*/
  val QSET_OFFSET = 24
  val PSS_CARD_TABLE_OFFSET = 96
  val PLAB_ALLOCATOR_OFFSET = 112
  val AGE_TABLE_OFFSET = 120
  val REGION_ATTR_DEST_OFFSET = 376
  val TENURING_THRESHOLD_OFFSET = 380
  val OBJCLOSURE_OFFSET = 384
  val LAST_ENQUEUED_CARD_OFFSET = 432
  val SURVIVING_YOUNG_WORDS_BASE_OFFSET = 464
  val OLD_GEN_IS_FULL_OFFSET = 488
  val PARTIAL_ARRAY_CHUNK_SIZE_OFFSET = 492
  val PARTIAL_ARRAY_STEPPER_OFFSET = 496
  val OBJ_ALLOC_STAT_OFFSET = 560

  /*------------------ HeapRegion -------------------*/
  val REGION_BOTTOM_OFFSET = 0x0
  val REGION_END_OFFSET = 0x8
  val REGION_TOP_OFFSET = 0x10
  val REGION_COMPACTION_TOP_OFFSET = 0x18
  val REGION_BOT_PART_OFFSET = 0x20
  val REGION_PAR_ALLOC_LOCK_OFFSET = 0x40
  val REGION_PRE_DUMMY_TOP_OFFSET = 0xa8
  val NEXT_TOP_AT_MARK_START_OFFSET = 0xe8
  val REGION_REM_SET_OFFSET = 0xb0
  val REGION_HRM_INDEX_OFFSET = 0xb8
  val HEAP_REGION_TYPE_OFFSET = 0xbc
  val HEAP_NEXT_OFFSET = 0xd0
  val HEAP_PREV_OFFSET = 0xd8
  val NODE_INDEX_OFFSET = 0x120
  val YOUND_INDEX_IN_CSET_OFFSET = 0x100
  val LogOfHRGrainBytes = 0x16
  val YOUNG_MASK = 0x2
  val OLD_MASK = 0x10
  val HEAPREGION_TYPE_SURVIVOR = 0x3
  val HEAPREGION_TYPE_OLD = 0x10

  /*----------------- Plab Allocator ----------------*/
  val COLLECTEDHEAP_OFFSET = 0x0
  val ALLOCATOR_OFFSET = 0x8
  val ALLOC_BUFFERS_OFFSET = 0x10
  val DIRECT_ALLOCATED_OFFSET = 0x20
  val NUM_PLAB_FILLS = 0x30
  val NUM_DIRECT_ALLOCATIONS = 0x40

  /*------------------- PLAB ------------------------*/
  val WORDSZ_OFFSET = 0x20
  val BOTTOM_OFFSET = 0x28
  val TOP_OFFSET = 0x30
  val END_OFFSET = 0x38
  val HARD_END_OFFSET = 0x40
  val ALLOCATED_OFFSET = 0x48
  val WASTED_OFFSET = 0x50

  /*-------------------- ObjClosure -----------------*/
  val SCANNING_IN_YOUNG_OFFSET = 32


  /*------------------- RDC Qset --------------------*/
  val QSET_QUEUE_OFFSET = 48

  /*--------------------- CardTable -----------------*/
  val BYTE_MAP_OFFSET = 56
  val BYTE_MAP_BASE_OFFSET = 64

  /*------------------- PtrQueue --------------------*/
  val INDEX_OFFSET = 0
  val BUFFER_OFFSET = 16

  val GCTaskQueue_Size = 1 << 17
  val GCScannerTask_Size = 8
  val GCObjectPtr_Size = 8
  val LogHeapWordSize = log2Up(GCObjectPtr_Size)
  val GCHeapRegionAttr_Size = 2
}

trait HWParameters {
  //true is boolean, True is Bool
  val DebugEnable = true

  val MMUAddrWidth = 64
  val MMUDataWidth = 64

  val LLCSourceMaxNum = 64
  val LLCSourceMaxNumBitSize = log2Up(LLCSourceMaxNum) + 1

  val allBytesOnes = U((BigInt(1) << (MMUDataWidth / 8)) - 1, MMUDataWidth / 8 bits)
  val halfBytesOnes = U((BigInt(1) << (MMUDataWidth / 8 / 2)) - 1, MMUDataWidth / 8 bits)
  val oneByteOnes = U(1, MMUDataWidth / 8 bits)

  // helper: issueReq
  def issueReq(port: LocalMMUIO, addr: UInt, Write: Bool, WriteStrb: UInt, WriteData: UInt, reqIssued: Bool)(onResp: UInt => Unit): Unit = {

    // ensure default safe values (caller may have already set globals; safe to reassign)
    port.Request.valid := False
    port.Response.ready := False

    // Issue request if not already issued
    when(!reqIssued) {
      port.Request.valid := True
      // payload fields (these must exist on your Request bundle)
      port.Request.payload.RequestVirtualAddr := addr
      port.Request.payload.RequestSourceID := port.ConherentRequsetSourceID.payload
      port.Request.payload.RequestType_isWrite := Write
      port.Request.payload.RequestWStrb := WriteStrb
      port.Request.payload.RequestData := WriteData
      port.Response.ready := True

      // mark as issued when downstream accepts the request
      when(port.Request.fire) {
        reqIssued := True
      }
    }

    // handle response (when previously issued)
    when(reqIssued && port.Response.fire) {
      val rd = port.Response.payload.ResponseData
      reqIssued := False
      onResp(rd) // callback to let caller handle response data
    }
  }

  def process2stream(p : GCProcess2Trace): Stream[GCTracePayload] = {
    val s = Stream(GCTracePayload())

    s.valid := p.Valid
    s.payload.OopType := p.OopType
    s.payload.KlassPtr := p.KlassPtr
    s.payload.SrcOopPtr := p.SrcOopPtr
    s.payload.DestOopPtr := p.DestOopPtr
    s.payload.Kid := p.Kid
    s.payload.ArrayLength := p.ArrayLength
    s.payload.PartialArrayStart := p.PartialArrayStart
    s.payload.StepIndex := p.StepIndex
    s.payload.StepNCreate := p.StepNCreate

    p.Ready := s.ready
    s
  }

  def aop2stream(p : AopParameters) : Stream[GCAopPayload] = {
    val s = Stream(GCAopPayload())
    s.valid := p.Valid
    s.payload.ParScanThreadStatePtr := p.ParScanThreadStatePtr
    s.payload.RegionAttrPtr := p.RegionAttrPtr
    s.payload.Task := p.Task

    p.Ready := s.ready
    s
  }
}

class GCFetch2ProcessUnit extends Bundle with HWParameters with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()
  val DestOopPtr = out UInt(MMUAddrWidth bits)

  val Task = in UInt(MMUAddrWidth bits)
  val OopType = in UInt(GCOopTypeWidth bits)
  val SrcOopPtr = in UInt(MMUAddrWidth bits)
  val MarkWord = in UInt(MMUDataWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DestOopPtr)
    out(Valid, Task, OopType, SrcOopPtr, MarkWord)
  }
}

class GCProcess2Survivor extends Bundle with HWParameters with GCParameters with IMasterSlave {
    val Valid = in Bool()
    val Ready = out Bool()

    val Done = out Bool()
    val DestOopPtr = out UInt(MMUAddrWidth bits)

    val MarkWord = in UInt(MMUDataWidth bits)
    val SrcOopPtr = in UInt(MMUAddrWidth bits)
    val RegionAttrPtr = in UInt(MMUAddrWidth bits)

    override def asMaster(): Unit = {
      in(Ready, Done, DestOopPtr)
      out(Valid, SrcOopPtr, MarkWord, RegionAttrPtr)
    }
}

class ProcessUnit extends Bundle with HWParameters with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()
  val DestOopPtr = out UInt(MMUAddrWidth bits)

  val Task = in UInt(MMUAddrWidth bits)
  val OopType = in UInt(GCOopTypeWidth bits)
  val SrcOopPtr = in UInt(MMUAddrWidth bits)
  val MarkWord = in UInt(MMUDataWidth bits)
  val RegionAttr = in UInt(16 bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DestOopPtr)
    out(Valid, Task, OopType, SrcOopPtr, MarkWord, RegionAttr)
  }
}

case class GCTracePayload() extends Bundle with HWParameters with GCParameters {
  val OopType = UInt(GCOopTypeWidth bits)
  val KlassPtr = UInt(MMUAddrWidth bits)
  val SrcOopPtr = UInt(MMUAddrWidth bits)
  val DestOopPtr = UInt(MMUAddrWidth bits)
  val Kid = UInt(32 bits)
  val ArrayLength = UInt(32 bits)
  val PartialArrayStart = UInt(32 bits)
  val StepIndex = UInt(32 bits)
  val StepNCreate = UInt(32 bits)
}

class GCProcess2Trace extends Bundle with HWParameters with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  // some parse module caculate parameters
  val OopType = in UInt(GCOopTypeWidth bits)
  val KlassPtr = in UInt(MMUAddrWidth bits)
  val SrcOopPtr = in UInt(MMUAddrWidth bits)
  val DestOopPtr = in UInt(MMUAddrWidth bits)
  val Kid = in UInt(32 bits)
  val ArrayLength = in UInt(32 bits)
  val PartialArrayStart = in UInt(32 bits)
  val StepIndex = in UInt(32 bits)
  val StepNCreate = in UInt(32 bits)
  val ScanningInYoung = in Bool()

  override def asMaster(): Unit = {
    out(Valid, OopType, KlassPtr, SrcOopPtr, DestOopPtr, Kid, ArrayLength, PartialArrayStart, StepIndex, StepNCreate, ScanningInYoung)
    in(Ready, Done)
  }
}

class GCProcess2Copy extends Bundle with HWParameters with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  // some parse module caculate parameters
  val DestOopPtr = in UInt(MMUAddrWidth bits)
  val SrcOopPtr = in UInt(MMUAddrWidth bits)
  val Size = in UInt(MMUDataWidth bits)

  override def asMaster(): Unit = {
    out(Valid, SrcOopPtr, DestOopPtr, Size)
    in(Ready, Done)
  }
}

class GCArrayProcessConfigIO extends Bundle with HWParameters with IMasterSlave{
  val ParScanThreadStatePtr = in UInt(MMUAddrWidth bits)
  val ChunkSize = in UInt(32 bits)
  val STEPPER_OFFSET = in UInt(MMUDataWidth bits)
  val HeapRegionBiasedBase = in UInt(MMUAddrWidth bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  override def asMaster(): Unit = {
    out(ParScanThreadStatePtr, ChunkSize, STEPPER_OFFSET, HeapRegionBiasedBase, HeapRegionShiftBy)
    in()
  }
}

class GCOopProcessConfigIO extends Bundle with HWParameters with IMasterSlave{
  val ParScanThreadStatePtr = in(UInt(MMUAddrWidth bits))
  val RegionAttrBiasedBase = in UInt(MMUAddrWidth bits)
  val RegionAttrShiftBy = in UInt(32 bits)
  val HeapRegionBiasedBase = in UInt(MMUAddrWidth bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  override def asMaster(): Unit = {
    out(ParScanThreadStatePtr, RegionAttrBiasedBase, RegionAttrShiftBy, HeapRegionBiasedBase, HeapRegionShiftBy)
    in()
  }
}

class GCCopy2SurvivorConfigIO extends Bundle with HWParameters with GCParameters with IMasterSlave {
  val ParScanThreadStatePtr = in(UInt(MMUAddrWidth bits))
  val RegionAttrBiasedBase = in UInt(MMUAddrWidth bits)
  val RegionAttrShiftBy = in UInt(32 bits)
  val HeapRegionBiasedBase = in UInt(MMUAddrWidth bits)
  val HeapRegionShiftBy = in UInt(32 bits)


  override def asMaster(): Unit = {
    out(ParScanThreadStatePtr, RegionAttrBiasedBase, RegionAttrShiftBy, HeapRegionBiasedBase, HeapRegionShiftBy)
    in()
  }
}

class GCTraceConfigIO extends Bundle with HWParameters with IMasterSlave{
  val RegionAttrBase = in UInt(MMUAddrWidth bits)
  val RegionAttrBiasedBase = in UInt(MMUAddrWidth bits)
  val RegionAttrShiftBy = in UInt(32 bits)
  val HeapRegionBias = in UInt(32 bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  val HumongousReclaimCandidatesBoolBase = in UInt(MMUAddrWidth bits)
  val ParScanThreadStatePtr = in UInt(MMUAddrWidth bits)

  override def asMaster(): Unit = {
    out(RegionAttrBase, RegionAttrBiasedBase, RegionAttrShiftBy, HeapRegionBias, HeapRegionShiftBy, HumongousReclaimCandidatesBoolBase, ParScanThreadStatePtr)
    in()
  }
}

class GCTaskStackConfigIO extends Bundle with HWParameters with IMasterSlave{
  val TaskQueue_BottomAddr = in UInt(MMUAddrWidth bits)
  val TaskQueue_AgeTopAddr = in UInt(MMUAddrWidth bits)
  val TaskQueue_ElemsBase = in UInt(MMUAddrWidth bits)

  val TaskReady = out Bool()
  val TaskValid = in Bool()
  val Done = out Bool()

  override def asMaster(): Unit = {
    out(TaskQueue_BottomAddr, TaskQueue_AgeTopAddr, TaskQueue_ElemsBase, TaskValid)
    in(TaskReady, Done)
  }
}

class TraceMReq2MMU(oopWorkStage:Int, oopTraceStates:Int) extends Bundle with IMasterSlave {
  val oopWorkMReqs  = Vec.fill(oopWorkStage)(new LocalMMUIO)
  val staticMReq    = new LocalMMUIO
  val oopTraceMReqs = Vec.fill(oopTraceStates)(new LocalMMUIO)

  override def asMaster(): Unit = {
    oopWorkMReqs.foreach(master(_))
    master(staticMReq)
    oopTraceMReqs.foreach(master(_))
  }
}

case class GCAopPayload() extends Bundle with HWParameters with GCParameters{
  val ParScanThreadStatePtr = UInt(MMUAddrWidth bits)
  val RegionAttrPtr = UInt(MMUAddrWidth bits)
  val Task = UInt(MMUAddrWidth bits)
}

class AopParameters extends Bundle with HWParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()
  val ParScanThreadStatePtr = in UInt(MMUAddrWidth bits)
  val RegionAttrPtr = in UInt(MMUAddrWidth bits)
  val Task = in UInt(MMUAddrWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done)
    out(Valid, ParScanThreadStatePtr, RegionAttrPtr, Task)
  }
}

class LocalMMUIO extends Bundle with HWParameters with IMasterSlave{
  //发出的访存请求
  val Request = master Stream(new Bundle{
    val RequestVirtualAddr = UInt(MMUAddrWidth bits)
    val RequestData = UInt(MMUDataWidth bits)
    val RequestSourceID = UInt(LLCSourceMaxNumBitSize bits)
    val RequestType_isWrite = Bool()
    val RequestWStrb = UInt(MMUDataWidth / 8 bits)
  })

  //读请求分发到的TL Link的事务编号
  val ConherentRequsetSourceID    = slave Flow(UInt(LLCSourceMaxNumBitSize bits))

  //Memoryloader一定能保证收回！
  val Response = slave Stream(new Bundle{
    val ResponseData = UInt(MMUDataWidth bits)
    val ResponseSourceID = UInt(LLCSourceMaxNumBitSize bits)
  })
  override def asMaster(): Unit = {
    master(Request)
    slave(Response,ConherentRequsetSourceID)
  }
}

object WrapInc
{
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      (value + U(1))(log2Up(n)-1 downto  0)
    } else {
      val wrap = (value === U(n-1))
      Mux(wrap, U(0), value + U(1))
    }
  }
}
object WrapDec {
  // "n" is the number of elements, valid range is 0 ~ n-1.
  // Decrement with wrap:
  //    if value == 0 → n-1
  //    else          → value - 1
  def apply(value: UInt, n: Int): UInt = {
    if (isPow2(n)) {
      // 对于 2 的幂，直接做减法并截位即可
      (value - U(1))(log2Up(n)-1 downto 0)
    } else {
      // 非 2 的幂，用条件判断
      val wrap = (value === U(0))
      Mux(wrap, U(n-1), value - U(1))
    }
  }
}


object LocalMMUTaskType {
  val TaskTypeBitWidth = 5
  val TaskTypeMax = 30
}

class Ctrl2Top extends Bundle with HWParameters with IMasterSlave {
  val RegionAttrBase = in UInt(MMUAddrWidth bits)
  val RegionAttrBiasedBase = in UInt(MMUAddrWidth bits)
  val RegionAttrShiftBy = in UInt(32 bits)
  val HeapRegionBias = in UInt(32 bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  val HeapRegionBiasedBase = in UInt(MMUAddrWidth bits)
  val HumongousReclaimCandidatesBoolBase = in UInt(MMUAddrWidth bits)
  val ParScanThreadStatePtr = in UInt(MMUAddrWidth bits)
  val TaskQueue_BottomAddr = in UInt(MMUAddrWidth bits)
  val TaskQueue_AgeTopAddr = in UInt(MMUAddrWidth bits)
  val TaskQueue_ElemsBase = in UInt(MMUAddrWidth bits)

  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  override def asMaster(): Unit = {
    out(Valid, RegionAttrBase, RegionAttrBiasedBase, RegionAttrShiftBy, HeapRegionBias, HeapRegionShiftBy, HeapRegionBiasedBase, HumongousReclaimCandidatesBoolBase, ParScanThreadStatePtr, TaskQueue_AgeTopAddr, TaskQueue_BottomAddr, TaskQueue_ElemsBase)
    in(Ready, Done)
  }
}

class GCTopIO extends Bundle{
  val mmu2llc = master(new LocalMMUIO)
  val ctrl2top = slave(new Ctrl2Top)
}
