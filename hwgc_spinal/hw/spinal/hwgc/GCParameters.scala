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
  // 和MMUAddrWidth一样
  val GCElementWidth = 64

  val GCTaskStack_Entry = 64
  val GCTaskStack_SpillNeed = 56
  val GCTaskStack_ReadNeed = 8

  val GCCopyEntry = 64

  /* ----------------- ScannerTask Tag ----------------- */
  val PartialArrayTag = 2
  val GCOopTagWidth = 2

  val NotArrayOop = 0
  val PartialArrayOop = 1
  val GCOopTypeWidth = 1

  val InstanceKlassID = 0
  val InstanceRefKlassID = 1
  val InstanceMirrorKlassID = 2
  val InstanceClassLoaderKlassID = 3
  val TypeArrayKlassID = 4
  val ObjectArrayKlassID = 5

  val PreFetchBuffer = 16
  val PreFetchScanWindow = PreFetchBuffer
  val PreFetchBufferWidth = log2Up(PreFetchBuffer)


  val GCTaskQueue_Size = 1 << 17

  val UINT_MAX = U(BigInt(2147483647L * 2 + 1), 32 bits)
}

trait HWParameters {
  //true is boolean, True is Bool
  val DebugEnable = true

  val MMUAddrWidth = 64
  val MMUDataWidth = 256

  val LineBytesNum = MMUDataWidth / 8
  val LineBytesNumBitSize = log2Up(LineBytesNum) + 1

  val LLCSourceMaxNum = 64
  val LLCSourceMaxNumBitSize = log2Up(LLCSourceMaxNum) + 1

  // helper: issueReq
  def issueReq(port: LocalMMUIO, addr: UInt, Write: Bool, Size: UInt, WriteData: UInt, reqIssued: Bool)(onResp: UInt => Unit): Unit = {

    // ensure default safe values (caller may have already set globals; safe to reassign)
    port.Request.valid := !reqIssued

    when(!reqIssued) {
      port.RequestSize.valid := True
      port.RequestSize.payload := Size.resize(LineBytesNumBitSize)

      port.Request.payload.RequestVirtualAddr := addr
      port.Request.payload.RequestSourceID := port.ConherentRequsetSourceID.payload
      port.Request.payload.RequestType_isWrite := Write
      port.Request.payload.RequestWStrb := getWstrb(Size.resize(LineBytesNumBitSize))
      port.Request.payload.RequestData := WriteData.resize(MMUDataWidth)
    }

    when(port.Request.fire) {
      reqIssued := True
    }

    // handle response (when previously issued)
    when(port.Response.fire) {
      val rd = port.Response.payload.ResponseData
      reqIssued := False
      onResp(rd) // callback to let caller handle response data
    }
  }

  def getWstrb(bytes: UInt): UInt = {
    val mask = Bits(LineBytesNum bits)
    mask := Mux(bytes >= U(LineBytesNum), B(LineBytesNum bits, default -> true), ((U(1) << bytes.resize(LineBytesNumBitSize)) - U(1)).asBits.resize(LineBytesNum))
    mask.asUInt
  }
}

class GCToFetch extends Bundle with GCParameters with IMasterSlave {
  val Pop = Stream(UInt(GCElementWidth bits))
  val PrePop = Stream(UInt(GCElementWidth bits))
  val PushCount = UInt(32 bits)

  override def asMaster(): Unit = {
    master(Pop, PrePop)
    out(PushCount)
  }
}

class GCToStack extends Bundle with GCParameters with IMasterSlave {
  val Push = Stream(UInt(GCElementWidth bits))
  val LastPush = Bool()

  override def asMaster(): Unit = {
    master(Push)
    out(LastPush)
  }
}

class GCToProcessUnit extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()

  val Task = in UInt(GCElementWidth bits)
  val OopType = in UInt(GCOopTypeWidth bits)
  val SrcOopPtr = in UInt(GCElementWidth bits)
  val MarkWord = in UInt(GCElementWidth bits)
  val KlassPtr = in UInt(GCElementWidth bits)
  val SrcLength = in UInt(32 bits)

  override def asMaster(): Unit = {
    in(Ready, Done)
    out(Valid, Task, OopType, SrcOopPtr, MarkWord, KlassPtr, SrcLength)
  }

  def clearIn(): Unit = {
    Valid := False
    Task := U(0)
    OopType := U(0)
    SrcOopPtr := U(0)
    MarkWord := U(0)
    KlassPtr := U(0)
    SrcLength := U(0)
  }

  def clearOut(): Unit = {
    Done := False
    Ready := False
  }
}

class GCToSurvivor extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()
  val DoneOwner = in UInt(1 bits)
  val DestOopPtr = out UInt(GCElementWidth bits)
  val isTypeArray = out Bool()

  val Owner = in UInt(1 bits)
  val MarkWord = in UInt(GCElementWidth bits)
  val KlassPtr = in UInt(GCElementWidth bits)
  val SrcOopPtr = in UInt(GCElementWidth bits)
  val SrcLength = in UInt(32 bits)
  val SrcRegionAttr = in UInt(16 bits)
  val RegionAttrPtr = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DoneOwner, DestOopPtr, isTypeArray)
    out(Valid, Owner, SrcOopPtr, MarkWord, KlassPtr, RegionAttrPtr, SrcLength, SrcRegionAttr)
  }

  def clearIn(): Unit = {
    Valid := False
    Owner := U(0)
    MarkWord := U(0)
    KlassPtr := U(0)
    SrcOopPtr := U(0)
    SrcLength := U(0)
    SrcRegionAttr := U(0)
    RegionAttrPtr := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
    DoneOwner := U(0)
    DestOopPtr := U(0)
    isTypeArray := False
  }
}

class GCWriteSrcOopPtr extends Bundle with GCParameters with IMasterSlave{
  val valid = in Bool()
  val srcOopPtr = in UInt(GCElementWidth bits)
  val writeValue = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    out(valid, srcOopPtr, writeValue)
  }

  def clearIn(): Unit = {
    valid := False
    srcOopPtr := U(0)
    writeValue := U(0)
  }
}

class GCUpdatedRegion extends Bundle with GCParameters with IMasterSlave{
  val Valid0 = in Bool()
  val Valid1 = in Bool()
  val Buffer0 = in UInt(GCElementWidth bits)
  val Buffer1 = in UInt(GCElementWidth bits)
  val RegionTop0 = in UInt(GCElementWidth bits)
  val RegionTop1 = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    out(Valid0, Valid1, Buffer0, Buffer1, RegionTop0, RegionTop1)
  }
}

class GCUpdatedAop extends Bundle with GCParameters with IMasterSlave{
  val Valid0 = in Bool()
  val Valid1 = in Bool()
  val Valid2 = in Bool()
  val Valid3 = in Bool()
  val Addr0 = in UInt (GCElementWidth bits)
  val Addr1 = in UInt(GCElementWidth bits)
  val Addr2 = in UInt(GCElementWidth bits)
  val Addr3 = in UInt(GCElementWidth bits)
  val Data0 = in UInt(GCElementWidth bits)
  val Data1 = in UInt(GCElementWidth * 3 bits)
  val Data2 = in UInt(GCElementWidth * 2 bits)
  val Data3 = in UInt(GCElementWidth * 4 bits)

  override def asMaster(): Unit = {
    out(Valid0, Valid1, Valid2, Valid3, Addr0, Addr1, Addr2, Addr3, Data0, Data1, Data2, Data3)
  }
}

class GCToAllocate extends Bundle with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  val Size = in UInt(32 bits)
  val DestAttrType = in UInt(8 bits)
  val DestObjPtr = out UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DestObjPtr)
    out(Valid, Size, DestAttrType)
  }

  def clearIn(): Unit = {
    Valid := False
    Size := U(0)
    DestAttrType := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
    DestObjPtr := U(0)
  }
}

class GCToParAllocate extends Bundle with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  val NodeIndex = in UInt(8 bits)
  val DestAttrIdx = in UInt(1 bits)
  val MinWordSize = in UInt(GCElementWidth bits)
  val AllocatorPtr = in UInt(GCElementWidth bits)
  val DesiredWordSize = in UInt(GCElementWidth bits)

  val DestObjPtr = out UInt(GCElementWidth bits)
  val ActualPlabSize = out UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DestObjPtr, ActualPlabSize)
    out(Valid, NodeIndex, MinWordSize, DestAttrIdx, AllocatorPtr, DesiredWordSize)
  }

  def clearIn(): Unit = {
    Valid := False
    NodeIndex := U(0)
    MinWordSize := U(0)
    DestAttrIdx := U(0)
    AllocatorPtr := U(0)
    DesiredWordSize := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
    DestObjPtr := U(0)
    ActualPlabSize := U(0)
  }
}

class GCToAttemptAllocate extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  val regionPtr = in UInt(GCElementWidth bits)
  val allocRegion = in UInt(GCElementWidth bits)
  val desiredWordSize = in UInt(GCElementWidth bits)

  val DestObjPtr = out UInt(GCElementWidth bits)
  val ActualPlabSize = out UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DestObjPtr, ActualPlabSize)
    out(Valid, regionPtr, allocRegion, desiredWordSize)
  }

  def clearIn(): Unit = {
    Valid := False
    regionPtr := U(0)
    allocRegion := U(0)
    desiredWordSize := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
    DestObjPtr := U(0)
    ActualPlabSize := U(0)
  }
}

class GCToNewGCAlloc extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  val regionPtr = in UInt(GCElementWidth bits)
  val destAttrIdx = in UInt(1 bits)

  val newAllocRegion = out UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, newAllocRegion)
    out(Valid, regionPtr, destAttrIdx)
  }

  def clearIn(): Unit = {
    Valid := False
    regionPtr := U(0)
    destAttrIdx := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
    newAllocRegion := U(0)
  }
}

class GCToAllocFreeRegion extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  val heapRegionType = in UInt(8 bits)
  val regionNodeIndex = in UInt(32 bits)

  val newAllocRegion = out UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, newAllocRegion)
    out(Valid, heapRegionType, regionNodeIndex)
  }

  def clearIn(): Unit = {
    Valid := False
    heapRegionType := U(0)
    regionNodeIndex := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
    newAllocRegion := U(0)
  }
}

class GCToTrace extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  // some parse module calculate parameters
  val Kid = in UInt(32 bits)
  val OopType = in UInt(GCOopTypeWidth bits)
  val KlassPtr = in UInt(GCElementWidth bits)
  val SrcOopPtr = in UInt(GCElementWidth bits)
  val DestOopPtr = in UInt(GCElementWidth bits)
  val ScanningInYoung = in Bool()

  val StepIndex = in UInt(32 bits)
  val StepNCreate = in UInt(32 bits)
  val ArrayLength = in UInt(32 bits)
  val PartialArrayStart = in UInt(32 bits)

  override def asMaster(): Unit = {
    out(Valid, OopType, KlassPtr, SrcOopPtr, DestOopPtr, Kid, ArrayLength, PartialArrayStart, StepIndex, StepNCreate, ScanningInYoung)
    in(Ready, Done)
  }

  def clearIn(): Unit = {
    Valid := False
    Kid := U(0)
    OopType := U(0)
    KlassPtr := U(0)
    SrcOopPtr := U(0)
    DestOopPtr := U(0)
    ScanningInYoung := False
    StepIndex := U(0)
    StepNCreate := U(0)
    ArrayLength := U(0)
    PartialArrayStart := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
  }
}

class GCToCopy extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  // some parse module calculate parameters
  val DestOopPtr = in UInt(GCElementWidth bits)
  val SrcOopPtr = in UInt(GCElementWidth bits)
  val Size = in UInt(32 bits)

  override def asMaster(): Unit = {
    out(Valid, SrcOopPtr, DestOopPtr, Size)
    in(Ready, Done)
  }

  def clearIn(): Unit = {
    Valid := False
    SrcOopPtr := U(0)
    DestOopPtr := U(0)
    Size := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
  }
}

class GCToAop extends Bundle with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()

  val RegionAttr = in UInt(16 bits)
  val Task = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done)
    out(Valid, RegionAttr, Task)
  }

  def clearIn(): Unit = {
    Valid := False
    Task := U(0)
    RegionAttr := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
  }
}

class GCTaskStackConfigIO extends Bundle with GCParameters with IMasterSlave{
  val TaskQueue_Bottom = in UInt(32 bits)
  val TaskQueue_ElemsBase = in UInt(GCElementWidth bits)

  val TaskValid = in Bool()
  val TaskReady = out Bool()
  val Done = out Bool()

  override def asMaster(): Unit = {
    out(TaskQueue_Bottom, TaskQueue_ElemsBase, TaskValid)
    in(TaskReady, Done)
  }
}

class GCFetchConfigIO extends Bundle with GCParameters with IMasterSlave{
  val UseCompressedOop = in Bool()
  val CompressedOopBase = in UInt(GCElementWidth bits)
  val CompressedOopShift = in UInt(8 bits)
  val UseCompressedKlassPointers = in Bool()

  override def asMaster(): Unit = {
    out(UseCompressedOop, CompressedOopBase, CompressedOopShift, UseCompressedKlassPointers)
    in()
  }
}

class GCArrayProcessConfigIO extends Bundle with GCParameters with IMasterSlave {
  val ChunkSize = in UInt(32 bits)
  val StepperOffset = in UInt(GCElementWidth bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  val HeapRegionBiasedBase = in UInt(GCElementWidth bits)
  val UseCompressedKlassPointers = in Bool()

  override def asMaster(): Unit = {
    out(ChunkSize, StepperOffset, HeapRegionBiasedBase, HeapRegionShiftBy, UseCompressedKlassPointers)
    in()
  }
}

class GCOopProcessConfigIO extends Bundle with GCParameters with IMasterSlave{
  val RegionAttrShiftBy = in UInt(32 bits)
  val RegionAttrBiasedBase = in UInt(GCElementWidth bits)
  val LogOfHRGrainBytes = in UInt(32 bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  val HeapRegionBiasedBase = in UInt(GCElementWidth bits)
  val UseCompressedOop = in Bool()
  val CompressedOopBase = in UInt(GCElementWidth bits)
  val CompressedOopShift = in UInt(8 bits)

  override def asMaster(): Unit = {
    out(RegionAttrBiasedBase, RegionAttrShiftBy, HeapRegionBiasedBase, HeapRegionShiftBy, LogOfHRGrainBytes, UseCompressedOop, CompressedOopBase, CompressedOopShift)
    in()
  }
}

class GCCopy2SurvivorConfigIO extends Bundle with GCParameters with IMasterSlave {
  val ChunkSize = in UInt(32 bits)
  val AgeThreshold = in UInt(32 bits)
  val YoungWordsBase = in UInt(GCElementWidth bits)
  val PlabAllocatorPtr = in UInt(GCElementWidth bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  val HeapRegionBiasedBase = in UInt(GCElementWidth bits)
  val ParScanThreadStatePtr = in UInt(GCElementWidth bits)
  val UseCompressedKlassPointer = in Bool()
  val CompressedKlassPointerBase = in UInt(GCElementWidth bits)
  val CompressedKlassPointerShift = in UInt(8 bits)

  override def asMaster(): Unit = {
    out(ParScanThreadStatePtr, PlabAllocatorPtr, AgeThreshold, HeapRegionBiasedBase, HeapRegionShiftBy, ChunkSize, YoungWordsBase, UseCompressedKlassPointer, CompressedKlassPointerBase, CompressedKlassPointerShift)
    in()
  }
}

class GCAllocateConfigIO extends Bundle with GCParameters with IMasterSlave{
  val G1h = in UInt(GCElementWidth bits)
  val objectKlassObj = in UInt(GCElementWidth bits)
  val intArrayKlassObj = in UInt(GCElementWidth bits)
  val PlabAllocatorPtr = in UInt(GCElementWidth bits)
  val UseCompressedKlassPointers = in Bool()
  val CompressedKlassPointerBase = in UInt(GCElementWidth bits)
  val CompressedKlassPointerShift = in UInt(8 bits)

  override def asMaster(): Unit = {
    out(G1h, objectKlassObj, intArrayKlassObj, PlabAllocatorPtr, UseCompressedKlassPointers, CompressedKlassPointerBase, CompressedKlassPointerShift)
    in()
  }
}

class GCParAllocateConfigIO extends Bundle with GCParameters with IMasterSlave{
  val G1h = in UInt(GCElementWidth bits)
  val Thread = in UInt(GCElementWidth bits)
  val LockPtr = in UInt(GCElementWidth bits)
  val DummyRegion = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, Thread, LockPtr, DummyRegion)
    in()
  }
}

class GCAttemptAllocConfigIO extends Bundle with GCParameters with IMasterSlave{
  val G1h = in UInt(GCElementWidth bits)
  val DummyRegion = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, DummyRegion)
    in()
  }
}

class GCNewGCAllocConfigIO extends Bundle with GCParameters with IMasterSlave{
  val G1h = in UInt(GCElementWidth bits)
  val DummyRegion = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, DummyRegion)
    in()
  }
}

class GCAllocFreeRegionConfigIO extends Bundle with GCParameters with IMasterSlave{
  val G1h = in UInt(GCElementWidth bits)
  val NumaPtr = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, NumaPtr)
    in()
  }
}

class GCTraceConfigIO extends Bundle with GCParameters with IMasterSlave{
  val ChunkSize = in UInt(32 bits)
  val RegionAttrBase = in UInt(GCElementWidth bits)
  val RegionAttrShiftBy = in UInt(32 bits)
  val RegionAttrBiasedBase = in UInt(GCElementWidth bits)
  val HeapRegionBias = in UInt(32 bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  val LogOfHRGrainBytes = in UInt(32 bits)
  val UseCompressedOops = in Bool()
  val CompressedOopBase = in UInt(GCElementWidth bits)
  val CompressedOopShift = in UInt(8 bits)
  val UseCompressedKlassPointers = in Bool()
  val HumongousReclaimCandidatesBoolBase = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    out(ChunkSize, UseCompressedOops, CompressedOopBase, CompressedOopShift, UseCompressedKlassPointers, RegionAttrBase, RegionAttrBiasedBase, RegionAttrShiftBy, HeapRegionBias, HeapRegionShiftBy, HumongousReclaimCandidatesBoolBase, LogOfHRGrainBytes)
    in()
  }
}


class GCAopConfigIO extends Bundle with GCParameters with IMasterSlave{
  val CardTablePtr = in UInt(GCElementWidth bits)
  val ParScanThreadStatePtr = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    out(CardTablePtr, ParScanThreadStatePtr)
    in()
  }
}


class LocalMMUIO extends Bundle with HWParameters with IMasterSlave{
  //发出的访存请求
  val Request = master Stream new Bundle{
    val RequestSourceID = UInt(LLCSourceMaxNumBitSize bits)
    val RequestVirtualAddr = UInt(MMUAddrWidth bits)
    val RequestType_isWrite = Bool()
    val RequestData = UInt(MMUDataWidth bits)
    val RequestWStrb = UInt(LineBytesNum bits)
  }

  // add variable to describe the request need or not need split two request
  val RequestSize = master Flow UInt(LineBytesNumBitSize bits)

  val ConherentRequsetSourceID    = slave Flow UInt(LLCSourceMaxNumBitSize bits)

  val Response = slave Stream new Bundle{
    val ResponseData = UInt(MMUDataWidth bits)
    val ResponseSourceID = UInt(LLCSourceMaxNumBitSize bits)
  }

  override def asMaster(): Unit = {
    master(Request, RequestSize)
    slave(Response, ConherentRequsetSourceID)
  }
}

object WrapInc
{
  // "n" is the number of increments, so we wrap at n-1.
  def apply(value: UInt, n: Int, Increment: UInt): UInt = {
    if (isPow2(n)) {
      (value + Increment)(log2Up(n)-1 downto  0)
    } else {
      val wrap = value === U(n-1)
      Mux(wrap, U(0), value + Increment)
    }
  }
}
object WrapDec {
  // "n" is the number of elements, valid range is 0 ~ n-1.
  // Decrement with wrap:
  //    if value == 0 → n-1
  //    else          → value - 1
  def apply(value: UInt, n: Int, decrement: UInt): UInt = {
    if (isPow2(n)) {
      // 对于 2 的幂，直接做减法并截位即可
      (value - decrement)(log2Up(n)-1 downto 0)
    } else {
      // 非 2 的幂，用条件判断
      val wrap = value === U(0)
      Mux(wrap, U(n-1), value - decrement)
    }
  }
}


object LocalMMUTaskType {
  val TaskTypeMax = 11 + 2
  val TaskTypeBitWidth = log2Up(TaskTypeMax)
}

class Ctrl2Top extends Bundle with GCParameters with IMasterSlave {
  val ChunkSize = in UInt(32 bits)
  val AgeThreshold = in UInt(32 bits)
  val HeapRegionBias = in UInt(32 bits)
  val RegionAttrShiftBy = in UInt(32 bits)
  val HeapRegionShiftBy = in UInt(32 bits)
  val LogOfHRGrainBytes = in UInt(32 bits)
  val StepperOffset = in UInt(GCElementWidth bits)
  val YoungWordsBase = in UInt(GCElementWidth bits)
  val RegionAttrBase = in UInt(GCElementWidth bits)
  val PlabAllocatorPtr = in UInt(GCElementWidth bits)
  val RegionAttrBiasedBase = in UInt(GCElementWidth bits)
  val HeapRegionBiasedBase = in UInt(GCElementWidth bits)
  val ParScanThreadStatePtr = in UInt(GCElementWidth bits)
  val TaskQueue_Bottom    = in UInt(32 bits)
  val TaskQueue_ElemsBase = in UInt(GCElementWidth bits)
  val HumongousReclaimCandidatesBoolBase = in UInt(GCElementWidth bits)
  val CardTablePtr = in UInt(GCElementWidth bits)
  val G1h = in UInt(GCElementWidth bits)
  val IntArrayKlassObj = in UInt(GCElementWidth bits)
  val ObjectKlass = in UInt(GCElementWidth bits)
  val LockPtr = in UInt(GCElementWidth bits)
  val Thread = in UInt(GCElementWidth bits)
  val DummyRegion = in UInt(GCElementWidth bits)
  val NumaPtr = in UInt(GCElementWidth bits)
  val CompressedOopBase = in UInt(GCElementWidth bits)
  val CompressedKlassPointerBase = in UInt(GCElementWidth bits)
  val CompressedFlag = in UInt(32 bits)

  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  override def asMaster(): Unit = {
    out(Valid, ChunkSize, AgeThreshold, HeapRegionBias, RegionAttrShiftBy, HeapRegionShiftBy, LogOfHRGrainBytes, StepperOffset, YoungWordsBase, RegionAttrBase, PlabAllocatorPtr, RegionAttrBiasedBase, HeapRegionBiasedBase, ParScanThreadStatePtr, TaskQueue_Bottom, TaskQueue_ElemsBase, HumongousReclaimCandidatesBoolBase, CardTablePtr, G1h, IntArrayKlassObj, ObjectKlass, LockPtr, Thread, DummyRegion, NumaPtr, CompressedOopBase, CompressedKlassPointerBase, CompressedFlag)
    in(Ready, Done)
  }
}

class GCTopIO extends Bundle{
  val mmu2llc = master(new LocalMMUIO)
  val ctrl2top = slave(new Ctrl2Top)
}