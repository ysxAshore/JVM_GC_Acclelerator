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
  val GCTaskStack_SpillNeed = 63
  val GCTaskStack_ReadNeed = 8

  val GCoopWorkStages = 7
  val GCaopWorkStages = 9
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

  // OopDesc
  // --- Common Oop
  val MarkWordOff = 0
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

  val GCTaskQueue_Size = 1 << 17
  val GCScannerTask_Size = 8
  val GCObjectPtr_Size = 8
  val LogHeapWordSize = log2Up(GCObjectPtr_Size)
  val GCHeapRegionAttr_Size = 2

  val UINT_MAX = U(BigInt(2147483647L * 2 + 1), 32 bits)
}

trait HWParameters {
  //true is boolean, True is Bool
  val DebugEnable = true

  val MMUAddrWidth = 64
  val MMUDataWidth = 256

  val LLCSourceMaxNum = 64
  val LLCSourceMaxNumBitSize = log2Up(LLCSourceMaxNum) + 1

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
      port.Request.payload.RequestData := WriteData.resize(MMUDataWidth bits)
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

  def getWstrb(bytes: UInt): UInt = {
    val mask = Bits(MMUDataWidth / 8 bits)
    mask := Mux(bytes >= U(MMUDataWidth / 8), B(MMUDataWidth / 8 bits, default -> true), ((U(1) << bytes.resize(6 bits)) - U(1)).asBits.resize(MMUDataWidth / 8 bits))
    mask.asUInt
  }
}

class GCFetch2ProcessUnit extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()

  val Task = in UInt(GCElementWidth bits)
  val OopType = in UInt(GCOopTypeWidth bits)
  val SrcOopPtr = in UInt(GCElementWidth bits)
  val MarkWord = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done)
    out(Valid, Task, OopType, SrcOopPtr, MarkWord)
  }
}

class GCProcess2Survivor extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()
  val DestOopPtr = out UInt(GCElementWidth bits)

  val MarkWord = in UInt(GCElementWidth bits)
  val SrcOopPtr = in UInt(GCElementWidth bits)
  val RegionAttrPtr = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DestOopPtr)
    out(Valid, SrcOopPtr, MarkWord, RegionAttrPtr)
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
}

class GCToParAllocate extends Bundle with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  val excuteAll = in Bool()
  val botUpdates = in Bool()
  val allocRegion = in UInt(GCElementWidth bits)
  val minWordSize = in UInt(GCElementWidth bits)
  val desiredWordSize = in UInt(GCElementWidth bits)

  val DestObjPtr = out UInt(GCElementWidth bits)
  val ActualPlabSize = out UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DestObjPtr, ActualPlabSize)
    out(Valid, excuteAll, botUpdates, allocRegion, minWordSize, desiredWordSize)
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
}

class GCToNewGCAlloc extends Bundle with GCParameters with IMasterSlave {
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  val regionPtr = in UInt(GCElementWidth bits)

  val newAllocRegion = out UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, newAllocRegion)
    out(Valid, regionPtr)
  }
}

class GCToAllocFreeRegion extends Bundle with GCParameters with IMasterSlave{
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

}

class GCToTrace extends Bundle with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()
  val Done = out Bool()

  // some parse module caculate parameters
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
}

class GCToCopy extends Bundle with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()

  // some parse module caculate parameters
  val DestOopPtr = in UInt(GCElementWidth bits)
  val SrcOopPtr = in UInt(GCElementWidth bits)
  val Size = in UInt(32 bits)

  override def asMaster(): Unit = {
    out(Valid, SrcOopPtr, DestOopPtr, Size)
    in(Ready, Done)
  }
}

class GCTaskStackConfigIO extends Bundle with GCParameters with IMasterSlave{
  val TaskQueue_BottomAddr = in UInt(GCElementWidth bits)
  val TaskQueue_ElemsBase = in UInt(GCElementWidth bits)

  val TaskValid = in Bool()
  val TaskReady = out Bool()
  val Done = out Bool()

  override def asMaster(): Unit = {
    out(TaskQueue_BottomAddr, TaskQueue_ElemsBase, TaskValid)
    in(TaskReady, Done)
  }
}

class GCFetchConfigIO extends Bundle with GCParameters with IMasterSlave{
  val UseCompressedOop = in Bool()
  val CompressedOopBase = in UInt(GCElementWidth bits)
  val CompressedOopShift = in UInt(8 bits)

  override def asMaster(): Unit = {
    out(UseCompressedOop, CompressedOopBase, CompressedOopShift)
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
  val Thread = in UInt(GCElementWidth bits)
  val LockPtr = in UInt(GCElementWidth bits)
  val objectKlassObj = in UInt((GCElementWidth bits))
  val intArrayKlassObj = in UInt((GCElementWidth bits))
  val PlabAllocatorPtr = in UInt(GCElementWidth bits)
  val UseCompressedKlassPointers = in Bool()
  val CompressedKlassPointerBase = in UInt(GCElementWidth bits)
  val CompressedKlassPointerShift = in UInt(8 bits)

  override def asMaster(): Unit = {
    out(G1h, Thread, LockPtr, objectKlassObj, intArrayKlassObj, PlabAllocatorPtr, UseCompressedKlassPointers, CompressedKlassPointerBase, CompressedKlassPointerShift)
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

class TraceMReq2MMU(oopWorkStage:Int) extends Bundle with IMasterSlave {
  val commonMReq  = new LocalMMUIO
  val oopWorkMReqs  = Vec.fill(oopWorkStage)(new LocalMMUIO)

  override def asMaster(): Unit = {
    master(commonMReq)
    oopWorkMReqs.foreach(master(_))
  }
}

case class GCAopPayload() extends Bundle with GCParameters{
  val RegionAttr = UInt(16 bits)
  val Task = UInt(GCElementWidth bits)
}

class ToAopParameters extends Bundle with GCParameters with IMasterSlave{
  val Valid = in Bool()
  val Ready = out Bool()

  val Done = out Bool()

  val RegionAttr = in UInt(16 bits)
  val Task = in UInt(GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done)
    out(Valid, RegionAttr, Task)
  }
}

class LocalMMUIO extends Bundle with HWParameters with IMasterSlave{
  //发出的访存请求
  val Request = master Stream(new Bundle{
    val RequestSourceID = UInt(LLCSourceMaxNumBitSize bits)
    val RequestVirtualAddr = UInt(MMUAddrWidth bits)
    val RequestType_isWrite = Bool()
    val RequestData = UInt(MMUDataWidth bits)
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
  //Aop 9
  //ArrayProcess 1
  //Copy 1write + 1read = 2
  //Fetch 1
  //OopCopy2Survivor 1 + 4special = 5
  //OopProcess 1
  //TaskStack 1
  //Trace 7 + 1
  val TaskTypeMax = 9 + 1 + 2 + 1 + 1 + 4 + 1 + 1 + 7 + 1
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
  val TaskQueue_BottomAddr = in UInt(GCElementWidth bits)
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
    out(Valid, AgeThreshold, HeapRegionBias, RegionAttrShiftBy, HeapRegionShiftBy, LogOfHRGrainBytes, StepperOffset, YoungWordsBase, RegionAttrBase, PlabAllocatorPtr, RegionAttrBiasedBase, HeapRegionBiasedBase, ParScanThreadStatePtr, TaskQueue_BottomAddr, TaskQueue_ElemsBase, HumongousReclaimCandidatesBoolBase, CardTablePtr, G1h, IntArrayKlassObj, ObjectKlass, LockPtr, Thread, DummyRegion, NumaPtr, CompressedOopBase, CompressedKlassPointerBase, CompressedFlag)
    in(Ready, Done)
  }
}

class GCTopIO extends Bundle{
  val mmu2llc = master(new LocalMMUIO)
  val ctrl2top = slave(new Ctrl2Top)
}