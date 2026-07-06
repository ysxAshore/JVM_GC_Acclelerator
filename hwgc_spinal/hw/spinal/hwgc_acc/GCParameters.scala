package hwgc_acc

import hwgc_top.{Ctrl2Top, GCAllocCacheUpdate, GCTopParameters, LocalMMUIO}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

trait GCParameters {
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

  val PreFetchBufferNum = 16
  val PreFetchScanWindow = PreFetchBufferNum
  val PreFetchBufferWidth = log2Up(PreFetchBufferNum)

  val GCTaskQueue_Size = 1 << 17
}

class GCToFetch extends Bundle with GCTopParameters with IMasterSlave {
  val Pop = Stream(UInt(GCElementWidth bits))
  val PrePop = Stream(UInt(GCElementWidth bits))
  val PushCount = UInt(32 bits)

  override def asMaster(): Unit = {
    master(Pop, PrePop)
    out(PushCount)
  }
}

class GCToStack extends Bundle with GCTopParameters with IMasterSlave {
  val Push = Stream(UInt(GCElementWidth bits))
  val LastPush = Bool()

  override def asMaster(): Unit = {
    master(Push)
    out(LastPush)
  }
}

case class GCToProcessUnitPayload() extends Bundle with GCTopParameters with GCParameters {
  val Task = UInt(GCElementWidth bits)
  val OopType = UInt(GCOopTypeWidth bits)
  val SrcOopPtr = UInt(GCElementWidth bits)
  val MarkWord = UInt(GCElementWidth bits)
  val KlassPtr = UInt(GCElementWidth bits)
  val SrcLength = UInt(32 bits)
}

case class GCToProcessUnit() extends Bundle with IMasterSlave {
  val cmd = Stream(GCToProcessUnitPayload())
  val Done = Bool()

  override def asMaster(): Unit = {
    master(cmd)
    in(Done)
  }

  def clearIn(): Unit = {
    cmd.valid := False
    cmd.payload.clearAll()
  }

  def clearOut(): Unit = {
    cmd.ready := False
    Done := False
  }
}

case class GCToSurvivorPayload() extends Bundle with GCTopParameters  {
  val Owner = UInt (1 bits)
  val MarkWord = UInt (GCElementWidth bits)
  val KlassPtr = UInt (GCElementWidth bits)
  val SrcOopPtr = UInt (GCElementWidth bits)
  val SrcLength = UInt (32 bits)
  val SrcRegionAttr = UInt (16 bits)
  val RegionAttrPtr = UInt (GCElementWidth bits)
}

case class GCToSurvivorDonePayload() extends Bundle with GCTopParameters {
  val DoneOwner = UInt(1 bits)
  val DestOopPtr = UInt(GCElementWidth bits)
  val isTypeArray = Bool()
}

case class GCToSurvivor() extends Bundle with IMasterSlave {

  val cmd = Stream(GCToSurvivorPayload())
  val done = Flow(GCToSurvivorDonePayload())

  override def asMaster(): Unit = {
    master(cmd)
    slave(done)
  }

  def clearIn(): Unit = {
    cmd.valid := False
    cmd.payload.clearAll()
  }

  def clearOut(): Unit = {
    cmd.ready := False
    done.valid := False
    done.payload.clearAll()
  }
}

case class GCWriteSrcOopPtrPayload() extends Bundle with GCTopParameters {
  val srcOopPtr = UInt(GCElementWidth bits)
  val writeValue = UInt(GCElementWidth bits)
}

case class GCWriteSrcOopPtr() extends Bundle with IMasterSlave {
  val writeForward = Flow(GCWriteSrcOopPtrPayload())

  override def asMaster(): Unit = {
    master(writeForward)
  }

  def clearIn(): Unit = {
    writeForward.clearAll()
  }
}

case class GCToAllocatePayload() extends Bundle with GCTopParameters {
  val Size = UInt(32 bits)
  val DestAttrType = UInt(8 bits)
}

case class GCToAllocateDonePayload() extends Bundle with GCTopParameters {
  val DestOopPtr = UInt(GCElementWidth bits)
  val PlabRefillFailed = Bool()
}

case class GCToAllocate() extends Bundle with IMasterSlave {
  val cmd = Stream(GCToAllocatePayload())
  val done = Flow(GCToAllocateDonePayload())

  override def asMaster(): Unit = {
    master(cmd)
    slave(done)
  }

  def clearIn(): Unit = {
    cmd.valid := False
    cmd.payload.clearAll()
  }

  def clearOut(): Unit = {
    cmd.ready := False
    done.clearAll()
  }
}

case class GCToParAllocatePayload() extends Bundle with GCTopParameters {
  val DestAttrIdx = UInt (1 bits)
  val MinWordSize = UInt (GCElementWidth bits)
  val AllocatorPtr = UInt (GCElementWidth bits)
  val DesiredWordSize = UInt (GCElementWidth bits)
}

case class GCToParAllocateDonePayload() extends Bundle with GCTopParameters {
  val DestObjPtr = UInt (GCElementWidth bits)
  val ActualPlabSize = UInt (GCElementWidth bits)
}

case class GCToParAllocate() extends Bundle with IMasterSlave {
  val cmd = Stream(GCToParAllocatePayload())
  val done = Flow(GCToParAllocateDonePayload())

  override def asMaster(): Unit = {
    master(cmd)
    slave(done)
  }

  def clearIn(): Unit = {
    cmd.valid := False
    cmd.payload.clearAll()
  }

  def clearOut(): Unit = {
    cmd.ready := False
    done.clearAll()
  }
}

case class GCToTracePayload() extends Bundle with GCTopParameters with GCParameters {
  val Kid = UInt (32 bits)
  val OopType = UInt (GCOopTypeWidth bits)
  val KlassPtr = UInt (GCElementWidth bits)
  val SrcOopPtr = UInt (GCElementWidth bits)
  val DestOopPtr = UInt (GCElementWidth bits)
  val ScanningInYoung = Bool ()

  val StepIndex = UInt (32 bits)
  val StepNCreate = UInt (32 bits)
  val ArrayLength = UInt (32 bits)
  val PartialArrayStart = UInt (32 bits)
}

case class GCToTrace() extends Bundle with IMasterSlave {
  val cmd = Stream(GCToTracePayload())
  val Done = Bool ()

  override def asMaster(): Unit = {
    master(cmd)
    in(Done)
  }

  def clearIn(): Unit = {
    cmd.valid := False
    cmd.payload.clearAll()
  }

  def clearOut(): Unit = {
    cmd.ready := False
    Done := False
  }
}

case class GCToCopyPayload() extends Bundle with GCTopParameters {
  val DestOopPtr = UInt (GCElementWidth bits)
  val SrcOopPtr = UInt (GCElementWidth bits)
  val Size = UInt (32 bits)
}

case class GCToCopy() extends Bundle with IMasterSlave {
  val cmd = Stream(GCToCopyPayload())
  val Done = Bool ()

  override def asMaster(): Unit = {
    master(cmd)
    in(Done)
  }

  def clearIn(): Unit = {
    cmd.valid := False
    cmd.payload.clearAll()
  }

  def clearOut(): Unit = {
    cmd.ready := False
    Done := False
  }
}

case class GCToAopPayload() extends Bundle with GCTopParameters {
  val RegionAttr = UInt (16 bits)
  val Task = UInt (GCElementWidth bits)
}

case class GCToAop() extends Bundle with GCTopParameters with IMasterSlave {
  val cmd = Stream(GCToAopPayload())
  val Done = Bool ()

  override def asMaster(): Unit = {
    in(Done)
    master(cmd)
  }

  def clearIn(): Unit = {
    cmd.valid := False
    cmd.payload.clearAll()
  }

  def clearOut(): Unit = {
    cmd.ready := False
    Done := False
  }
}

case class GCToNewGCAllocPayload() extends Bundle with GCTopParameters {
  val RegionPtr = UInt (GCElementWidth bits)
  val RegionType = UInt (8 bits)
}

case class GCToNewGCAllocDonePayload() extends Bundle with GCTopParameters {
  val NewAllocRegion = out UInt (GCElementWidth bits)
}

case class GCToNewGCAlloc() extends Bundle with GCTopParameters with IMasterSlave {
  val cmd = Stream(GCToNewGCAllocPayload())
  val done = Flow(GCToNewGCAllocDonePayload())

  override def asMaster(): Unit = {
    master(cmd)
    slave(done)
  }

  def clearIn(): Unit = {
    cmd.valid := False
    cmd.payload.clearAll()
  }

  def clearOut(): Unit = {
    cmd.ready := False
    done.clearAll()
  }
}

case class GCTaskStackConfigPayload() extends Bundle with GCTopParameters {
  val TaskQueue_Bottom = UInt (32 bits)
  val TaskQueue_ElemsBase = UInt (GCElementWidth bits)
}

case class GCTaskStackConfigIO() extends Bundle with IMasterSlave {
  val config = Stream(GCTaskStackConfigPayload())
  val Done = Bool ()

  override def asMaster(): Unit = {
    master(config)
    in(Done)
  }
}

case class GCFetchConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val UseCompressedOop = Bool ()
  val CompressedOopBase = UInt (GCElementWidth bits)
  val CompressedOopShift = UInt (8 bits)
  val UseCompressedKlassPointers = Bool ()

  override def asMaster(): Unit = {
    out(UseCompressedOop, CompressedOopBase, CompressedOopShift, UseCompressedKlassPointers)
  }
}

case class GCArrayProcessConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val ChunkSize = UInt (32 bits)
  val StepperOffset = UInt (GCElementWidth bits)
  val HeapRegionShiftBy = UInt (32 bits)
  val HeapRegionBiasedBase = UInt (GCElementWidth bits)
  val UseCompressedKlassPointers = Bool ()

  override def asMaster(): Unit = {
    out(ChunkSize, StepperOffset, HeapRegionBiasedBase, HeapRegionShiftBy, UseCompressedKlassPointers)
  }
}

case class GCOopProcessConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val RegionAttrShiftBy = UInt (32 bits)
  val RegionAttrBiasedBase = UInt (GCElementWidth bits)
  val LogOfHRGrainBytes = UInt (32 bits)
  val HeapRegionShiftBy = UInt (32 bits)
  val HeapRegionBiasedBase = UInt (GCElementWidth bits)
  val UseCompressedOop = Bool ()
  val CompressedOopBase = UInt (GCElementWidth bits)
  val CompressedOopShift = UInt (8 bits)

  override def asMaster(): Unit = {
    out(RegionAttrBiasedBase, RegionAttrShiftBy, HeapRegionBiasedBase, HeapRegionShiftBy, LogOfHRGrainBytes, UseCompressedOop, CompressedOopBase, CompressedOopShift)
  }
}

case class GCCopy2SurvivorConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val ChunkSize = UInt (32 bits)
  val AgeThreshold = UInt (32 bits)
  val YoungWordsBase = UInt (GCElementWidth bits)
  val PlabAllocatorPtr = UInt (GCElementWidth bits)
  val HeapRegionShiftBy = UInt (32 bits)
  val HeapRegionBiasedBase = UInt (GCElementWidth bits)
  val ParScanThreadStatePtr = UInt (GCElementWidth bits)
  val UseCompressedKlassPointer = Bool ()
  val CompressedKlassPointerBase = UInt (GCElementWidth bits)
  val CompressedKlassPointerShift = UInt (8 bits)
  val ObjectKlassObj = UInt (GCElementWidth bits)
  val IntArrayKlassObj = UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(ParScanThreadStatePtr, PlabAllocatorPtr, AgeThreshold, HeapRegionBiasedBase, HeapRegionShiftBy, ChunkSize,
      YoungWordsBase, UseCompressedKlassPointer, CompressedKlassPointerBase, CompressedKlassPointerShift, ObjectKlassObj, IntArrayKlassObj)
  }
}

case class GCAllocateConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val G1h = UInt (GCElementWidth bits)
  val ObjectKlassObj = UInt (GCElementWidth bits)
  val IntArrayKlassObj = UInt (GCElementWidth bits)
  val PlabAllocatorPtr = UInt (GCElementWidth bits)
  val UseCompressedKlassPointers = Bool ()
  val CompressedKlassPointerBase = UInt (GCElementWidth bits)
  val CompressedKlassPointerShift = UInt (8 bits)

  override def asMaster(): Unit = {
    out(G1h, ObjectKlassObj, IntArrayKlassObj, PlabAllocatorPtr, UseCompressedKlassPointers, CompressedKlassPointerBase, CompressedKlassPointerShift)
  }
}

case class GCTraceConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val ChunkSize = UInt (32 bits)
  val RegionAttrBase = UInt (GCElementWidth bits)
  val RegionAttrShiftBy = UInt (32 bits)
  val RegionAttrBiasedBase = UInt (GCElementWidth bits)
  val HeapRegionBias = UInt (32 bits)
  val HeapRegionShiftBy = UInt (32 bits)
  val LogOfHRGrainBytes = UInt (32 bits)
  val UseCompressedOops = Bool ()
  val CompressedOopBase = UInt (GCElementWidth bits)
  val CompressedOopShift = UInt (8 bits)
  val UseCompressedKlassPointers = Bool ()
  val HumongousReclaimCandidatesBoolBase = UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(ChunkSize, UseCompressedOops, CompressedOopBase,
      CompressedOopShift, UseCompressedKlassPointers, RegionAttrBase, RegionAttrBiasedBase, RegionAttrShiftBy,
      HeapRegionBias, HeapRegionShiftBy, HumongousReclaimCandidatesBoolBase, LogOfHRGrainBytes)
  }
}

case class GCAopConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val CardTablePtr = UInt (GCElementWidth bits)
  val ParScanThreadStatePtr = UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(CardTablePtr, ParScanThreadStatePtr)
  }
}

case class GCDoAllocateConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val G1h = UInt (GCElementWidth bits)
  val Thread = UInt (GCElementWidth bits)
  val LockPtr = UInt (GCElementWidth bits)
  val DummyRegion = UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, Thread, LockPtr, DummyRegion)
  }
}

case class GCNewGCAllocConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val G1h = UInt (GCElementWidth bits)
  val DummyRegion = UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, DummyRegion)
  }
}

case class GCAllocFreeRegionConfigIO() extends Bundle with GCTopParameters with IMasterSlave {
  val G1h = UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h)
  }
}


class GCAccTopIO extends Bundle {
  val mmu2llc = master(new LocalMMUIO)
  val config = slave(new Ctrl2Top)
  val CacheUpdateOut = master(Stream(new GCAllocCacheUpdate))
  val CacheUpdateIn = slave(Flow(new GCAllocCacheUpdate))
}