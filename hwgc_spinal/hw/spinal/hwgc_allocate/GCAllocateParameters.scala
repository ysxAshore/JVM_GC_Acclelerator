package hwgc_allocate

import hwgc_top.{GCTopParameters, LocalMMUIO}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCToDoAllocate extends Bundle with GCTopParameters with IMasterSlave {
  val Valid = in Bool ()
  val Ready = out Bool ()

  val Done = out Bool ()

  val regionPtr = in UInt (GCElementWidth bits)
  val allocRegion = in UInt (GCElementWidth bits)

  val NodeIndex = in UInt (8 bits)
  val DestAttrIdx = in UInt (1 bits)
  val MinWordSize = in UInt (GCElementWidth bits)
  val AllocatorPtr = in UInt (GCElementWidth bits)
  val DesiredWordSize = in UInt (GCElementWidth bits)

  val DestObjPtr = out UInt (GCElementWidth bits)
  val ActualPlabSize = out UInt (GCElementWidth bits)

  val updateCacheValid = out Bool ()
  val updateRegionPtr = out UInt (GCElementWidth bits)
  val updateRegion = out UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, DestObjPtr, ActualPlabSize, updateCacheValid, updateRegionPtr, updateRegion)
    out(Valid, regionPtr, allocRegion, NodeIndex, DestAttrIdx, MinWordSize, AllocatorPtr, DesiredWordSize)
  }

  def clearIn(): Unit = {
    Valid := False

    regionPtr := U(0, GCElementWidth bits)
    allocRegion := U(0, GCElementWidth bits)

    NodeIndex := U(0, 8 bits)
    DestAttrIdx := U(0, 1 bits)

    MinWordSize := U(0, GCElementWidth bits)
    AllocatorPtr := U(0, GCElementWidth bits)
    DesiredWordSize := U(0, GCElementWidth bits)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False

    DestObjPtr := U(0, GCElementWidth bits)
    ActualPlabSize := U(0, GCElementWidth bits)

    updateCacheValid := False
    updateRegionPtr := U(0, GCElementWidth bits)
    updateRegion := U(0, GCElementWidth bits)
  }
}

class GCToNewGCAlloc extends Bundle with GCTopParameters with IMasterSlave {
  val Valid = in Bool ()
  val Ready = out Bool ()
  val Done = out Bool ()

  val regionPtr = in UInt (GCElementWidth bits)
  val regionType = in UInt (8 bits)

  val newAllocRegion = out UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, newAllocRegion)
    out(Valid, regionPtr, regionType)
  }

  def clearIn(): Unit = {
    Valid := False
    regionPtr := U(0)
    regionType := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
    newAllocRegion := U(0)
  }
}

class GCToAllocFreeRegion extends Bundle with GCTopParameters with IMasterSlave {
  val Valid = in Bool ()
  val Ready = out Bool ()
  val Done = out Bool ()

  val heapRegionType = in UInt (32 bits)
  val newAllocRegion = out UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    in(Ready, Done, newAllocRegion)
    out(Valid, heapRegionType)
  }

  def clearIn(): Unit = {
    Valid := False
    heapRegionType := U(0)
  }

  def clearOut(): Unit = {
    Ready := False
    Done := False
    newAllocRegion := U(0)
  }
}

class GCDoAllocateConfigIO extends Bundle with GCTopParameters with IMasterSlave {
  val G1h = in UInt (GCElementWidth bits)
  val Thread = in UInt (GCElementWidth bits)
  val LockPtr = in UInt (GCElementWidth bits)
  val DummyRegion = in UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, Thread, LockPtr, DummyRegion)
    in()
  }
}

class GCNewGCAllocConfigIO extends Bundle with GCTopParameters with IMasterSlave {
  val G1h = in UInt (GCElementWidth bits)
  val DummyRegion = in UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, DummyRegion)
    in()
  }
}

class GCAllocFreeRegionConfigIO extends Bundle with GCTopParameters with IMasterSlave {
  val G1h = in UInt (GCElementWidth bits)
  val NumaPtr = in UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, NumaPtr)
    in()
  }
}

class GCAllocateTopConfig extends Bundle with GCTopParameters with IMasterSlave {
  val G1h = in UInt (GCElementWidth bits)
  val Thread = in UInt (GCElementWidth bits)
  val LockPtr = in UInt (GCElementWidth bits)
  val DummyRegion = in UInt (GCElementWidth bits)

  override def asMaster(): Unit = {
    out(G1h, Thread, LockPtr, DummyRegion)
    in()
  }
}

class GCAllocateTopIO(count: Int) extends Bundle {
  val mmu2llc = master(new LocalMMUIO)
  val toDoAllocates = Vec(slave(new GCToDoAllocate), count)
  val config = slave(new GCAllocateTopConfig)
}