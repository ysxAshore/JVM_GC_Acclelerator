package hwgc_allocate

import hwgc_acc.{GCLocalMMU, GCUnalignedMMUAdapter}

import spinal.core._

class GCAllocateTop(count: Int) extends Module {
  val io = new GCAllocateTopIO(count)

  val gcAllocateArb = new GCAllocateArb(count)
  val gcDoAllocate = new GCDoAllocate
  val gcNewGCAlloc = new GCNewGCAlloc
  val gcAllocFreeRegion = new GCAllocFreeRegion

  val gcLocalMMU = new GCLocalMMU(5)
  val gcUnalignedMMUAdapter = Array.fill(5)(new GCUnalignedMMUAdapter)

  for (i <- 0 until 5) {
    gcLocalMMU.io.localMMUIOs(i) <> gcUnalignedMMUAdapter(i).io.out
  }

  gcAllocateArb.io.ToDoAllocates <> io.toDoAllocates
  gcAllocateArb.io.ToDoAllocate <> gcDoAllocate.io.ToDoAllocate

  gcDoAllocate.io.MreqMainIml <> gcUnalignedMMUAdapter(0).io.in
  gcDoAllocate.io.MreqPar <> gcUnalignedMMUAdapter(1).io.in
  gcDoAllocate.io.MreqAttempt <> gcUnalignedMMUAdapter(2).io.in
  gcDoAllocate.io.ToNewGCAlloc <> gcNewGCAlloc.io.ToNewGCAlloc
  gcDoAllocate.io.ConfigIO.G1h := io.config.G1h
  gcDoAllocate.io.ConfigIO.Thread := io.config.Thread
  gcDoAllocate.io.ConfigIO.LockPtr := io.config.LockPtr
  gcDoAllocate.io.ConfigIO.DummyRegion := io.config.DummyRegion

  gcNewGCAlloc.io.Mreq <> gcUnalignedMMUAdapter(3).io.in
  gcNewGCAlloc.io.ToAllocFreeRegion <> gcAllocFreeRegion.io.ToAllocFreeRegion
  gcNewGCAlloc.io.ConfigIO.G1h := io.config.G1h
  gcNewGCAlloc.io.ConfigIO.DummyRegion := io.config.DummyRegion

  gcAllocFreeRegion.io.Mreq <> gcUnalignedMMUAdapter(4).io.in
  gcAllocFreeRegion.io.ConfigIO.G1h := io.config.G1h

  io.mmu2llc <> gcLocalMMU.io.LastLevelCacheTLIO
}
