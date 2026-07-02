package hwgc_top

import hwgc_acc.GCAccTop
import spinal.core._
import spinal.lib.StreamArbiterFactory

import scala.language.postfixOps

class GCTop extends Module with GCTopParameters{
  val io = new GCTopIO

  val accs = Seq.fill(1)(new GCAccTop)
  val cacheUpdateArb = StreamArbiterFactory.roundRobin.on(
    accs.map(_.io.CacheUpdateOut)
  )

  for (acc <- accs) {
    acc.io.CacheUpdateIn.valid := cacheUpdateArb.valid
    acc.io.CacheUpdateIn.payload := cacheUpdateArb.payload
  }

  cacheUpdateArb.ready := True

  // ---- config: Ctrl2Top → GCAccCtrl2Top + GCAllocateConfig ----
  accs(0).io.config.ChunkSize                          := io.ctrl2top.ChunkSize
  accs(0).io.config.AgeThreshold                       := io.ctrl2top.AgeThreshold
  accs(0).io.config.HeapRegionBias                     := io.ctrl2top.HeapRegionBias
  accs(0).io.config.HeapRegionShiftBy                  := io.ctrl2top.HeapRegionShiftBy
  accs(0).io.config.HeapRegionBiasedBase               := io.ctrl2top.HeapRegionBiasedBase
  accs(0).io.config.RegionAttrShiftBy                  := io.ctrl2top.RegionAttrShiftBy
  accs(0).io.config.RegionAttrBiasedBase               := io.ctrl2top.RegionAttrBiasedBase
  accs(0).io.config.LogOfHRGrainBytes                  := io.ctrl2top.LogOfHRGrainBytes
  accs(0).io.config.StepperOffset                      := io.ctrl2top.StepperOffset
  accs(0).io.config.YoungWordsBase                     := io.ctrl2top.YoungWordsBase
  accs(0).io.config.RegionAttrBase                     := io.ctrl2top.RegionAttrBase
  accs(0).io.config.PlabAllocatorPtr                   := io.ctrl2top.PlabAllocatorPtr
  accs(0).io.config.ParScanThreadStatePtr              := io.ctrl2top.ParScanThreadStatePtr
  accs(0).io.config.TaskQueue_Bottom                   := io.ctrl2top.TaskQueue_Bottom
  accs(0).io.config.TaskQueue_ElemsBase                := io.ctrl2top.TaskQueue_ElemsBase
  accs(0).io.config.HumongousReclaimCandidatesBoolBase := io.ctrl2top.HumongousReclaimCandidatesBoolBase
  accs(0).io.config.CardTablePtr                       := io.ctrl2top.CardTablePtr
  accs(0).io.config.G1h                                := io.ctrl2top.G1h
  accs(0).io.config.DummyRegion                        := io.ctrl2top.DummyRegion
  accs(0).io.config.LockPtr                            := io.ctrl2top.LockPtr
  accs(0).io.config.Thread                             := io.ctrl2top.Thread
  accs(0).io.config.IntArrayKlassObj                   := io.ctrl2top.IntArrayKlassObj
  accs(0).io.config.ObjectKlass                        := io.ctrl2top.ObjectKlass
  accs(0).io.config.CompressedOopBase                  := io.ctrl2top.CompressedOopBase
  accs(0).io.config.CompressedKlassPointerBase         := io.ctrl2top.CompressedKlassPointerBase
  accs(0).io.config.CompressedFlag                     := io.ctrl2top.CompressedFlag
  accs(0).io.config.Valid                              := io.ctrl2top.Valid
  io.ctrl2top.Ready                                    := accs(0).io.config.Ready
  io.ctrl2top.Done                                     := accs(0).io.config.Done

  io.mmu2llc <> accs(0).io.mmu2llc
}

object GCTopVerilog extends App{
  Config.spinal.generateVerilog(new GCTop())
}