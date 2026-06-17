package hwgc_top

import hwgc_acc.GCAccTop
import hwgc_allocate.GCAllocateTop

import spinal.core._

import scala.language.postfixOps

class GCTop extends Module with GCTopParameters{
  val io = new GCTopIO

  val gcAcc = new GCAccTop
  val gcAllocate = new GCAllocateTop(1)

  gcAcc.io.toDoAllocate <> gcAllocate.io.toDoAllocates(0)

  // ---- config: Ctrl2Top → GCAccCtrl2Top + GCAllocateConfig ----
  gcAcc.io.config.ChunkSize                         := io.ctrl2top.ChunkSize
  gcAcc.io.config.AgeThreshold                      := io.ctrl2top.AgeThreshold
  gcAcc.io.config.HeapRegionBias                    := io.ctrl2top.HeapRegionBias
  gcAcc.io.config.HeapRegionShiftBy                 := io.ctrl2top.HeapRegionShiftBy
  gcAcc.io.config.HeapRegionBiasedBase              := io.ctrl2top.HeapRegionBiasedBase
  gcAcc.io.config.RegionAttrShiftBy                 := io.ctrl2top.RegionAttrShiftBy
  gcAcc.io.config.RegionAttrBiasedBase              := io.ctrl2top.RegionAttrBiasedBase
  gcAcc.io.config.LogOfHRGrainBytes                 := io.ctrl2top.LogOfHRGrainBytes
  gcAcc.io.config.StepperOffset                     := io.ctrl2top.StepperOffset
  gcAcc.io.config.YoungWordsBase                     := io.ctrl2top.YoungWordsBase
  gcAcc.io.config.RegionAttrBase                     := io.ctrl2top.RegionAttrBase
  gcAcc.io.config.PlabAllocatorPtr                   := io.ctrl2top.PlabAllocatorPtr
  gcAcc.io.config.ParScanThreadStatePtr              := io.ctrl2top.ParScanThreadStatePtr
  gcAcc.io.config.TaskQueue_Bottom                   := io.ctrl2top.TaskQueue_Bottom
  gcAcc.io.config.TaskQueue_ElemsBase                := io.ctrl2top.TaskQueue_ElemsBase
  gcAcc.io.config.HumongousReclaimCandidatesBoolBase := io.ctrl2top.HumongousReclaimCandidatesBoolBase
  gcAcc.io.config.CardTablePtr                       := io.ctrl2top.CardTablePtr
  gcAcc.io.config.G1h                                := io.ctrl2top.G1h
  gcAcc.io.config.IntArrayKlassObj                   := io.ctrl2top.IntArrayKlassObj
  gcAcc.io.config.ObjectKlass                        := io.ctrl2top.ObjectKlass
  gcAcc.io.config.CompressedOopBase                  := io.ctrl2top.CompressedOopBase
  gcAcc.io.config.CompressedKlassPointerBase         := io.ctrl2top.CompressedKlassPointerBase
  gcAcc.io.config.CompressedFlag                     := io.ctrl2top.CompressedFlag
  gcAcc.io.config.Valid                              := io.ctrl2top.Valid
  io.ctrl2top.Ready                                  := gcAcc.io.config.Ready
  io.ctrl2top.Done                                   := gcAcc.io.config.Done

  // GCAllocateConfig: 纯组合逻辑配置，无握手
  gcAllocate.io.config.G1h          := io.ctrl2top.G1h
  gcAllocate.io.config.Thread       := io.ctrl2top.Thread
  gcAllocate.io.config.LockPtr      := io.ctrl2top.LockPtr
  gcAllocate.io.config.DummyRegion  := io.ctrl2top.DummyRegion

  // ---- MMU 仲裁: gcAcc.mmu2llc + gcAllocate.mmu2llc → io.mmu2llc ----
  val mmuArb = new GCTopMMUArb
  // gcAcc → port 0
  mmuArb.io.localMMUIOs(0) <> gcAcc.io.mmu2llc
  // gcAllocate → port 1
  mmuArb.io.localMMUIOs(1) <> gcAllocate.io.mmu2llc

  // 仲裁后输出到顶层
  mmuArb.io.LastLevelCacheTLIO <> io.mmu2llc
}

object GCTopVerilog extends App{
  Config.spinal.generateVerilog(new GCTop())
}