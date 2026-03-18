package hwgc

import spinal.core._

import scala.language.postfixOps

class GCTop extends Module with GCParameters with HWParameters {
  val io = new GCTopIO

  val gcTaskStack = new GCTaskStack
  val gcFetch = new GCFetch
  val gcArrayProcess = new GCArrayProcess
  val gcOopProcess = new GCOopProcess
  val gcOopCopy2Survivor = new GCOopCopy2Survivor
  val gcAllocate = new GCAllocate
  val gcParAllocate = new GCParAllocate
  val gcAttemptAlloc = new GCAttemptAlloc
  val gcNewGCAlloc = new GCNewGCAlloc
  val gcAllocFreeRegion = new GCAllocFreeRegion
  val gcCopy = new GCCopy
  val gcTrace = new GCTrace
  val gcAop = new GCAop
  val gcLocalMMU = new GCLocalMMU
  val gcUnalignedMMUAdapter = Array.fill(LocalMMUTaskType.TaskTypeMax)(new GCUnalignedMMUAdapter)

  for(i <- 0 until LocalMMUTaskType.TaskTypeMax){
    gcLocalMMU.io.localMMUIOs(i) <> gcUnalignedMMUAdapter(i).io.out
  }

  val task_valid = RegInit(False)

  val ChunkSize = RegInit(U(0, 32 bits))
  val AgeThreshold = RegInit(U(0, 32 bits))
  val HeapRegionBias = RegInit(U(0, 32 bits))
  val RegionAttrShiftBy = RegInit(U(0, 32 bits))
  val HeapRegionShiftBy = RegInit(U(0, 32 bits))
  val LogOfHRGrainBytes = RegInit(U(0, 32 bits))
  val StepperOffset = RegInit(U(0, GCElementWidth bits))
  val YoungWordsBase = RegInit(U(0, GCElementWidth bits))
  val RegionAttrBase = RegInit(U(0, GCElementWidth bits))
  val PlabAllocatorPtr = RegInit(U(0, GCElementWidth bits))
  val RegionAttrBiasedBase = RegInit(U(0, GCElementWidth bits))
  val HeapRegionBiasedBase = RegInit(U(0, GCElementWidth bits))
  val ParScanThreadStatePtr = RegInit(U(0, GCElementWidth bits))
  val TaskQueue_Bottom = RegInit(U(0, 32 bits))
  val TaskQueue_ElemsBase = RegInit(U(0, GCElementWidth bits))
  val HumongousReclaimCandidatesBoolBase = RegInit(U(0, GCElementWidth bits))
  val CardTablePtr = RegInit(U(0, GCElementWidth bits))
  val G1h = RegInit(U(0, GCElementWidth bits))
  val IntArrayKlassObj = RegInit(U(0, GCElementWidth bits))
  val ObjectKlass = RegInit(U(0, GCElementWidth bits))
  val LockPtr = RegInit(U(0, GCElementWidth bits))
  val Thread = RegInit(U(0, GCElementWidth bits))
  val DummyRegion = RegInit(U(0, GCElementWidth bits))
  val NumaPtr = RegInit(U(0, GCElementWidth bits))
  val CompressedOopBase = RegInit(U(0, GCElementWidth bits))
  val CompressedKlassPointerBase = RegInit(U(0, GCElementWidth bits))
  val CompressedFlag = RegInit(U(0, 32 bits))

  val DebugTimeStamp = RegInit(U(0,64 bits))
  DebugTimeStamp := DebugTimeStamp + U(1)

  io.ctrl2top.Ready := !task_valid
  io.ctrl2top.Done := task_valid && gcTaskStack.io.ConfigIO.Done
  // taskstack取完了 且 当前正在做的task也处理完成

  when(io.ctrl2top.Done){
    task_valid := False
  }

  when(io.ctrl2top.Valid && io.ctrl2top.Ready){
    task_valid := True
    ChunkSize                         := io.ctrl2top.ChunkSize
    AgeThreshold                      := io.ctrl2top.AgeThreshold
    HeapRegionBias                    := io.ctrl2top.HeapRegionBias
    RegionAttrShiftBy                 := io.ctrl2top.RegionAttrShiftBy
    HeapRegionShiftBy                 := io.ctrl2top.HeapRegionShiftBy
    LogOfHRGrainBytes                 := io.ctrl2top.LogOfHRGrainBytes
    StepperOffset                     := io.ctrl2top.StepperOffset
    YoungWordsBase                    := io.ctrl2top.YoungWordsBase
    RegionAttrBase                    := io.ctrl2top.RegionAttrBase
    PlabAllocatorPtr                  := io.ctrl2top.PlabAllocatorPtr
    RegionAttrBiasedBase              := io.ctrl2top.RegionAttrBiasedBase
    HeapRegionBiasedBase              := io.ctrl2top.HeapRegionBiasedBase
    ParScanThreadStatePtr             := io.ctrl2top.ParScanThreadStatePtr
    TaskQueue_Bottom                  := io.ctrl2top.TaskQueue_Bottom
    TaskQueue_ElemsBase               := io.ctrl2top.TaskQueue_ElemsBase
    HumongousReclaimCandidatesBoolBase:= io.ctrl2top.HumongousReclaimCandidatesBoolBase
    CardTablePtr                      := io.ctrl2top.CardTablePtr
    G1h                               := io.ctrl2top.G1h
    IntArrayKlassObj                  := io.ctrl2top.IntArrayKlassObj
    ObjectKlass                       := io.ctrl2top.ObjectKlass
    LockPtr                           := io.ctrl2top.LockPtr
    Thread                            := io.ctrl2top.Thread
    DummyRegion                       := io.ctrl2top.DummyRegion
    NumaPtr                           := io.ctrl2top.NumaPtr
    CompressedOopBase                 := io.ctrl2top.CompressedOopBase
    CompressedKlassPointerBase        := io.ctrl2top.CompressedKlassPointerBase
    CompressedFlag                    := io.ctrl2top.CompressedFlag
  }

  // GCTaskStack
  gcTaskStack.io.toFetch <> gcFetch.io.toFetch
  gcTaskStack.io.toStack <> gcTrace.io.ToStack
  gcTaskStack.io.Mreq <> gcUnalignedMMUAdapter(0).io.in
  gcTaskStack.io.ConfigIO.TaskQueue_Bottom := TaskQueue_Bottom
  gcTaskStack.io.ConfigIO.TaskQueue_ElemsBase := TaskQueue_ElemsBase
  gcTaskStack.io.ConfigIO.TaskValid := task_valid
  gcTaskStack.io.DebugTimeStamp := DebugTimeStamp

  // GCFetch
  gcFetch.io.Mreq <> gcUnalignedMMUAdapter(1).io.in
  gcFetch.io.Fetch2OopProcess <> gcOopProcess.io.Fetch2Process
  gcFetch.io.Fetch2ArrayProcess <> gcArrayProcess.io.Fetch2Process
  gcFetch.io.Trace2Fetch <> gcTrace.io.Trace2Fetch
  gcFetch.io.ConfigIO.UseCompressedOop := CompressedFlag(0)
  gcFetch.io.ConfigIO.CompressedOopBase := CompressedOopBase
  gcFetch.io.ConfigIO.CompressedOopShift := CompressedFlag(15 downto 8)
  gcFetch.io.ConfigIO.UseCompressedKlassPointers := CompressedFlag(1)
  gcFetch.io.DebugTimeStamp := DebugTimeStamp
  gcFetch.io.CopyDone := gcCopy.io.ToCopy.Done

  // GCOopProcess (ToAop)
  gcOopProcess.io.Mreq <> gcUnalignedMMUAdapter(2).io.in
  gcOopProcess.io.Process2CopySurvivor <> gcOopCopy2Survivor.io.ToCopySurvivor
  gcOopProcess.io.ConfigIO.RegionAttrShiftBy := RegionAttrShiftBy
  gcOopProcess.io.ConfigIO.RegionAttrBiasedBase := RegionAttrBiasedBase
  gcOopProcess.io.ConfigIO.LogOfHRGrainBytes := LogOfHRGrainBytes
  gcOopProcess.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcOopProcess.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcOopProcess.io.ConfigIO.UseCompressedOop := CompressedFlag(0)
  gcOopProcess.io.ConfigIO.CompressedOopBase := CompressedOopBase
  gcOopProcess.io.ConfigIO.CompressedOopShift := CompressedFlag(15 downto 8)
  gcOopProcess.io.DebugTimeStamp := DebugTimeStamp

  // GCArrayProcess (ToTrace)
  gcArrayProcess.io.Mreq <> gcUnalignedMMUAdapter(3).io.in
  gcArrayProcess.io.ConfigIO.ChunkSize := ChunkSize
  gcArrayProcess.io.ConfigIO.StepperOffset := StepperOffset
  gcArrayProcess.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcArrayProcess.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcArrayProcess.io.ConfigIO.UseCompressedKlassPointers := CompressedFlag(1)
  gcArrayProcess.io.DebugTimeStamp := DebugTimeStamp

  // GCOopCopy2Survivor
  gcOopCopy2Survivor.io.Mreq <> gcUnalignedMMUAdapter(4).io.in
  gcOopCopy2Survivor.io.ToCopy <> gcCopy.io.ToCopy
  gcOopCopy2Survivor.io.ToAllocate <> gcAllocate.io.ToAllocate
  gcOopCopy2Survivor.io.ConfigIO.ChunkSize := ChunkSize
  gcOopCopy2Survivor.io.ConfigIO.AgeThreshold := AgeThreshold
  gcOopCopy2Survivor.io.ConfigIO.YoungWordsBase := YoungWordsBase
  gcOopCopy2Survivor.io.ConfigIO.PlabAllocatorPtr := PlabAllocatorPtr
  gcOopCopy2Survivor.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcOopCopy2Survivor.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcOopCopy2Survivor.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcOopCopy2Survivor.io.ConfigIO.UseCompressedKlassPointer := CompressedFlag(1)
  gcOopCopy2Survivor.io.ConfigIO.CompressedKlassPointerBase := CompressedKlassPointerBase
  gcOopCopy2Survivor.io.ConfigIO.CompressedKlassPointerShift := CompressedFlag(23 downto 16)
  gcOopCopy2Survivor.io.DebugTimeStamp := DebugTimeStamp

  // gcAllocate(ToParAllocate)
  gcAllocate.io.Mreq <> gcUnalignedMMUAdapter(5).io.in
  gcAllocate.io.ToAttemptAllocate <> gcAttemptAlloc.io.ToAttemptAllocate
  gcAllocate.io.ConfigIO.Thread := Thread
  gcAllocate.io.ConfigIO.LockPtr := LockPtr
  gcAllocate.io.ConfigIO.G1h := G1h
  gcAllocate.io.ConfigIO.objectKlassObj := ObjectKlass
  gcAllocate.io.ConfigIO.intArrayKlassObj := IntArrayKlassObj
  gcAllocate.io.ConfigIO.PlabAllocatorPtr := PlabAllocatorPtr
  gcAllocate.io.ConfigIO.UseCompressedKlassPointers := CompressedFlag(1)
  gcAllocate.io.ConfigIO.CompressedKlassPointerBase := CompressedKlassPointerBase
  gcAllocate.io.ConfigIO.CompressedKlassPointerShift := CompressedFlag(23 downto 16)
  gcAllocate.io.DebugTimeStamp := DebugTimeStamp

  // gcParAllocate
  gcParAllocate.io.Mreq <> gcUnalignedMMUAdapter(6).io.in
  gcParAllocate.io.DebugTimeStamp := DebugTimeStamp

  // gcAttemptAlloc
  gcAttemptAlloc.io.Mreq <> gcUnalignedMMUAdapter(7).io.in
  gcAttemptAlloc.io.ToNewGCAlloc <> gcNewGCAlloc.io.ToNewGCAlloc
  gcAttemptAlloc.io.ConfigIO.G1h := G1h
  gcAttemptAlloc.io.ConfigIO.DummyRegion := DummyRegion
  gcAttemptAlloc.io.DebugTimeStamp := DebugTimeStamp

  // gcNewGCAloc
  gcNewGCAlloc.io.Mreq <> gcUnalignedMMUAdapter(8).io.in
  gcNewGCAlloc.io.ToAllocFreeRegion <> gcAllocFreeRegion.io.ToAllocFreeRegion
  gcNewGCAlloc.io.ConfigIO.G1h := G1h
  gcNewGCAlloc.io.ConfigIO.DummyRegion := DummyRegion
  gcNewGCAlloc.io.DebugTimeStamp := DebugTimeStamp

  // gcAllocFreeRegion
  gcAllocFreeRegion.io.Mreq <> gcUnalignedMMUAdapter(9).io.in
  gcAllocFreeRegion.io.ConfigIO.G1h := G1h
  gcAllocFreeRegion.io.ConfigIO.NumaPtr := NumaPtr
  gcAllocFreeRegion.io.DebugTimeStamp := DebugTimeStamp

  // gcCopy
  gcCopy.io.readMReq <> gcUnalignedMMUAdapter(10).io.in
  gcCopy.io.writeMReq <> gcUnalignedMMUAdapter(11).io.in

  // gcTrace
  gcTrace.io.Mreq <> gcUnalignedMMUAdapter(12).io.in
  gcTrace.io.ConfigIO.ChunkSize                        := ChunkSize
  gcTrace.io.ConfigIO.RegionAttrBase                   := RegionAttrBase
  gcTrace.io.ConfigIO.RegionAttrShiftBy                := RegionAttrShiftBy
  gcTrace.io.ConfigIO.RegionAttrBiasedBase             := RegionAttrBiasedBase
  gcTrace.io.ConfigIO.HeapRegionBias                   := HeapRegionBias
  gcTrace.io.ConfigIO.HeapRegionShiftBy                := HeapRegionShiftBy
  gcTrace.io.ConfigIO.LogOfHRGrainBytes                := LogOfHRGrainBytes
  gcTrace.io.ConfigIO.UseCompressedOops                := CompressedFlag(0)
  gcTrace.io.ConfigIO.CompressedOopBase                := CompressedOopBase
  gcTrace.io.ConfigIO.CompressedOopShift               := CompressedFlag(15 downto 8)
  gcTrace.io.ConfigIO.UseCompressedKlassPointers       := CompressedFlag(1)
  gcTrace.io.ConfigIO.HumongousReclaimCandidatesBoolBase := HumongousReclaimCandidatesBoolBase
  gcTrace.io.DebugTimeStamp := DebugTimeStamp

  // gcAop
  gcAop.io.Mreq <> gcUnalignedMMUAdapter(13).io.in
  gcAop.io.ConfigIO.CardTablePtr := CardTablePtr
  gcAop.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcAop.io.DebugTimeStamp := DebugTimeStamp

  // gcLocalMMU
  gcLocalMMU.io.LastLevelCacheTLIO <> io.mmu2llc

  // gcTrace source mux
  val trace_mux = new GCTraceMux
  trace_mux.io.In0 <> gcArrayProcess.io.Process2Trace
  trace_mux.io.In1 <> gcOopCopy2Survivor.io.ToTrace
  trace_mux.io.Out <> gcTrace.io.ToTrace

  // gcAop source mux
  val aop_mux = new GCAopMux
  aop_mux.io.In0 <> gcOopProcess.io.Process2Aop
  aop_mux.io.In1 <> gcTrace.io.ToAop
  aop_mux.io.Out <> gcAop.io.Aop

  // gcParAllocate source mux
  val parAllocate_mux = new GCParAllocateMux
  parAllocate_mux.io.In0 <> gcAllocate.io.ToParAllocate
  parAllocate_mux.io.In1 <> gcAttemptAlloc.io.ToParAllocate
  parAllocate_mux.io.Out <> gcParAllocate.io.ToParAllocate
}

object GCTopVerilog extends App{
  Config.spinal.generateVerilog(new GCTop())
}