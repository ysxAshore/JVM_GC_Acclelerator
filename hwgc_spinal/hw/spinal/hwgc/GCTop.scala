package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCTop extends Module with GCParameters with HWParameters {
  val io = new GCTopIO

  val gcTaskStack = new GCTaskStack
  val gcFetch = new GCFetch
  val gcArrayProcess = new GCArrayProcess
  val gcOopProcess = new GCOopProcess
  val gcOop2CopySurvivor = new GCOopCopy2Survivor
  val gcTrace = new GCTrace
  val gcCopy = new GCCopy
  val gcAop = new GCAop
  val gcLocalMMU = new GCLocalMMU

  val task_valid = RegInit(False)
  val ChunkSize = RegInit(U(0, 32 bits))
  val CardTablePtr = RegInit(U(0, MMUAddrWidth bits))
  val AgeThreshold = RegInit(U(0, 32 bits))
  val StepperOffset = RegInit(U(0, MMUAddrWidth bits))
  val YoungWordsBase = RegInit(U(0, MMUAddrWidth bits))
  val RegionAttrBase = RegInit(U(0, MMUAddrWidth bits))
  val HeapRegionBias = RegInit(U(0, 32 bits))
  val PlabAllocatorPtr = RegInit(U(0, MMUAddrWidth bits))
  val RegionAttrShiftBy = RegInit(U(0, 32 bits))
  val HeapRegionShiftBy = RegInit(U(0, 32 bits))
  val LogOfHRGrainBytes = RegInit(U(0, 32 bits))
  val RegionAttrBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val HeapRegionBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val ParScanThreadStatePtr = RegInit(U(0, MMUAddrWidth bits))
  val HumongousReclaimCandidatesBoolBase = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom_addr = RegInit(U(0, MMUAddrWidth bits))
  val queue_ageTop_addr = RegInit(U(0, MMUAddrWidth bits))
  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))

  val DebugTimeStamp = RegInit(U(0,64 bits))
  DebugTimeStamp := DebugTimeStamp + U(1)

  // receive config
  io.ctrl2top.Ready := !task_valid
  io.ctrl2top.Done := gcTaskStack.io.ConfigIO.Done
  when(io.ctrl2top.Valid && io.ctrl2top.Ready){
    task_valid := True
    ChunkSize := io.ctrl2top.ChunkSize
    CardTablePtr := io.ctrl2top.CardTablePtr
    AgeThreshold := io.ctrl2top.AgeThreshold
    StepperOffset := io.ctrl2top.StepperOffset
    YoungWordsBase := io.ctrl2top.YoungWordsBase
    RegionAttrBase := io.ctrl2top.RegionAttrBase
    HeapRegionBias := io.ctrl2top.HeapRegionBias
    PlabAllocatorPtr := io.ctrl2top.PlabAllocatorPtr
    RegionAttrShiftBy := io.ctrl2top.RegionAttrShiftBy
    HeapRegionShiftBy := io.ctrl2top.HeapRegionShiftBy
    LogOfHRGrainBytes := io.ctrl2top.LogOfHRGrainBytes
    RegionAttrBiasedBase := io.ctrl2top.RegionAttrBiasedBase
    HeapRegionBiasedBase := io.ctrl2top.HeapRegionBiasedBase
    ParScanThreadStatePtr := io.ctrl2top.ParScanThreadStatePtr
    HumongousReclaimCandidatesBoolBase := io.ctrl2top.HumongousReclaimCandidatesBoolBase
    queue_elems_base := io.ctrl2top.TaskQueue_ElemsBase
    queue_bottom_addr := io.ctrl2top.TaskQueue_BottomAddr
    queue_ageTop_addr := io.ctrl2top.TaskQueue_AgeTopAddr
  }

  // GCTaskStack
  val issueTask = RegInit(False)
  gcTaskStack.io.Pop <> gcFetch.io.Stack2Fetch
  gcTaskStack.io.Push <> gcTrace.io.Trace2Stack
  gcTaskStack.io.DebugTimeStamp := DebugTimeStamp
  gcTaskStack.io.ConfigIO.TaskValid := task_valid && !issueTask
  gcTaskStack.io.ConfigIO.TaskQueue_AgeTopAddr := queue_ageTop_addr
  gcTaskStack.io.ConfigIO.TaskQueue_BottomAddr := queue_bottom_addr
  gcTaskStack.io.ConfigIO.TaskQueue_ElemsBase := queue_elems_base
  when(gcTaskStack.io.ConfigIO.TaskValid && gcTaskStack.io.ConfigIO.TaskReady){
    issueTask := True
  }
  when(gcTaskStack.io.ConfigIO.Done){
    issueTask := False
    task_valid := False
  }

  // GCFetch
  gcFetch.io.DebugTimeStamp := DebugTimeStamp
  gcFetch.io.Fetch2OopProcess <> gcOopProcess.io.Fetch2Process
  gcFetch.io.Fetch2ArrayProcess <> gcArrayProcess.io.Fetch2Process

  // GCArrayProcess
  gcArrayProcess.io.DebugTimeStamp := DebugTimeStamp
  gcArrayProcess.io.ConfigIO.ChunkSize := ChunkSize
  gcArrayProcess.io.ConfigIO.StepperOffset := StepperOffset
  gcArrayProcess.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcArrayProcess.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase

  // GCOopProcess
  gcOopProcess.io.DebugTimeStamp := DebugTimeStamp
  gcOopProcess.io.Process2CopySurvivor <> gcOop2CopySurvivor.io.Process2CopySurvivor
  gcOopProcess.io.ConfigIO.CardTablePtr := CardTablePtr
  gcOopProcess.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcOopProcess.io.ConfigIO.RegionAttrShiftBy := RegionAttrShiftBy
  gcOopProcess.io.ConfigIO.RegionAttrBiasedBase := RegionAttrBiasedBase
  gcOopProcess.io.ConfigIO.LogOfHRGrainBytes := LogOfHRGrainBytes
  gcOopProcess.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcOopProcess.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase

  // GCOopCopy2Survivor
  gcOop2CopySurvivor.io.DebugTimeStamp := DebugTimeStamp
  gcOop2CopySurvivor.io.Process2Copy <> gcCopy.io.fromProcess
  gcOop2CopySurvivor.io.ConfigIO.ChunkSize := ChunkSize
  gcOop2CopySurvivor.io.ConfigIO.AgeThreshold := AgeThreshold
  gcOop2CopySurvivor.io.ConfigIO.YoungWordsBase := YoungWordsBase
  gcOop2CopySurvivor.io.ConfigIO.PlabAllocatorPtr := PlabAllocatorPtr
  gcOop2CopySurvivor.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcOop2CopySurvivor.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcOop2CopySurvivor.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr

  // GCTrace
  val arrayStream = process2stream(gcArrayProcess.io.Process2Trace)
  val oopStream = process2stream(gcOop2CopySurvivor.io.Process2Trace)

  // val arb = StreamArbiterFactory.lowerFirst.onArgs(arrayStream, oopStream)
  val arb_trace = StreamArbiterFactory.lowerFirst.build(GCTracePayload(), 2)

  arb_trace.io.inputs(0) << arrayStream
  arb_trace.io.inputs(1) << oopStream

  gcTrace.io.ToTrace.Valid := arb_trace.io.output.valid
  gcTrace.io.ToTrace.Kid := arb_trace.io.output.payload.Kid
  gcTrace.io.ToTrace.OopType := arb_trace.io.output.payload.OopType
  gcTrace.io.ToTrace.KlassPtr := arb_trace.io.output.payload.KlassPtr
  gcTrace.io.ToTrace.SrcOopPtr := arb_trace.io.output.payload.SrcOopPtr
  gcTrace.io.ToTrace.DestOopPtr := arb_trace.io.output.payload.DestOopPtr
  gcTrace.io.ToTrace.ScanningInYoung := arb_trace.io.output.payload.ScanningInYoung
  gcTrace.io.ToTrace.StepIndex := arb_trace.io.output.payload.StepIndex
  gcTrace.io.ToTrace.StepNCreate := arb_trace.io.output.payload.StepNCreate
  gcTrace.io.ToTrace.ArrayLength := arb_trace.io.output.payload.ArrayLength
  gcTrace.io.ToTrace.PartialArrayStart := arb_trace.io.output.payload.PartialArrayStart

  arb_trace.io.output.ready := gcTrace.io.ToTrace.Ready

  gcArrayProcess.io.Process2Trace.Done := False
  gcOop2CopySurvivor.io.Process2Trace.Done := False
  val currentId_trace = RegInit(U(0, 1 bits))
  val working_trace = RegInit(False)

  when(arb_trace.io.output.fire){
    working_trace := True
    currentId_trace := arb_trace.io.chosen
  }

  when(working_trace && gcTrace.io.ToTrace.Done){
    switch(currentId_trace){
      is(0) {gcArrayProcess.io.Process2Trace.Done := True}
      is(1) {gcOop2CopySurvivor.io.Process2Trace.Done := True}
    }
    working_trace := False
  }

  gcTrace.io.DebugTimeStampe := DebugTimeStamp
  gcTrace.io.ConfigIO.CardTablePtr := CardTablePtr
  gcTrace.io.ConfigIO.RegionAttrBase := RegionAttrBase
  gcTrace.io.ConfigIO.RegionAttrShiftBy := RegionAttrShiftBy
  gcTrace.io.ConfigIO.RegionAttrBiasedBase := RegionAttrBiasedBase
  gcTrace.io.ConfigIO.HeapRegionBias := HeapRegionBias
  gcTrace.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcTrace.io.ConfigIO.LogOfHRGrainBytes := LogOfHRGrainBytes
  gcTrace.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcTrace.io.ConfigIO.HumongousReclaimCandidatesBoolBase := HumongousReclaimCandidatesBoolBase

  //Aop
  val traceAopStream = aop2stream(gcTrace.io.Trace2Aop)
  val oopAopStream = aop2stream(gcOopProcess.io.Process2Aop)

  val arb_aop = StreamArbiterFactory.lowerFirst.build(GCAopPayload(), 2)
  arb_aop.io.inputs(0) << traceAopStream
  arb_aop.io.inputs(1) << oopAopStream

  gcAop.io.DebugTimeStamp := DebugTimeStamp
  gcAop.io.Aop.Valid := arb_aop.io.output.valid
  gcAop.io.Aop.ParScanThreadStatePtr := arb_aop.io.output.payload.ParScanThreadStatePtr
  gcAop.io.Aop.CardTablePtr := arb_aop.io.output.payload.CardTablePtr
  gcAop.io.Aop.RegionAttr := arb_aop.io.output.payload.RegionAttr
  gcAop.io.Aop.Task := arb_aop.io.output.payload.Task

  arb_aop.io.output.ready := gcAop.io.Aop.Ready

  gcOopProcess.io.Process2Aop.Done := False
  gcTrace.io.Trace2Aop.Done := False

  val currentId_aop = RegInit(U(0, 1 bits))
  val working_aop = RegInit(False)

  when(arb_aop.io.output.fire){
    working_aop := True
    currentId_aop := arb_aop.io.chosen
  }

  when(working_aop && gcAop.io.Aop.Done){
    switch(currentId_aop){
      is(0) {gcTrace.io.Trace2Aop.Done := True}
      is(1) {gcOopProcess.io.Process2Aop.Done := True}
    }
    working_aop := False
  }

  // MMU
  gcLocalMMU.io.LastLevelCacheTLIO <> io.mmu2llc
  gcLocalMMU.io.localMMUIOs(0) <> gcTaskStack.io.LocalMMUIO
  gcLocalMMU.io.localMMUIOs(1) <> gcFetch.io.FetchMReq
  gcLocalMMU.io.localMMUIOs(2) <> gcArrayProcess.io.Mreq
  gcLocalMMU.io.localMMUIOs(3) <> gcOopProcess.io.Mreq
  gcLocalMMU.io.localMMUIOs(4) <> gcOop2CopySurvivor.io.CommonMreq
  for(i <- 5 until 5 + 4){
    gcLocalMMU.io.localMMUIOs(i) <> gcOop2CopySurvivor.io.SpecialMreq(i - 5)
  }
  gcLocalMMU.io.localMMUIOs(9) <> gcCopy.io.readMReq
  gcLocalMMU.io.localMMUIOs(10) <> gcCopy.io.writeMReq
  for(i <- 11 until 11 + GCoopWorkStages){
    gcLocalMMU.io.localMMUIOs(i) <> gcTrace.io.TraceMMUIO.oopWorkMReqs(i - 11)
  }
  gcLocalMMU.io.localMMUIOs(18) <> gcTrace.io.TraceMMUIO.commonMReq
  for(i <- 19 until 19 + GCaopWorkStages){
    gcLocalMMU.io.localMMUIOs(i) <> gcAop.io.aopMReqs(i - 19)
  }
}

object GCTopVerilog extends App{
  Config.spinal.generateVerilog(new GCTop())
}