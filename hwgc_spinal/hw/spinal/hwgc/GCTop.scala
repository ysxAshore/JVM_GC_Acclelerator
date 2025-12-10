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
  val RegionAttrBase = RegInit(U(0, MMUAddrWidth bits))
  val RegionAttrBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val RegionAttrShiftBy = RegInit(U(0, 32 bits))
  val HeapRegionBias = RegInit(U(0, 32 bits))
  val HeapRegionShiftBy = RegInit(U(0, 32 bits))
  val HeapRegionBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val HumongousReclaimCandidatesBoolBase = RegInit(U(0, MMUAddrWidth bits))
  val ParScanThreadStatePtr = RegInit(U(0, MMUAddrWidth bits))
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
    RegionAttrBase := io.ctrl2top.RegionAttrBase
    RegionAttrBiasedBase := io.ctrl2top.RegionAttrBiasedBase
    RegionAttrShiftBy := io.ctrl2top.RegionAttrShiftBy
    HeapRegionBias := io.ctrl2top.HeapRegionBias
    HeapRegionShiftBy := io.ctrl2top.HeapRegionShiftBy
    HeapRegionBiasedBase := io.ctrl2top.HeapRegionBiasedBase
    HumongousReclaimCandidatesBoolBase := io.ctrl2top.HumongousReclaimCandidatesBoolBase
    ParScanThreadStatePtr := io.ctrl2top.ParScanThreadStatePtr
    queue_elems_base := io.ctrl2top.TaskQueue_ElemsBase
    queue_bottom_addr := io.ctrl2top.TaskQueue_BottomAddr
    queue_ageTop_addr := io.ctrl2top.TaskQueue_AgeTopAddr
  }

  // GCTaskStack
  gcTaskStack.io.Pop <> gcFetch.io.Stack2Fetch
  gcTaskStack.io.Push <> gcTrace.io.Trace2Stack
  gcTaskStack.io.DebugTimeStamp := DebugTimeStamp
  gcTaskStack.io.ConfigIO.TaskValid := task_valid
  gcTaskStack.io.ConfigIO.TaskQueue_AgeTopAddr := queue_ageTop_addr
  gcTaskStack.io.ConfigIO.TaskQueue_BottomAddr := queue_bottom_addr
  gcTaskStack.io.ConfigIO.TaskQueue_ElemsBase := queue_elems_base
  when(gcTaskStack.io.ConfigIO.Done){
    task_valid := False
  }

  // GCFetch
  gcFetch.io.Fetch2ArrayProcess <> gcArrayProcess.io.Fetch2Process
  gcFetch.io.Fetch2OopProcess <> gcOopProcess.io.Fetch2Process

  // GCArrayProcess
  gcArrayProcess.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr

  // GCOopProcess
  gcOopProcess.io.Process2CopySurvivor <> gcOop2CopySurvivor.io.Process2CopySurvivor
  gcOopProcess.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcOopProcess.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcOopProcess.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy

  // GCOopCopy2Survivor
  gcOop2CopySurvivor.io.Process2Copy <> gcCopy.io.fromProcess
  gcOop2CopySurvivor.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcOop2CopySurvivor.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcOop2CopySurvivor.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy

  // GCTrace
  val arrayStream = process2stream(gcArrayProcess.io.Process2Trace)
  val oopStream = process2stream(gcOop2CopySurvivor.io.Process2Trace)

 // val arb = StreamArbiterFactory.lowerFirst.onArgs(arrayStream, oopStream)
  val arb_trace = StreamArbiterFactory.lowerFirst.build(GCTracePayload(), 2)

  arb_trace.io.inputs(0) << arrayStream
  arb_trace.io.inputs(1) << oopStream

  gcTrace.io.ToTrace.Valid := arb_trace.io.output.valid
  gcTrace.io.ToTrace.OopType := arb_trace.io.output.payload.OopType
  gcTrace.io.ToTrace.KlassPtr := arb_trace.io.output.payload.KlassPtr
  gcTrace.io.ToTrace.SrcOopPtr := arb_trace.io.output.payload.SrcOopPtr
  gcTrace.io.ToTrace.DestOopPtr := arb_trace.io.output.payload.DestOopPtr
  gcTrace.io.ToTrace.Kid := arb_trace.io.output.payload.Kid
  gcTrace.io.ToTrace.ArrayLength := arb_trace.io.output.payload.ArrayLength
  gcTrace.io.ToTrace.PartialArrayStart := arb_trace.io.output.payload.PartialArrayStart
  gcTrace.io.ToTrace.StepIndex := arb_trace.io.output.payload.StepIndex
  gcTrace.io.ToTrace.StepNCreate := arb_trace.io.output.payload.StepNCreate

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

  val traceAopStream = aop2stream(gcTrace.io.Trace2Aop)
  val oopAopStream = aop2stream(gcOopProcess.io.Process2Aop)

  val arb_aop = StreamArbiterFactory.lowerFirst.build(GCAopPayload(), 2)
  arb_aop.io.inputs(0) << traceAopStream
  arb_aop.io.inputs(1) << oopAopStream

  gcAop.io.Aop.Valid := arb_aop.io.output.valid
  gcAop.io.Aop.ParScanThreadStatePtr := arb_aop.io.output.payload.ParScanThreadStatePtr
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

  gcTrace.io.ConfigIO.ParScanThreadStatePtr := ParScanThreadStatePtr
  gcTrace.io.ConfigIO.RegionAttrBase := RegionAttrBase
  gcTrace.io.ConfigIO.RegionAttrShiftBy := RegionAttrShiftBy
  gcTrace.io.ConfigIO.RegionAttrBiasedBase := RegionAttrBiasedBase
  gcTrace.io.ConfigIO.HeapRegionBias := HeapRegionBias
  gcTrace.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy
  gcTrace.io.ConfigIO.HumongousReclaimCandidatesBoolBase := HumongousReclaimCandidatesBoolBase

  // MMU
  gcLocalMMU.io.LastLevelCacheTLIO <> io.mmu2llc
  gcLocalMMU.io.localMMUIOs(0) <> gcTaskStack.io.LocalMMUIO
  gcLocalMMU.io.localMMUIOs(1) <> gcFetch.io.FetchMReq
  gcLocalMMU.io.localMMUIOs(2) <> gcArrayProcess.io.Mreq
  gcLocalMMU.io.localMMUIOs(3) <> gcOopProcess.io.Mreq
  gcLocalMMU.io.localMMUIOs(4) <> gcOop2CopySurvivor.io.CommonMreq
  gcLocalMMU.io.localMMUIOs(5) <> gcCopy.io.readMReq
  gcLocalMMU.io.localMMUIOs(6) <> gcCopy.io.writeMReq
  for(i <- 7 until 7 + GCoopWorkStages){
    gcLocalMMU.io.localMMUIOs(i) <> gcTrace.io.TraceMMUIO.oopWorkMReqs(i - 7)
  }
  gcLocalMMU.io.localMMUIOs(15) <> gcTrace.io.TraceMMUIO.staticMReq
  for(i <- 16 until 20){
    gcLocalMMU.io.localMMUIOs(i) <> gcTrace.io.TraceMMUIO.oopTraceMReqs(i - 16)
  }
  for(i <- 20 until 20 + GCaopWorkStages){
    gcLocalMMU.io.localMMUIOs(i) <> gcAop.io.aopMReqs(i - 20)
  }
}

object GCTopVerilog extends App{
  Config.spinal.generateVerilog(new GCTop())
}