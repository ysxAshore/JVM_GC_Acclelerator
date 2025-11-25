package hwgc

import spinal.core._
import spinal.lib.StreamArbiter

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
  gcOop2CopySurvivor.io.ConfigIO.RegionAttrBiasedBase := RegionAttrBiasedBase
  gcOop2CopySurvivor.io.ConfigIO.RegionAttrShiftBy := RegionAttrShiftBy
  gcOop2CopySurvivor.io.ConfigIO.HeapRegionBiasedBase := HeapRegionBiasedBase
  gcOop2CopySurvivor.io.ConfigIO.HeapRegionShiftBy := HeapRegionShiftBy

  // GCTrace
  val useArray = gcArrayProcess.io.Process2Trace.Valid
  gcTrace.io.ToTrace.Valid := Mux(useArray, gcArrayProcess.io.Process2Trace.Valid, gcOop2CopySurvivor.io.Process2Trace.Valid)
  gcTrace.io.ToTrace.OopType := Mux(useArray, gcArrayProcess.io.Process2Trace.OopType, gcOop2CopySurvivor.io.Process2Trace.OopType)
  gcTrace.io.ToTrace.KlassPtr := Mux(useArray, gcArrayProcess.io.Process2Trace.KlassPtr, gcOop2CopySurvivor.io.Process2Trace.KlassPtr)
  gcTrace.io.ToTrace.SrcOopPtr := Mux(useArray, gcArrayProcess.io.Process2Trace.SrcOopPtr, gcOop2CopySurvivor.io.Process2Trace.SrcOopPtr)
  gcTrace.io.ToTrace.DestOopPtr := Mux(useArray, gcArrayProcess.io.Process2Trace.DestOopPtr, gcOop2CopySurvivor.io.Process2Trace.DestOopPtr)
  gcTrace.io.ToTrace.Kid := Mux(useArray, gcArrayProcess.io.Process2Trace.Kid, gcOop2CopySurvivor.io.Process2Trace.Kid)
  gcTrace.io.ToTrace.ArrayLength := Mux(useArray, gcArrayProcess.io.Process2Trace.ArrayLength, gcOop2CopySurvivor.io.Process2Trace.ArrayLength)
  gcTrace.io.ToTrace.PartialArrayStart := Mux(useArray, gcArrayProcess.io.Process2Trace.PartialArrayStart, gcOop2CopySurvivor.io.Process2Trace.PartialArrayStart)
  gcTrace.io.ToTrace.StepIndex := Mux(useArray, gcArrayProcess.io.Process2Trace.StepIndex, gcOop2CopySurvivor.io.Process2Trace.StepIndex)
  gcTrace.io.ToTrace.StepNCreate := Mux(useArray, gcArrayProcess.io.Process2Trace.StepNCreate, gcOop2CopySurvivor.io.Process2Trace.StepNCreate)

  gcArrayProcess.io.Process2Trace.Ready := gcTrace.io.ToTrace.Ready
  gcArrayProcess.io.Process2Trace.Done := gcTrace.io.ToTrace.Done
  gcOop2CopySurvivor.io.Process2Trace.Ready := gcTrace.io.ToTrace.Ready
  gcOop2CopySurvivor.io.Process2Trace.Done := gcTrace.io.ToTrace.Done

  val useTrace = gcTrace.io.Trace2Aop.Valid
  gcAop.io.Aop.Valid := Mux(useTrace, gcTrace.io.Trace2Aop.Valid, gcOopProcess.io.Process2Aop.Valid)
  gcAop.io.Aop.ParScanThreadStatePtr := Mux(useTrace,gcTrace.io.Trace2Aop.ParScanThreadStatePtr, gcOopProcess.io.Process2Aop.ParScanThreadStatePtr)
  gcAop.io.Aop.DestOopPtr := Mux(useTrace,gcTrace.io.Trace2Aop.DestOopPtr, gcOopProcess.io.Process2Aop.DestOopPtr)

  gcOopProcess.io.Process2Aop.Ready := gcAop.io.Aop.Ready
  gcOopProcess.io.Process2Aop.Done := gcAop.io.Aop.Done
  gcTrace.io.Trace2Aop.Ready := gcAop.io.Aop.Ready
  gcTrace.io.Trace2Aop.Done := gcAop.io.Aop.Done

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
  gcLocalMMU.io.localMMUIOs(4) <> gcOop2CopySurvivor.io.Mreq
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