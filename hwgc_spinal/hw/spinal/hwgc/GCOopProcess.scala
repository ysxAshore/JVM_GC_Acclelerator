package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCOopProcess extends Module with HWParameters with GCParameters{
  val io = new Bundle{
    val Fetch2Process = slave(new GCFetch2ProcessUnit)
    val Process2CopySurvivor = master(new GCProcess2Survivor)
    val Process2Aop = master(new AopParameters)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCOopProcessConfigIO)
  }

  // default value
  io.Fetch2Process.Ready := False
  io.Fetch2Process.Done := False
  io.Fetch2Process.DestOopPtr := U(0)

  io.Process2CopySurvivor.Valid := False
  io.Process2CopySurvivor.SrcOopPtr := U(0)
  io.Process2CopySurvivor.MarkWord := U(0)
  io.Process2CopySurvivor.RegionAttrPtr := U(0)

  io.Process2Aop.Valid := False
  io.Process2Aop.Task := U(0)
  io.Process2Aop.RegionAttr := U(0)
  io.Process2Aop.ParScanThreadStatePtr := U(0)

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  object overall_state extends SpinalEnum {
    val s_idle, s_sendCopy2Survivor, s_waitdone1, s_writeTask, s_readHR, s_readHRType, s_sendAop, s_waitDone2 = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val pss = RegInit(U(0, MMUAddrWidth bits))
  val regionAttrBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val regionAttrShiftBy = RegInit(U(0, 32 bits))
  val heapRegionBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val heapRegionShiftBy = RegInit(U(0, 32 bits))
  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val task = RegInit(U(0, MMUAddrWidth bits))
  val srcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val destOopPtr = RegInit(U(0,MMUAddrWidth bits))
  val markWord = RegInit(U(0, MMUDataWidth bits))
  val logOfHRGrainBytes = RegInit(U(0, 32 bits))

  def resetState(): Unit = {
    state := overall_state.s_idle
    io.Fetch2Process.Done := True
  }

  when(state === overall_state.s_idle){
    io.Fetch2Process.Ready := True
    when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
      pss := io.ConfigIO.ParScanThreadStatePtr
      regionAttrBiasedBase := io.ConfigIO.RegionAttrBiasedBase
      regionAttrShiftBy := io.ConfigIO.RegionAttrShiftBy
      heapRegionBiasedBase := io.ConfigIO.HeapRegionBiasedBase
      heapRegionShiftBy := io.ConfigIO.HeapRegionShiftBy
      logOfHRGrainBytes := io.ConfigIO.LogOfHRGrainBytes

      oopType := io.Fetch2Process.OopType
      task := io.Fetch2Process.Task
      srcOopPtr := io.Fetch2Process.SrcOopPtr
      markWord := io.Fetch2Process.MarkWord

      when((io.Fetch2Process.MarkWord & U(LOCK_MASK_IN_PLACE, MMUDataWidth bits)) === U(MARKED_VALUE, MMUDataWidth bits)){
        destOopPtr := io.Fetch2Process.MarkWord & ~U(LOCK_MASK_IN_PLACE, MMUDataWidth bits)
        state := overall_state.s_writeTask
      }.otherwise{
        state := overall_state.s_sendCopy2Survivor
      }
    }
  }

  when(state === overall_state.s_sendCopy2Survivor){
    io.Process2CopySurvivor.Valid := True

    io.Process2CopySurvivor.SrcOopPtr := srcOopPtr
    io.Process2CopySurvivor.MarkWord := markWord
    io.Process2CopySurvivor.RegionAttrPtr := (regionAttrBiasedBase + (srcOopPtr >> regionAttrShiftBy) * GCHeapRegionAttr_Size).resized

    when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
      state := overall_state.s_waitdone1
    }
  }

  when(state === overall_state.s_waitdone1){
    when(io.Process2CopySurvivor.Done){
      destOopPtr := io.Process2CopySurvivor.DestOopPtr
      state := overall_state.s_writeTask
    }
  }

  val reqIssued = RegInit(False)
  when(state === overall_state.s_writeTask){
    issueReq(io.Mreq, task, True, allBytesOnes, destOopPtr, reqIssued) { rd =>
      when((task ^ destOopPtr) >> logOfHRGrainBytes === U(0)){
        resetState()
      }.otherwise{
        state := overall_state.s_readHR
      }
    }
  }

  val from_region = RegInit(U(0, MMUAddrWidth bits))
  when(state === overall_state.s_readHR){
    issueReq(io.Mreq, (heapRegionBiasedBase + (task >> heapRegionShiftBy) * GCObjectPtr_Size).resized, False, U(0), U(0), reqIssued){ rd =>
      from_region := rd
      state := overall_state.s_readHRType
    }
  }

  when(state === overall_state.s_readHRType){
    issueReq(io.Mreq, from_region + HEAP_REGION_TYPE_OFFSET, False, U(0), U(0), reqIssued){ rd =>
      when((rd(31 downto 0) & U(YOUNG_MASK, 32 bits)) === U(0)){
        state := overall_state.s_sendAop
      }.otherwise{
        resetState()
      }
    }
  }

  val reqDone = RegInit(False)
  val regionAttr = RegInit(U(0, 16 bits))
  when(state === overall_state.s_sendAop){
    when(!reqDone){
      val regionAttrPtr = (regionAttrBiasedBase + (destOopPtr >> regionAttrShiftBy) * GCHeapRegionAttr_Size).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, regionAttrPtr, False, U(0), U(0), reqIssued){ rd =>
        reqDone := True
        regionAttr := rd(15 downto 0)
      }
    }

    io.Process2Aop.Valid := True
    io.Process2Aop.Task := task
    io.Process2Aop.ParScanThreadStatePtr := pss
    io.Process2Aop.RegionAttr := regionAttr

    when(io.Process2Aop.Valid && io.Process2Aop.Ready){
      state := overall_state.s_waitDone2
      reqDone := False
    }
  }

  when(state === overall_state.s_waitDone2){
    when(io.Process2Aop.Done){
      resetState()
    }
  }
}

object GCOopProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCOopProcess())
}
