package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCOopProcess extends Module with HWParameters with GCParameters{
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val Process2Aop = master(new ToAopParameters)
    val Fetch2Process = slave(new GCFetch2ProcessUnit)
    val Process2CopySurvivor = master(new GCProcess2Survivor)
    val ConfigIO = slave(new GCOopProcessConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  io.Fetch2Process.Ready := False
  io.Fetch2Process.Done := False

  io.Process2CopySurvivor.Valid := False
  io.Process2CopySurvivor.SrcOopPtr := U(0)
  io.Process2CopySurvivor.MarkWord := U(0)
  io.Process2CopySurvivor.RegionAttrPtr := U(0)

  io.Process2Aop.Valid := False
  io.Process2Aop.Task := U(0)
  io.Process2Aop.RegionAttr := U(0)

  object overall_state extends SpinalEnum {
    val s_idle, s_sendCopy2Survivor, s_waitDone1, s_writeTask, s_readHR, s_readHRType, s_sendAop, s_waitDone2 = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val task = RegInit(U(0, GCElementWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord = RegInit(U(0, GCElementWidth bits))

  def resetState(): Unit = {
    state := overall_state.s_idle
    io.Fetch2Process.Done := True
  }

  when(state === overall_state.s_idle){
    io.Fetch2Process.Ready := True
    when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
      task := io.Fetch2Process.Task
      srcOopPtr := io.Fetch2Process.SrcOopPtr
      markWord := io.Fetch2Process.MarkWord

      val doCopy2Survivor = (io.Fetch2Process.MarkWord & U(x"3", GCElementWidth bits)) =/= U(x"3", GCElementWidth bits)
      when(!doCopy2Survivor){
        destOopPtr := io.Fetch2Process.MarkWord & ~U(x"3", GCElementWidth bits)
        state := overall_state.s_writeTask
      }.otherwise{
        state := overall_state.s_sendCopy2Survivor
      }

      if(DebugEnable){
        report(Seq(
          "[GCOopProcess<", io.DebugTimeStamp,
          ">]Receive task from Fetch Module",
          ", the srcOopPtr is ", io.Fetch2Process.SrcOopPtr,
          ", the markWord is ", io.Fetch2Process.MarkWord,
          ", the destOopPtr is ", Mux(doCopy2Survivor, U(0), io.Fetch2Process.MarkWord & ~U(x"3", GCElementWidth bits)),
          ", the next state is ", Mux(doCopy2Survivor, overall_state.s_sendCopy2Survivor, overall_state.s_writeTask),
          "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_sendCopy2Survivor){
    io.Process2CopySurvivor.Valid := True

    io.Process2CopySurvivor.SrcOopPtr := srcOopPtr
    io.Process2CopySurvivor.MarkWord := markWord
    io.Process2CopySurvivor.RegionAttrPtr := (io.ConfigIO.RegionAttrBiasedBase + (srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) * GCHeapRegionAttr_Size).resize(GCElementWidth bits)

    when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
      state := overall_state.s_waitDone1

      if(DebugEnable){
        report(Seq(
          "[GCOopProcess<", io.DebugTimeStamp,
          ">]Send the task to Copy2Survivor",
          "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_waitDone1){
    when(io.Process2CopySurvivor.Done){
      destOopPtr := io.Process2CopySurvivor.DestOopPtr
      state := overall_state.s_writeTask

      if(DebugEnable){
        report(Seq(
          "[GCOopProcess<", io.DebugTimeStamp,
          ">]The Copy2Survivor Module has done",
          ", and the destOopPtr is ", io.Process2CopySurvivor.DestOopPtr,
          "\n"
        ))
      }
    }
  }

  val reqIssued = RegInit(False)
  when(state === overall_state.s_writeTask){
    val writeObj = Mux(io.ConfigIO.UseCompressedOop, ((destOopPtr - io.ConfigIO.CompressedOopBase) >> io.ConfigIO.CompressedOopShift).resize(GCElementWidth bits), destOopPtr)
    val writeMask = Mux(io.ConfigIO.UseCompressedOop, getWstrb(4), getWstrb(8))
    issueReq(io.Mreq, task.resize(MMUAddrWidth bits), True, writeMask, writeObj, reqIssued) { rd =>
      when((task ^ destOopPtr) >> io.ConfigIO.LogOfHRGrainBytes === U(0)){
        resetState()
      }.otherwise{
        state := overall_state.s_readHR
      }
    }
  }

  val heap_region = RegInit(U(0, MMUAddrWidth bits))
  when(state === overall_state.s_readHR){
    issueReq(io.Mreq, (io.ConfigIO.HeapRegionBiasedBase + (task >> io.ConfigIO.HeapRegionShiftBy) * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits), False, U(0), U(0), reqIssued){ rd =>
      heap_region := rd(GCElementWidth -1 downto 0)
      state := overall_state.s_readHRType
    }
  }

  when(state === overall_state.s_readHRType){
    issueReq(io.Mreq, (heap_region + U"xbc").resize(MMUAddrWidth bits), False, U(0), U(0), reqIssued){ rd =>
      when((rd(31 downto 0) & U(x"2", 32 bits)) === U(0)){
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
      val regionAttrPtr = (io.ConfigIO.RegionAttrBiasedBase + (destOopPtr >> io.ConfigIO.RegionAttrShiftBy) * GCHeapRegionAttr_Size).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, regionAttrPtr, False, U(0), U(0), reqIssued){ rd =>
        reqDone := True
        regionAttr := rd(15 downto 0)
      }
    }

    io.Process2Aop.Valid := reqDone
    io.Process2Aop.Task := task
    io.Process2Aop.RegionAttr := regionAttr

    when(io.Process2Aop.Valid && io.Process2Aop.Ready){
      state := overall_state.s_waitDone2
      reqDone := False

      if(DebugEnable){
        report(Seq(
          "[GCOopProcess<", io.DebugTimeStamp,
          ">]Send the task to Aop",
          "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_waitDone2){
    when(io.Process2Aop.Done){
      resetState()

      if(DebugEnable){
        report(Seq(
          "[GCOopProcess<", io.DebugTimeStamp,
          ">]The task has done",
          "\n"
        ))
      }
    }
  }
}

object GCOopProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCOopProcess())
}