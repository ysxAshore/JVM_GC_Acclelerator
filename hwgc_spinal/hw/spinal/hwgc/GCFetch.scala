package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCFetch read task from TaskStack, the dispatch to arrayProcess and oopProcess */
class GCFetch extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val FetchMReq = master(new LocalMMUIO)
    val Stack2Fetch = slave Stream UInt(MMUAddrWidth bits)
    val Fetch2ArrayProcess = master(new GCFetch2ProcessUnit)
    val Fetch2OopProcess = master(new GCFetch2ProcessUnit)
    val DebugTimeStamp = in(UInt(MMUDataWidth bits))
  }

  // default value
  io.FetchMReq.Request.valid := False
  io.FetchMReq.Request.payload.clearAll()
  io.FetchMReq.Response.ready := False

  io.Stack2Fetch.ready := False

  io.Fetch2ArrayProcess.Valid := False
  io.Fetch2ArrayProcess.Task := U(0)
  io.Fetch2ArrayProcess.OopType := U(0)
  io.Fetch2ArrayProcess.SrcOopPtr := U(0)
  io.Fetch2ArrayProcess.MarkWord := U(0)

  io.Fetch2OopProcess.Valid := False
  io.Fetch2OopProcess.Task := U(0)
  io.Fetch2OopProcess.OopType := U(0)
  io.Fetch2OopProcess.SrcOopPtr := U(0)
  io.Fetch2OopProcess.MarkWord := U(0)

  object overall_state extends SpinalEnum {
    val s_idle, s_readOop, s_readMW, s_send, s_waitDone = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val task = RegInit(U(0, MMUAddrWidth bits))
  val oopType = RegInit(U(0, GCOopTypeWidth bits))

  // mem related regs
  val issued = RegInit(False)
  val memData = RegInit(U(0, MMUAddrWidth bits))

  // idle: take task from stack and sub the tag
  when(state === overall_state.s_idle){
    io.Stack2Fetch.ready := True
    when(io.Stack2Fetch.fire){
      val payload = io.Stack2Fetch.payload
      when(payload(GCOopTypeWidth - 1 downto 0) === U(OopTag, GCOopTypeWidth bits)){
        oopType := U(CommonOop, GCOopTypeWidth bits)
        task := (payload - U(OopTag)).resize(MMUAddrWidth bits)
      }.otherwise{
        oopType := U(PartialArrayOop, GCOopTypeWidth bits)
        task := (payload - U(PartialArrayTag)).resize(MMUAddrWidth bits)
      }

      // reset mem flags for next task
      issued := False
      memData := U(0)

      state := overall_state.s_readOop

      if(DebugEnable){
        report(Seq(
          "[GCFetch<", io.DebugTimeStamp,
          ">]Receice task from TaskStack ", payload,
          "\n"
        ))
      }
    }
  }

  // readOop: oopTag -> send mreq
  when(state === overall_state.s_readOop){
    when(oopType === U(CommonOop, GCOopTypeWidth bits)){
      issueReq(io.FetchMReq, task, False, U(0), U(0), issued){ rd =>
        memData := rd
        state := overall_state.s_readMW
      }
    }.otherwise{
      memData := task
      state := overall_state.s_readMW
    }
  }

  val markWord = RegInit(U(0, MMUDataWidth bits))
  when(state === overall_state.s_readMW){
    issueReq(io.FetchMReq, memData + MarkWordOff, False, U(0), U(0), issued){ rd =>
      markWord := rd
      state := overall_state.s_send
    }
  }


  // send
  val processUnit = Mux(oopType === U(CommonOop, GCOopTypeWidth bits), io.Fetch2OopProcess, io.Fetch2ArrayProcess)
  when(state === overall_state.s_send){
    // Mux(cond, A, B) could read, but not support write
    when(oopType === U(CommonOop, GCOopTypeWidth bits)){
      io.Fetch2OopProcess.Valid := True
      io.Fetch2OopProcess.Task := task
      io.Fetch2OopProcess.OopType := oopType
      io.Fetch2OopProcess.SrcOopPtr := memData
      io.Fetch2OopProcess.MarkWord := markWord
    }.otherwise{
      io.Fetch2ArrayProcess.Valid := True
      io.Fetch2ArrayProcess.Task := task
      io.Fetch2ArrayProcess.OopType := oopType
      io.Fetch2ArrayProcess.SrcOopPtr := memData
      io.Fetch2ArrayProcess.MarkWord := markWord
    }
    when(processUnit.Valid && processUnit.Ready){
      state := overall_state.s_waitDone

      if(DebugEnable){
        report(Seq(
          "[GCFetch<", io.DebugTimeStamp,
          ">]Dispatch Task ", task,
          ", OopType ", oopType,
          ", SrcOopPtr ", memData,
          ", MarkWord ", markWord,
          "success!",
          "\n"
        ))
      }
    }
  }

  // waitDone
  when(state === overall_state.s_waitDone){
    when(processUnit.Done){
      state := overall_state.s_idle

      if(DebugEnable){
        report(Seq(
          "[GCFetch<", io.DebugTimeStamp,
          ">]Task ", task,
          "has done",
          "\n"
        ))
      }
    }
  }
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(new GCFetch())
}
