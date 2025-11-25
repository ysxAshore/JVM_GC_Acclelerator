package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCFetch read task from TaskStack, the dispatch to arrayProcess and oopProcess */
class GCFetch extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Stack2Fetch = slave Stream UInt(MMUAddrWidth bits)
    val Fetch2ArrayProcess = master(new ProcessUnit)
    val Fetch2OopProcess = master(new ProcessUnit)
    val FetchMReq = master(new LocalMMUIO)
  }

  // default value
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

  io.FetchMReq.Request.valid := False
  io.FetchMReq.Request.payload.clearAll()
  io.FetchMReq.Response.ready := False

  object overall_state extends SpinalEnum {
    val s_idle, s_readOop, s_readMW, s_send, s_waitDone = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val task = RegInit(U(0, MMUAddrWidth bits))

  // mem related regs
  val issued = RegInit(False)
  val memData = RegInit(U(0, MMUAddrWidth bits))

  // idle: take task from stack
  when(state === overall_state.s_idle){
    io.Stack2Fetch.ready := True
    when(io.Stack2Fetch.fire){
      val payload = io.Stack2Fetch.payload
      when(payload(GCOopTypeWidth - 1 downto 0) === U(OopTag)){
        oopType := U(CommonOop)
        task := payload - U(OopTag)
      }.otherwise{
        oopType := U(PartialArrayOop)
        task := payload - U(PartialArrayTag)
      }

      // reset mem flags for next task
      issued := False
      memData := U(0)

      state := overall_state.s_readOop
    }
  }

  // readOop: oopTag -> send mreq
  when(state === overall_state.s_readOop){
    when(oopType === U(CommonOop)){
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
  when(state === overall_state.s_send){
    when(oopType === U(CommonOop)){
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
    // Mux(cond, A, B)会生成 A 和 B 同时被驱动的逻辑路径，是根据组合逻辑选择数据
    val processUnit = Mux(oopType === U(CommonOop), io.Fetch2OopProcess, io.Fetch2ArrayProcess)
    when(processUnit.Valid && processUnit.Ready){
      state := overall_state.s_waitDone
    }
  }

  // waitDone
  when(state === overall_state.s_waitDone){
    val processUnit = Mux(oopType === U(CommonOop), io.Fetch2OopProcess, io.Fetch2ArrayProcess)
    when(processUnit.Done){
      state := overall_state.s_idle
    }
  }
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(new GCFetch())
}
