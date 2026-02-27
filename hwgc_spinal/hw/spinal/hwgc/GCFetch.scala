package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCFetch read task from TaskStack, the dispatch to arrayProcess and oopProcess */
class GCFetch extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val Stack2Fetch = slave Stream UInt(GCElementWidth bits)
    val Fetch2ArrayProcess = master(new GCFetch2ProcessUnit)
    val Fetch2OopProcess = master(new GCFetch2ProcessUnit)
    val ConfigIO = slave(new GCFetchConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

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

  val task = RegInit(U(0, GCElementWidth bits))
  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val from_obj = RegInit(U(0, GCElementWidth bits))

  // mem related regs
  val issued = RegInit(False)

  // idle: take task from stack and sub the tag
  when(state === overall_state.s_idle){
    io.Stack2Fetch.ready := True
    when(io.Stack2Fetch.fire){
      val payload = io.Stack2Fetch.payload
      when(payload(GCOopTagWidth - 1 downto 0) === U(PartialArrayTag, GCOopTagWidth bits)){
        oopType := U(PartialArrayOop, GCOopTypeWidth bits)
        task := (payload - U(PartialArrayTag)).resize(GCElementWidth bits)
      }.otherwise{
        oopType := U(NotArrayOop, GCOopTypeWidth bits)
        task := (payload - payload(GCOopTagWidth - 1 downto 0)).resize(GCElementWidth bits)
      }

      // reset mem flags for next task
      issued := False
      from_obj := U(0)

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
    when(oopType === U(NotArrayOop, GCOopTypeWidth bits)){
      issueReq(io.Mreq, task, False, U(0), U(0), issued){ rd =>
        when(io.ConfigIO.UseCompressedOop){
          from_obj := (io.ConfigIO.CompressedOopBase + (rd << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth bits)
        }.otherwise{
          from_obj := rd(GCElementWidth - 1 downto 0)
        }
        state := overall_state.s_readMW
      }
    }.otherwise{
      from_obj := task
      state := overall_state.s_readMW
    }
  }

  val markWord = RegInit(U(0, GCElementWidth bits))
  when(state === overall_state.s_readMW){
    issueReq(io.Mreq, from_obj.resize(MMUAddrWidth bits), False, U(0), U(0), issued){ rd =>
      markWord := rd(GCElementWidth - 1 downto 0)
      state := overall_state.s_send
    }
  }


  // send
  val processUnit = Mux(oopType === U(NotArrayOop, GCOopTypeWidth bits), io.Fetch2OopProcess, io.Fetch2ArrayProcess)
  when(state === overall_state.s_send){
    // Mux(cond, A, B) could read, but not support write
    when(oopType === U(NotArrayOop, GCOopTypeWidth bits)){
      io.Fetch2OopProcess.Valid := True
      io.Fetch2OopProcess.Task := task
      io.Fetch2OopProcess.OopType := oopType
      io.Fetch2OopProcess.SrcOopPtr := from_obj
      io.Fetch2OopProcess.MarkWord := markWord
    }.otherwise{
      io.Fetch2ArrayProcess.Valid := True
      io.Fetch2ArrayProcess.Task := task
      io.Fetch2ArrayProcess.OopType := oopType
      io.Fetch2ArrayProcess.SrcOopPtr := from_obj
      io.Fetch2ArrayProcess.MarkWord := markWord
    }

    when(processUnit.Valid && processUnit.Ready){
      state := overall_state.s_waitDone

      if(DebugEnable){
        report(Seq(
          "[GCFetch<", io.DebugTimeStamp,
          ">]Dispatch Task ", task,
          ", OopType ", oopType,
          ", SrcOopPtr ", from_obj,
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
