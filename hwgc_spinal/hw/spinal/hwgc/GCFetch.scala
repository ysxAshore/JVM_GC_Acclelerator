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

  io.Fetch2ArrayProcess.clearIn()
  io.Fetch2OopProcess.clearIn()

  object overall_state extends SpinalEnum {
    val s_idle, s_readOop, s_readMW, s_send, s_waitDone = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val task = RegInit(U(0, GCElementWidth bits))
  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val fromObj = RegInit(U(0, GCElementWidth bits))
  val markWord = RegInit(U(0, GCElementWidth bits))
  val klassPtr = RegInit(U(0, GCElementWidth bits))
  val issued = RegInit(False)

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCFetch<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def driveProcessUnit(target: GCFetch2ProcessUnit): Unit = {
    target.Valid     := True
    target.Task      := task
    target.OopType   := oopType
    target.SrcOopPtr := fromObj
    target.MarkWord  := markWord
    target.KlassPtr  := klassPtr
  }

  switch(state){
    is(overall_state.s_idle){
      io.Stack2Fetch.ready := True
      when(io.Stack2Fetch.fire){
        val payload = io.Stack2Fetch.payload
        when(payload(GCOopTagWidth - 1 downto 0) === U(PartialArrayTag, GCOopTagWidth bits)){
          oopType := U(PartialArrayOop)
          task := payload - U(PartialArrayTag)
        }.otherwise{
          oopType := U(NotArrayOop, GCOopTypeWidth bits)
          task := payload - payload(GCOopTagWidth - 1 downto 0)
        }

        issued := False
        fromObj := U(0)
        state := overall_state.s_readOop

        dbg(Seq("Receive task from TaskStack Module ", payload))
      }
    }

    is(overall_state.s_readOop){
      when(oopType === U(NotArrayOop)){
        issueReq(io.Mreq, task, False, U(0), U(0), issued){ rd =>
          fromObj := Mux(io.ConfigIO.UseCompressedOop, (io.ConfigIO.CompressedOopBase + (rd(31 downto 0) << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth), rd(GCElementWidth - 1 downto 0))
          state := overall_state.s_readMW
        }
      }.otherwise{
        fromObj := task
        state := overall_state.s_readMW
      }
    }

    is(overall_state.s_readMW){
      issueReq(io.Mreq, fromObj, False, U(0), U(0), issued){ rd =>
        markWord := rd(GCElementWidth - 1 downto 0)
        klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        state := overall_state.s_send
      }
    }

    is(overall_state.s_send){
      // Mux(cond, A, B) could read, but not support write
      when(oopType === U(NotArrayOop)){
        driveProcessUnit(io.Fetch2OopProcess)
      }.otherwise{
        driveProcessUnit(io.Fetch2ArrayProcess)
      }

      val targetUnit = Mux(oopType === U(NotArrayOop), io.Fetch2OopProcess, io.Fetch2ArrayProcess)
      when(targetUnit.Valid && targetUnit.Ready){
        state := overall_state.s_waitDone
        dbg(Seq("Dispatch Task=", task, " OopType=", oopType, " SrcOopPtr=", fromObj, " MarkWord=", markWord, " KlassPtr = ", klassPtr, " success!"))
      }
    }

    is(overall_state.s_waitDone){
      val targetDone = Mux(oopType === U(NotArrayOop), io.Fetch2OopProcess.Done, io.Fetch2ArrayProcess.Done)
      when(targetDone) {
        state := overall_state.s_idle
        dbg(Seq("Task=", task, " done"))
      }
    }
  }
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(new GCFetch())
}