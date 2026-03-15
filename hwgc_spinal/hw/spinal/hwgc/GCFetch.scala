package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class GcFetchData() extends Bundle with GCParameters {
  val valid = Bool()
  val task = UInt(GCElementWidth bits)
  val oopType = UInt(GCOopTypeWidth bits)
  val fromObj = UInt(GCElementWidth bits)
  val markWord = UInt(GCElementWidth bits)
  val klassPtr = UInt(GCElementWidth bits)
}

/* GCFetch read task from TaskStack, the dispatch to arrayProcess and oopProcess */
class GCFetch extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)

    val Stack2Fetch = slave Stream UInt(GCElementWidth bits)
    val StackPreFetch = slave Stream UInt(GCElementWidth bits)
    val Trace2Fetch = slave Stream UInt(GCElementWidth bits)

    val CopyDone = in Bool()

    val Fetch2ArrayProcess = master(new GCToProcessUnit)
    val Fetch2OopProcess = master(new GCToProcessUnit)

    val ConfigIO = slave(new GCFetchConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := False

  io.Stack2Fetch.ready := False
  io.StackPreFetch.ready := False
  io.Trace2Fetch.ready := True

  io.Fetch2ArrayProcess.clearIn()
  io.Fetch2OopProcess.clearIn()

  object overall_state extends SpinalEnum {
    val s_idle, s_readOop, s_readMW, s_send, s_waitDone = newElement()
  }

  def receiveTask(payload: UInt, data: GcFetchData) : Unit = {
    when(payload(GCOopTagWidth - 1 downto 0) === U(PartialArrayTag, GCOopTagWidth bits)){
      data.oopType := U(PartialArrayOop)
      data.task := payload - U(PartialArrayTag)
    }.otherwise{
      data.oopType := U(NotArrayOop, GCOopTypeWidth bits)
      data.task := payload - payload(GCOopTagWidth - 1 downto 0)
    }
  }

  def readOop(payload: GcFetchData)(afterAccept: UInt => Unit): Unit = {
    when(payload.oopType === U(NotArrayOop)){
      issueReq(io.Mreq, payload.task, False, U(8), U(0), issued){ rd =>
        val fromObj = Mux(io.ConfigIO.UseCompressedOop, (io.ConfigIO.CompressedOopBase + (rd(31 downto 0) << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth), rd(GCElementWidth - 1 downto 0))
        afterAccept(fromObj)
      }
    }.otherwise{
      afterAccept(payload.task)
    }
  }

  def readMarkWord(payload: GcFetchData)(afterAccept: UInt => Unit): Unit = {
    issueReq(io.Mreq, payload.fromObj, False, U(16), U(0), issued){ rd =>
      afterAccept(rd)
    }
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCFetch<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def driveProcessUnit(target: GCToProcessUnit, payload: GcFetchData): Unit = {
    target.Valid     := True
    target.Task      := payload.task
    target.OopType   := payload.oopType
    target.SrcOopPtr := payload.fromObj
    target.MarkWord  := payload.markWord
    target.KlassPtr  := payload.klassPtr
  }

  val issued = RegInit(False)
  val copyDone_reg = RegInit(False)

  val state = RegInit(overall_state.s_idle)

  val push_state = RegInit(overall_state.s_idle)
  val push_data = RegInit(GcFetchData().getZero)
  val main_data = RegInit(GcFetchData().getZero)

  when(io.CopyDone){
    copyDone_reg := True
  }

  switch(push_state){
    is(overall_state.s_idle){
      io.Trace2Fetch.ready := True
      when(io.Trace2Fetch.ready && io.Trace2Fetch.valid){
        val payload = io.Trace2Fetch.payload
        when(state === overall_state.s_idle){
          receiveTask(payload, main_data)
          state := overall_state.s_readOop
          dbg(Seq("Receive task ", payload - payload(GCOopTagWidth - 1 downto 0)))
        }.otherwise{
          receiveTask(payload, push_data)
          push_data.valid := True
          push_state := overall_state.s_readOop
        }
      }
    }

    is(overall_state.s_readOop){
      when(main_data.oopType === U(PartialArrayOop) || io.CopyDone || copyDone_reg){
        readOop(push_data){ fromObj =>
          copyDone_reg := False
          when(state === overall_state.s_idle){
            state := overall_state.s_readMW
            push_state := overall_state.s_idle
            push_data.valid := False

            main_data.task := push_data.task
            main_data.oopType := push_data.oopType
            main_data.fromObj := fromObj
            dbg(Seq("Receive task ", push_data.task))
          }.otherwise {
            push_data.fromObj := fromObj
            push_state := overall_state.s_readMW
          }
        }
      }
    }

    is(overall_state.s_readMW){
      readMarkWord(push_data){ rd =>
        when(state === overall_state.s_idle){
          state := overall_state.s_send
          push_state := overall_state.s_idle
          push_data.valid := False

          main_data.task := push_data.task
          main_data.oopType := push_data.oopType
          main_data.fromObj := push_data.fromObj
          main_data.markWord := rd(GCElementWidth - 1 downto 0)
          main_data.klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          dbg(Seq("Receive task ", push_data.task))
        }.otherwise{
          push_data.markWord := rd(GCElementWidth - 1 downto 0)
          push_data.klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

          push_state := overall_state.s_send
        }
      }
    }

    is(overall_state.s_send){
      when(state === overall_state.s_idle) {
        main_data := push_data
        state := push_state

        push_state := overall_state.s_idle
        push_data.valid := False

        dbg(Seq("Receive task ", push_data.task))
      }
    }
  }

  val targetDone = Mux(main_data.oopType === U(NotArrayOop), io.Fetch2OopProcess.Done, io.Fetch2ArrayProcess.Done)
  val targetDone_reg = RegInit(False)
  when(targetDone){
    targetDone_reg := True
  }

  switch(state){
    is(overall_state.s_idle){
      io.Stack2Fetch.ready := !push_data.valid && !io.Trace2Fetch.valid

      when(io.Stack2Fetch.fire){
        state := overall_state.s_readOop

        val payload = io.Stack2Fetch.payload
        receiveTask(payload, main_data)
        main_data.fromObj := U(0)

        dbg(Seq("Receive task ", payload))
      }
    }

    is(overall_state.s_readOop){
      readOop(main_data){ fromObj =>
        main_data.fromObj := fromObj
        state := overall_state.s_readMW
      }
    }

    is(overall_state.s_readMW){
      readMarkWord(main_data) { rd =>
        main_data.markWord := rd(GCElementWidth - 1 downto 0)
        main_data.klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

        state := overall_state.s_send
      }
    }

    is(overall_state.s_send){
      // Mux(cond, A, B) could read, but not support write
      when(main_data.oopType === U(NotArrayOop)){
        driveProcessUnit(io.Fetch2OopProcess, main_data)
      }.otherwise{
        driveProcessUnit(io.Fetch2ArrayProcess, main_data)
      }

      val targetUnit = Mux(main_data.oopType === U(NotArrayOop), io.Fetch2OopProcess, io.Fetch2ArrayProcess)
      when(targetUnit.Valid && targetUnit.Ready){
        state := overall_state.s_waitDone
        dbg(Seq("Dispatch Task=", main_data.task, " OopType=", main_data.oopType, " SrcOopPtr=", main_data.fromObj, " MarkWord=", main_data.markWord, " KlassPtr = ", main_data.klassPtr, " success!"))
      }
    }

    is(overall_state.s_waitDone){
      when(targetDone || targetDone_reg) {
        targetDone_reg := False
        state := overall_state.s_idle
        dbg(Seq("Task=", main_data.task, " done"))
      }
    }
  }
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(new GCFetch())
}