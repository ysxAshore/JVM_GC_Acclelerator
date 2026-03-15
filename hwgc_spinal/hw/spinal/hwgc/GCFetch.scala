package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class GcFetchData() extends Bundle with GCParameters {
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

  io.Stack2Fetch.ready := False
  io.StackPreFetch.ready := False
  io.Trace2Fetch.ready := False

  io.Fetch2ArrayProcess.clearIn()
  io.Fetch2OopProcess.clearIn()

  object overall_state extends SpinalEnum {
    val s_idle, s_readOop, s_readMW, s_send, s_waitDone = newElement()
  }
  object MreqOwner extends SpinalEnum {
    val none, mainReadOop, mainReadMW, pushReadOop, pushReadMW, prefetchReadOop, prefetchReadMW = newElement()
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

  def decodeReadOopResp(rd: UInt): UInt = {
    Mux(io.ConfigIO.UseCompressedOop, (io.ConfigIO.CompressedOopBase + (rd(31 downto 0) << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth), rd(GCElementWidth - 1 downto 0))
  }

  def decodeMarkWord(rd: UInt): UInt = rd(GCElementWidth - 1 downto 0)
  def decodeKlassPtr(rd: UInt): UInt = rd(GCElementWidth * 2 - 1 downto GCElementWidth)

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

  val PreFetchBuffer = 8
  val PreFetchBufferWidth = log2Up(PreFetchBuffer)

  val issued = RegInit(False)
  val mreqOwner = RegInit(MreqOwner.none)

  val state = RegInit(overall_state.s_idle)

  val push_state = RegInit(overall_state.s_idle)
  val push_data = RegInit(GcFetchData().getZero)
  val main_data = RegInit(GcFetchData().getZero)

  val preFetch_state = RegInit(overall_state.s_idle)
  val preFetchBuffer = Vec.fill(PreFetchBuffer)(RegInit(GcFetchData().getZero))
  val preFetchBufferDone = Vec.fill(PreFetchBuffer)(RegInit(False))

  val buffer_ptr = RegInit(U(0, PreFetchBufferWidth bits))
  val buffer_top = RegInit(U(0, PreFetchBufferWidth bits)) // top存
  val buffer_bottom = RegInit(U(0, PreFetchBufferWidth bits)) // bottom取
  val buffer_count = RegInit(U(0, PreFetchBufferWidth + 1 bits))
  val buffer_free = U(PreFetchBufferWidth, PreFetchBufferWidth + 1 bits) - buffer_count

  val targetDone = Mux(main_data.oopType === U(NotArrayOop), io.Fetch2OopProcess.Done, io.Fetch2ArrayProcess.Done)
  val copyDoneSeen = RegInit(False)
  val targetDoneSeen = RegInit(False)
  val waitPreFetch = RegInit(False)

  val wantMainReadOop     = state === overall_state.s_readOop && main_data.oopType === U(NotArrayOop)
  val wantMainReadMW      = state === overall_state.s_readMW
  val wantPushReadOop     = (push_state === overall_state.s_readOop) && (push_data.oopType === U(NotArrayOop)) && (main_data.oopType === U(PartialArrayOop) || io.CopyDone || copyDoneSeen)
  val wantPushReadMW      = push_state === overall_state.s_readMW
  val withoutMainReq = !wantMainReadMW && !wantMainReadOop
  val withoutPushReq = !wantPushReadMW && !wantPushReadOop
  val wantPrefetchReadOop = withoutMainReq && withoutPushReq && preFetch_state === overall_state.s_readOop && preFetchBuffer(buffer_ptr).oopType === U(NotArrayOop)
  val wantPrefetchReadMW  = withoutPushReq && withoutPushReq && preFetch_state === overall_state.s_readMW

  when(io.CopyDone){
    copyDoneSeen := True
  }
  when(targetDone){
    targetDoneSeen := True
  }

  switch(push_state){
    is(overall_state.s_idle){
      io.Trace2Fetch.ready := True
      when(io.Trace2Fetch.ready && io.Trace2Fetch.valid){
        val payload = io.Trace2Fetch.payload
        when(state === overall_state.s_idle){
          receiveTask(payload, main_data)
          state := overall_state.s_readOop
        }.otherwise{
          receiveTask(payload, push_data)
          push_state := overall_state.s_readOop
        }
      }
    }

    is(overall_state.s_readOop){
      when(push_data.oopType === U(PartialArrayOop)) {
        push_data.fromObj := push_data.task
        push_state := overall_state.s_readMW
      }
    }

    is(overall_state.s_send){
      when(state === overall_state.s_idle) {
        main_data := push_data
        state := overall_state.s_send
        push_state := overall_state.s_idle
      }
    }
  }

  switch(preFetch_state){
    is(overall_state.s_idle) {
      io.StackPreFetch.ready := buffer_free =/= U(0)
      when(io.StackPreFetch.valid && io.StackPreFetch.ready) {
        buffer_ptr := buffer_top
        buffer_top := buffer_top + 1
        buffer_count := buffer_count + 1
        preFetchBufferDone(buffer_top) := False
        preFetch_state := overall_state.s_readOop
        receiveTask(io.StackPreFetch.payload, preFetchBuffer(buffer_top))
      }
    }

    is(overall_state.s_readOop){
      when(preFetchBuffer(buffer_ptr).oopType === U(PartialArrayOop)) {
        preFetchBuffer(buffer_ptr).fromObj := preFetchBuffer(buffer_ptr).task
        preFetch_state := overall_state.s_readMW
      }
    }
  }

  switch(state){
    is(overall_state.s_idle){
      io.Stack2Fetch.ready := push_state === overall_state.s_idle && !io.Trace2Fetch.valid && !waitPreFetch

      when(io.Stack2Fetch.fire){
        val payload = io.Stack2Fetch.payload
        when(preFetchBuffer(buffer_bottom).task === payload - payload(GCOopTagWidth - 1 downto 0)){
          when(preFetchBufferDone(buffer_bottom)){
            main_data := preFetchBuffer(buffer_bottom)
            state := overall_state.s_send
            buffer_count := buffer_count - 1
            buffer_bottom := buffer_bottom + 1
          }.otherwise{
            waitPreFetch := True
          }
        }.otherwise{
          state := overall_state.s_readOop
          receiveTask(payload, main_data)
          main_data.fromObj := U(0)
        }
      }

      when(waitPreFetch && preFetchBufferDone(buffer_bottom)){
        main_data := preFetchBuffer(buffer_bottom)
        state := overall_state.s_send
        buffer_count := buffer_count - 1
        buffer_bottom := buffer_bottom + 1
        waitPreFetch := False
      }
    }

    is(overall_state.s_readOop){
      when(main_data.oopType === U(PartialArrayOop)){
        main_data.fromObj := main_data.task
        state := overall_state.s_readMW
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
      when(targetDone || targetDoneSeen) {
        targetDoneSeen := False
        state := overall_state.s_idle
        dbg(Seq("Task=", main_data.task, " done"))
      }
    }
  }

  when(!issued){
    val data = Mux(wantMainReadOop || wantMainReadMW, main_data, Mux(wantPushReadOop || wantPushReadMW, push_data, preFetchBuffer(buffer_ptr)))
    io.Mreq.Request.valid := !withoutPushReq || !withoutMainReq || wantPrefetchReadMW || wantPrefetchReadOop
    io.Mreq.RequestSize.valid := !withoutPushReq || !withoutMainReq || wantPrefetchReadMW || wantPrefetchReadOop
    io.Mreq.Request.payload.RequestWStrb := U(0)
    io.Mreq.Request.payload.RequestData := U(0)
    io.Mreq.Request.payload.RequestType_isWrite := False
    io.Mreq.Request.payload.RequestSourceID := io.Mreq.ConherentRequsetSourceID.payload
    when(wantMainReadOop || wantPushReadOop || wantPrefetchReadOop){
      io.Mreq.RequestSize.payload := U(8)
      io.Mreq.Request.payload.RequestVirtualAddr := data.task
    }.elsewhen(wantMainReadMW || wantPushReadMW || wantPrefetchReadMW){
      io.Mreq.RequestSize.payload := U(16)
      io.Mreq.Request.payload.RequestVirtualAddr := data.fromObj
    }

    when(io.Mreq.Request.fire){
      issued := True
      mreqOwner := Mux(wantMainReadOop, MreqOwner.mainReadOop,
        Mux(wantPushReadOop, MreqOwner.pushReadOop,
          Mux(wantMainReadMW, MreqOwner.mainReadMW,
            Mux(wantPushReadMW, MreqOwner.pushReadMW,
              Mux(wantPrefetchReadOop, MreqOwner.prefetchReadOop, MreqOwner.prefetchReadMW)))))
    }
  }

  io.Mreq.Response.ready := issued
  when(io.Mreq.Response.fire){
    issued := False
    mreqOwner := MreqOwner.none

    val rd = io.Mreq.Response.payload.ResponseData
    switch(mreqOwner){
      is(MreqOwner.mainReadOop){
        main_data.fromObj := decodeReadOopResp(rd)
        state := overall_state.s_readMW
      }
      is(MreqOwner.mainReadMW){
        main_data.markWord := decodeMarkWord(rd)
        main_data.klassPtr := decodeKlassPtr(rd)
        state := overall_state.s_send
      }
      is(MreqOwner.pushReadOop){
        push_data.fromObj := decodeReadOopResp(rd)
        push_state := overall_state.s_readMW
      }
      is(MreqOwner.pushReadMW){
        copyDoneSeen := False
        when(state === overall_state.s_idle){
          main_data.task := push_data.task
          main_data.oopType := push_data.oopType
          main_data.fromObj := push_data.fromObj
          main_data.markWord := decodeMarkWord(rd)
          main_data.klassPtr := decodeKlassPtr(rd)
          state := overall_state.s_send
          push_state := overall_state.s_idle
        }.otherwise{
          push_data.markWord := decodeMarkWord(rd)
          push_data.klassPtr := decodeKlassPtr(rd)
          push_state := overall_state.s_send
        }
      }
      is(MreqOwner.prefetchReadOop){
        preFetchBuffer(buffer_ptr).fromObj := decodeReadOopResp(rd)
        preFetch_state := overall_state.s_readMW
      }
      is(MreqOwner.prefetchReadMW){
        preFetch_state := overall_state.s_idle
        when(state === overall_state.s_idle && (io.Trace2Fetch.fire || waitPreFetch)){
          waitPreFetch := False
          main_data.task := preFetchBuffer(buffer_bottom).task
          main_data.oopType := preFetchBuffer(buffer_bottom).oopType
          main_data.fromObj := preFetchBuffer(buffer_bottom).fromObj
          main_data.markWord := decodeMarkWord(rd)
          main_data.klassPtr := decodeKlassPtr(rd)
          state := overall_state.s_send
          buffer_bottom := buffer_bottom + 1
          buffer_count := buffer_count - 1
        }.otherwise{
          preFetchBuffer(buffer_ptr).markWord := decodeMarkWord(rd)
          preFetchBuffer(buffer_ptr).klassPtr := decodeKlassPtr(rd)
          preFetchBufferDone(buffer_ptr) := True
        }
      }
    }
  }
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(new GCFetch())
}