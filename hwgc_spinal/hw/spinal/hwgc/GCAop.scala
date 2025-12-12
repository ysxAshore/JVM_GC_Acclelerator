package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class AopStageData() extends Bundle with HWParameters{
  val dest = UInt(MMUAddrWidth bits)
  val pss = UInt(MMUAddrWidth bits)
  val ct_ptr = UInt(MMUAddrWidth bits)
  val byte_map = UInt(MMUAddrWidth bits)
  val card_index = UInt(MMUDataWidth bits)
  val index = UInt(MMUDataWidth bits)
  val buffer = UInt(MMUAddrWidth bits)
  val res = UInt(MMUDataWidth bits)
}

class GCAop extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Aop = slave (new AopParameters)
    val aopMReqs = Vec.fill(GCaopWorkStages)(new LocalMMUIO)
    val DebugTimeStamp = in(UInt(MMUDataWidth bits))
    aopMReqs.foreach(master(_))
  }

  // default value
  for(i <- 0 until GCaopWorkStages){
    io.aopMReqs(i).Request.valid := False
    io.aopMReqs(i).Request.payload.clearAll()
    io.aopMReqs(i).Response.ready := False
  }

  case class Stage() extends Area{
    val valid = RegInit(False)
    val reqDone = RegInit(False)
    val reqIssued = RegInit(False)
    val reg = RegInit(AopStageData().getZero)
    val responseData = RegInit(U(0, MMUDataWidth bits))
  }

  val stages = Seq.fill(GCaopWorkStages)(Stage())

  io.Aop.Ready := !stages(0).valid
  io.Aop.Done := !stages.map(_.valid).reduce(_ || _)

  when(io.Aop.Valid && io.Aop.Ready){
    val s0 = stages(0)
    s0.valid := io.Aop.RegionAttr(7 downto 0) =/= U(0)
    s0.reqDone := False
    s0.reqIssued := False
    s0.reg.pss := io.Aop.ParScanThreadStatePtr
    s0.reg.dest := io.Aop.Task
    s0.reg.ct_ptr := io.Aop.CardTablePtr
    s0.reg.byte_map := U(0)
    s0.reg.card_index := U(0)
    s0.reg.index := U(0)
    s0.reg.buffer := U(0)
    s0.reg.res := U(0)
    if(DebugEnable){
      report(Seq(
        "[GCAop<", io.DebugTimeStamp,
        ">]Receive task",
        "\n"
      ))
    }
  }

  def mmuOpForIndex(i: Int, s: AopStageData): (Bool, UInt, Bool, UInt, UInt) = {
    val valid = if(i != 4) True else False
    val write = if(i == 6 || i == 7 || i == 8) True else False
    val addr = UInt(MMUAddrWidth bits)
    val wdata = UInt(MMUDataWidth bits)
    val wmask = Mux(write, allBytesOnes, U(0, MMUDataWidth / 8 bits))
    i match {
      case 0 => addr := (s.ct_ptr + BYTE_MAP_OFFSET).resize(MMUAddrWidth bits)
      case 1 => addr := (s.ct_ptr + BYTE_MAP_BASE_OFFSET).resize(MMUAddrWidth bits)
      case 2 => addr := (s.pss + LAST_ENQUEUED_CARD_OFFSET).resize(MMUAddrWidth bits)
      case 3 => addr := (s.pss + QSET_OFFSET + QSET_QUEUE_OFFSET + INDEX_OFFSET).resize(MMUAddrWidth bits)
      case 5 => addr := (s.pss + QSET_OFFSET + QSET_QUEUE_OFFSET + BUFFER_OFFSET).resize(MMUAddrWidth bits)
      case 6 => addr := (s.buffer + (s.index - U(1)) * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
      case 7 => addr := (s.pss + QSET_OFFSET + QSET_QUEUE_OFFSET + INDEX_OFFSET).resize(MMUAddrWidth bits)
      case 8 => addr := (s.pss + LAST_ENQUEUED_CARD_OFFSET).resize(MMUAddrWidth bits)
      case _ => addr := U(0)
    }
    i match {
      case 6 => wdata := s.res
      case 7 => wdata := ((s.index - U(1)) * U(GCObjectPtr_Size)).resize(MMUDataWidth bits)
      case 8 => wdata := s.card_index
      case _ => wdata := U(0)
    }
    (valid, addr, write, wmask, wdata)
  }

  // helper: reset stage
  def resetStage(i: Int): Unit = {
    stages(i).valid := False
    stages(i).reqDone := False
    stages(i).reqIssued := False
  }

  def advance(from: Int,
              to: Int,
              changeByteMap: Option[UInt] = None,
              changeCardIndex: Option[UInt] = None,
              changeRes: Option[UInt] = None,
              changeIndex: Option[UInt] = None,
              changeBuffer: Option[UInt] = None,
             ): Unit = {
    val sFrom = stages(from)
    val sTo   = stages(to)

    when(sFrom.valid && sFrom.reqDone && !sTo.valid) {
      resetStage(from)

      sTo.valid := True
      sTo.reg.pss := sFrom.reg.pss
      sTo.reg.dest := sFrom.reg.dest
      sTo.reg.ct_ptr := sFrom.reg.ct_ptr
      sTo.reg.byte_map := changeByteMap.getOrElse(sFrom.reg.byte_map)
      sTo.reg.card_index := changeCardIndex.getOrElse(sFrom.reg.card_index)
      sTo.reg.index := changeIndex.getOrElse(sFrom.reg.index)
      sTo.reg.buffer := changeBuffer.getOrElse(sFrom.reg.buffer)
      sTo.reg.res := changeRes.getOrElse(sFrom.reg.res)
    }
  }

  // send and save response
  for(i <- 0 until GCaopWorkStages){
    val s = stages(i)
    val mreq = io.aopMReqs(i)
    val (want, addr, isWrite, wmask, wdata) = mmuOpForIndex(i, s.reg)
    when(s.valid && !s.reqDone && want) {
      issueReq(mreq, addr, isWrite, wmask, wdata, s.reqIssued) { rd =>
        s.reqDone := True
        s.responseData := rd
      }
    }
  }

  // stage advancement logic
  // stage0 -> stage1: byte_map from responseData
  advance(0, 1, changeByteMap = Some(stages(0).responseData))
  // stage1 -> stage2: compute card_index/res using responseData and dest
  advance(1, 2, changeCardIndex = Some((stages(1).responseData + (stages(1).reg.dest >> 9) - stages(1).reg.byte_map).resize(MMUDataWidth bits)))
  // stage2: check equality with responseData vs card_index
  when(stages(2).valid && stages(2).reqDone){
    val s2 = stages(2)
    when(s2.responseData === s2.reg.card_index){
      resetStage(2)
    }.otherwise{
      when(!stages(3).valid){
        resetStage(2)
        stages(3).valid := True
        stages(3).reg := s2.reg
      }
    }
  }
  // stage3 -> either stage4 or stage5 depending on responseData(index == 0)
  when(stages(3).valid && stages(3).reqDone) {
    val s3 = stages(3)
    when(s3.responseData === U(0)) {
      // interrupt in stages(5)
      advance(3, 4, changeIndex = Some((s3.responseData / U(8)).resize(MMUDataWidth bits)))
    } .otherwise {
      advance(3, 5, changeIndex = Some((s3.responseData / U(8)).resize(MMUDataWidth bits)))
    }
  }

  // interrupt
  when(stages(4).valid){
    if(DebugEnable){
      report(Seq(
        "[GCAop<", io.DebugTimeStamp,
        ">]issue interrupt call enqueue_failed function",
        "\n"
      ))
    }
  }

  // stage5 -> stage6 copy buffer from responseData
  advance(5, 6, changeBuffer = Some(stages(5).responseData.resize(MMUAddrWidth bits)))

  advance(6, 7)
  advance(7, 8)

  // stage9 is final
  when(stages(8).valid && stages(8).reqDone) {
    resetStage(8)
    if(DebugEnable){
      report(Seq(
        "[GCAop<", io.DebugTimeStamp,
        ">]the task ", stages(8).reg.dest,
        "has done",
        "\n"
      ))
    }
  }
}

object GCAopVerilog extends App{
  Config.spinal.generateVerilog(new GCAop())
}