package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
class GCParAllocate extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToParAllocate = slave(new GCToParAllocate)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ToParAllocate.clearOut()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(11)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCParAllocate<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def resetState(): Unit = {
    io.ToParAllocate.Done := True
    io.ToParAllocate.DestObjPtr := destObjPtr
    io.ToParAllocate.ActualPlabSize := actualPlabSize
    state := overall_state.states(0)
    dbg(Seq("The task in par_allocate module has done"))
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val excuteAll = RegInit(False)
  val botUpdates = RegInit(False)
  val allocRegion = RegInit(U(0, GCElementWidth bits))
  val minWordSize = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val actualPlabSize = RegInit(U(0, GCElementWidth bits))
  val destObjPtr = RegInit(U(0, GCElementWidth bits))

  val alloc_top = RegInit(U(0, GCElementWidth bits))
  val alloc_end = RegInit(U(0, GCElementWidth bits))
  val blk_start = RegInit(U(0, GCElementWidth bits))
  val blk_end = RegInit(U(0, GCElementWidth bits))
  val bot_part_ptr = RegInit(U(0, GCElementWidth bits))
  val next_offset_threshold = RegInit(U(0, GCElementWidth bits))
  val index = RegInit(U(0, GCElementWidth bits))
  val bot_ptr = RegInit(U(0, GCElementWidth bits))
  val array = RegInit(U(0, GCElementWidth bits))
  val reserved_start = RegInit(U(0, GCElementWidth bits))
  val start_card = RegInit(U(0, GCElementWidth bits))
  val end_card = RegInit(U(0, GCElementWidth bits))
  val start_card_for_region = RegInit(U(0, GCElementWidth bits))
  val reach = RegInit(U(0, GCElementWidth bits))
  val num_cards = RegInit(U(0, GCElementWidth bits))
  val begin = RegInit(U(0, GCElementWidth bits))
  val ct_offset = RegInit(U(0, 8 bits))
  val iterator = RegInit(U(0, 8 bits))
  val offset0 = RegInit(U(0, GCElementWidth bits))
  val offset8 = RegInit(U(0, GCElementWidth bits))

  switch(state){
    is(overall_state.states(0)){
      io.ToParAllocate.Ready := True
      when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
        excuteAll := io.ToParAllocate.excuteAll
        botUpdates := io.ToParAllocate.botUpdates
        allocRegion := io.ToParAllocate.allocRegion
        minWordSize := io.ToParAllocate.minWordSize
        desiredWordSize := io.ToParAllocate.desiredWordSize

        state := overall_state.states(1)
        dbg(Seq("The task enter the par_allocate module, excuteAll = ", excuteAll, "botUpdates = ", botUpdates, "allocRegion = ", allocRegion, "minWordSize = ", minWordSize, "desiredWordsize = ", desiredWordSize ))
      }
    }

    is(overall_state.states(1)){
      val addr = allocRegion + U"x8"
      issueReq(io.Mreq, addr, False, U(16), U(0), issued) { rd =>
        alloc_end := rd(GCElementWidth - 1 downto 0)
        alloc_top := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      val available = (alloc_end - alloc_top) / U(8)
      val want_to_allocate = Mux(available > desiredWordSize, desiredWordSize, available)
      when(want_to_allocate >= minWordSize){
        actualPlabSize := want_to_allocate
        destObjPtr := alloc_top
        val addr = allocRegion + U"x10"
        val writeValue = (alloc_top + want_to_allocate * U(8)).resize(GCElementWidth)
        issueReq(io.Mreq, addr, True, U(8), writeValue, issued) { _ =>
          state := overall_state.states(3)
        }
      }.otherwise{
        destObjPtr := U(0)
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      when(!excuteAll){
        resetState()
      }.otherwise{
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      when(destObjPtr =/= U(0) && botUpdates){
        blk_start := destObjPtr
        blk_end := (destObjPtr + actualPlabSize * U(8)).resize(GCElementWidth)
        val addr = allocRegion + U"x20"
        issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
          bot_part_ptr := addr
          next_offset_threshold := rd(GCElementWidth - 1 downto 0)
          index := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          bot_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          state := overall_state.states(5)
        }
      }.otherwise{
        resetState()
      }
    }

    is(overall_state.states(5)){
      when(blk_end > next_offset_threshold){
        issueReq(io.Mreq, bot_ptr, False, U(24), U(0), issued) { rd =>
          reserved_start := rd(GCElementWidth - 1 downto 0)
          array := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          state := overall_state.states(6)
        }
      }.otherwise{
        resetState()
      }
    }

    is(overall_state.states(6)){
      val addr = array + index
      val writeValue = ((next_offset_threshold - blk_start) / U(8)).resize(8)
      issueReq(io.Mreq, addr, True, U(1), writeValue, issued) { _ =>
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      val end_index = ((blk_end - U(8) - reserved_start) >> U(9)).resize(GCElementWidth)
      val rem_st = (reserved_start + ((index + U(1)) << U(6)) * U(8)).resize(GCElementWidth)
      val rem_end = (reserved_start + ((end_index << U(6)) + U(64)) * U(8)).resize(GCElementWidth)
      start_card := ((rem_st - reserved_start) >> U(9)).resize(GCElementWidth)
      end_card := ((rem_end - U(8) - reserved_start) >> U(9)).resize(GCElementWidth)
      offset0 := rem_end
      offset8 := end_index + U(1)
      when(index + U(1) <= end_index && rem_st < rem_end && start_card <= end_card){
        start_card_for_region := start_card
        ct_offset := U"xff"
        iterator := U(0)
        state := overall_state.states(8)
      }.otherwise{
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(8)){
      when(iterator < U(14)){
        reach := (start_card - U(1) + ((U(1) << (U(4) * (iterator + U(1)))) - U(1))).resize(GCElementWidth)
        ct_offset := U(64, 8 bits) + iterator
        num_cards := Mux(reach >= end_card, end_card, reach) - start_card_for_region + U(1)
        begin := array + start_card_for_region
        iterator := iterator + U(1)
        state := overall_state.states(9)
      }.otherwise{
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(9)){
      when(num_cards > 0){
        val addr = begin
        val busBytes = U(MMUDataWidth / 8)
        val writeBytes = Mux(num_cards >= busBytes, busBytes, num_cards).resize(8 bits)
        val writeValue = Vec.fill(32)(ct_offset).asBits.asUInt
        issueReq(io.Mreq, addr, True, writeBytes, writeValue, issued) { _ =>
          begin := begin + writeBytes
          num_cards := num_cards - writeBytes
          state := overall_state.states(9)
        }
      }.otherwise{
        start_card_for_region := reach + U(1)
        when(reach >= end_card){
          state := overall_state.states(10)
        }.otherwise{
          state := overall_state.states(8)
        }
      }
    }

    is(overall_state.states(10)){
      val writeValue = Cat(offset8, offset0).asUInt
      issueReq(io.Mreq, bot_part_ptr, True, U(16), writeValue, issued) { _ =>
        resetState()
      }
    }
  }
}

object GCParAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCParAllocate())
}