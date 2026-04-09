package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCAllocFreeRegion extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCAllocFreeRegionConfigIO)
    val ToAllocFreeRegion = slave(new GCToAllocFreeRegion)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ToAllocFreeRegion.clearOut()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(8)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCAllocFreeRegion<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def resetState(): Unit = {
    io.ToAllocFreeRegion.Done := True
    io.ToAllocFreeRegion.newAllocRegion := newAllocRegion
    state := overall_state.states(0)
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val heapRegionType = RegInit(U(0, 8 bits))
  val regionNodeIndex = RegInit(U(0, 32 bits))

  val newAllocRegion = RegInit(U(0, GCElementWidth bits))

  val from_head = RegInit(False)
  val res_conf = RegInit(U(0, GCElementWidth bits))
  val free_list_ptr = RegInit(U(0, GCElementWidth bits))

  val list_about_valid = RegInit(False)
  val list_head_ptr = RegInit(U(0, GCElementWidth bits))
  val list_tail_ptr = RegInit(U(0, GCElementWidth bits))
  val list_last_ptr = RegInit(U(0, GCElementWidth bits))
  val list_length = RegInit(U(0, 32 bits))

  switch(state){
    is(overall_state.states(0)){
      io.ToAllocFreeRegion.Ready := True
      when(io.ToAllocFreeRegion.Valid && io.ToAllocFreeRegion.Ready){
        heapRegionType := io.ToAllocFreeRegion.heapRegionType
        regionNodeIndex := io.ToAllocFreeRegion.regionNodeIndex

        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      newAllocRegion := U(0)
      free_list_ptr := io.ConfigIO.G1h + U"x130" + U"xb0"
      from_head := (heapRegionType & U(2, 8 bits)) === U(0)

      when(!list_about_valid){
        val addr = free_list_ptr + U"x10"
        issueReq(io.Mreq, addr, False, U(4), U(0), issued) { rd =>
          list_length := rd(31 downto 0)
          state := overall_state.states(2)
        }
      }.otherwise{
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(2)){
      val addr = free_list_ptr + U"x28"
      issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
        list_head_ptr := rd(GCElementWidth - 1 downto 0)
        list_tail_ptr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        list_last_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)

        list_about_valid := True
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      when(list_length === 0){
        newAllocRegion := U(0)
        list_about_valid := False

        val addr = free_list_ptr + U"x10"
        issueReq(io.Mreq, addr, True, U(4), U(0), issued) { _ =>
          state := overall_state.states(4)
        }
      }.otherwise{
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(4)){
      val addr = free_list_ptr + U"x28"
      issueReq(io.Mreq, addr, True, U(24), U(0), issued) { _ =>
      }
      when(issued){
        issued := False
        resetState()
      }
    }

    is(overall_state.states(5)){
      val res = Mux(from_head, list_head_ptr, list_tail_ptr)
      val addr = res + Mux(from_head, U"xd0", U"xd8")
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        res_conf := rd(GCElementWidth - 1 downto 0)

        state := overall_state.states(6)
        newAllocRegion := res
      }
    }

    is(overall_state.states(6)){
      when(from_head){
        list_head_ptr := res_conf
      }.otherwise{
        list_tail_ptr := res_conf
      }

      when(res_conf === 0){
        when(from_head){
          list_tail_ptr := 0
        }.otherwise{
          list_head_ptr := 0
        }
        state := overall_state.states(7)
      }.otherwise{
        val addr = res_conf + Mux(from_head, U"xd8", U"xd0")
        issueReq(io.Mreq, addr, True, U(8), U(0), issued) { _ =>
        }
        when(issued){
          issued := False
          state := overall_state.states(7)
        }
      }
    }

    is(overall_state.states(7)){
      val addr = newAllocRegion + Mux(from_head, U"xd0", U"xd8")
      issueReq(io.Mreq, addr, True, U(8), U(0), issued) { _ =>
      }
      when(issued){
        issued := False
        when(list_last_ptr === newAllocRegion){
          list_last_ptr := 0
        }
        list_length := list_length - 1
        resetState()
      }
    }
  }
}

object GCAllocFreeRegionVerilog extends App{
  Config.spinal.generateVerilog(new GCAllocFreeRegion())
}