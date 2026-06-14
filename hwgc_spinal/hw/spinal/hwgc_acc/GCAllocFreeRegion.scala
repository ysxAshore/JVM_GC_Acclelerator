package hwgc_acc

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

  def resetState(res: UInt): Unit = {
    io.ToAllocFreeRegion.Done := True
    io.ToAllocFreeRegion.newAllocRegion := res
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
        free_list_ptr := io.ConfigIO.G1h + U"x130" + U"xb0"

        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      newAllocRegion := U(0)
      from_head := (heapRegionType & U(2, 8 bits)) === U(0)

      val addr = free_list_ptr + U"x10"
      val isAligned = addr(4 downto 0) === 0
      val readSize = Mux(isAligned, U(32), U(16))
      issueReq(io.Mreq, addr, False, readSize, U(0), issued) { rd =>
        list_length := rd(31 downto 0)
        when(isAligned){
          list_head_ptr := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
        }
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      val addr = free_list_ptr + U"x10"
      val isAligned = addr(4 downto 0) === 0
      val addrAligned = Mux(isAligned, addr + U"x20", addr + U"x10")
      val readSize = Mux(isAligned, U(16), U(32))
      issueReq(io.Mreq, addrAligned, False, readSize, U(0), issued) { rd =>
        when(isAligned){
          list_tail_ptr := rd(GCElementWidth - 1 downto 0)
          list_last_ptr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        }.otherwise{
          list_head_ptr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          list_tail_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          list_last_ptr := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
        }
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      when(list_length === 0){
        resetState(0)
      }.otherwise{
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      val res = Mux(from_head, list_head_ptr, list_tail_ptr)
      val addr = res + Mux(from_head, U"xd0", U"xd8")
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        res_conf := rd(GCElementWidth - 1 downto 0)

        state := overall_state.states(6)
        newAllocRegion := res
      }
    }

    is(overall_state.states(5)){
      val writeSize =  Mux(res_conf === U(0) && newAllocRegion =/= 0 && newAllocRegion === list_last_ptr, U(16), U(8))
      val writeAddr = Mux(res_conf === U(0), free_list_ptr + U"x28", free_list_ptr + Mux(from_head, U"x28", U"x30"))
      val writeData = Mux(res_conf === U(0),
        Mux(from_head, Cat(U(0, GCElementWidth bits), res_conf).asUInt, Cat(res_conf, U(0, GCElementWidth)).asUInt),
        res_conf.resize(GCElementWidth * 2)
      )
      issueReq(io.Mreq, writeAddr, True, writeSize, writeData, issued) { _ => }
      when(issued){
        issued := False
        when(res_conf =/= 0) {state := overall_state.states(6)}
        .otherwise {state := overall_state.states(7)}
      }
    }

    is(overall_state.states(6)){
      val addr = res_conf + Mux(from_head, U"xd8", U"xd0")
      issueReq(io.Mreq, addr, True, U(8), U(0), issued) { _ => }
      when(issued){
        issued := False
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      val addr = newAllocRegion + Mux(from_head, U"xd0", U"xd8")
      issueReq(io.Mreq, addr, True, U(8), U(0), issued) { _ => }
      when(issued){
        issued := False
        when(newAllocRegion =/= 0 && list_last_ptr === newAllocRegion){
          state := overall_state.states(8)
        }.elsewhen(newAllocRegion =/= 0){
          list_length := list_length - 1
          state := overall_state.states(9)
        }.otherwise{
          resetState(0)
        }
      }
    }

    is(overall_state.states(8)){
      val addr = res_conf + Mux(from_head, U"xd8", U"xd0")
      issueReq(io.Mreq, addr, True, U(8), U(0), issued) { _ => }
      when(issued){
        issued := False
        state := overall_state.states(7)
      }
    }

  }
}

object GCAllocFreeRegionVerilog extends App{
  Config.spinal.generateVerilog(new GCAllocFreeRegion())
}