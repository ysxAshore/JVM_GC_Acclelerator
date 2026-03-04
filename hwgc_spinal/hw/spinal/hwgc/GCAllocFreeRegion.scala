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
  io.Mreq.Response.ready := False

  io.ToAllocFreeRegion.clearOut()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(19)(_ => newElement())
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

  val page_size = RegInit(U(0, 32 bits))
  val region_size = RegInit(U(0, 32 bits))
  val active_node_ids = RegInit(U(0, 32 bits))
  val from_head = RegInit(False)
  val cur_depth = RegInit(U(0, 32 bits))
  val max_depth = RegInit(U(0, 32 bits))
  val cur = RegInit(U(0, GCElementWidth bits))
  val prev = RegInit(U(0, GCElementWidth bits))
  val next = RegInit(U(0, GCElementWidth bits))
  val temp_value = RegInit(U(0, GCElementWidth bits))
  val free_list_ptr = RegInit(U(0, GCElementWidth bits))

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
      val addr = io.ConfigIO.NumaPtr + U"x18"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        active_node_ids := rd(31 downto 0)
        region_size := rd(31 + GCElementWidth downto GCElementWidth)
        page_size := rd(31 + GCElementWidth * 2 downto GCElementWidth * 2)
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      newAllocRegion := U(0)
      free_list_ptr := io.ConfigIO.G1h + U"x130" + U"xb0"
      from_head := (heapRegionType & U(2, 8 bits)) === U(0)
      when(regionNodeIndex =/= UINT_MAX - U(1) && active_node_ids > U(1)){
        state := overall_state.states(3)
      }.otherwise{
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(3)){
      cur_depth := U(0)
      max_depth := (U(3) * Max(page_size / region_size, U(1)) * active_node_ids).resize(32)
      val addr = free_list_ptr + Mux(from_head, U"x28", U"x30")
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        cur := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      when(cur =/= U(0) && cur_depth < max_depth){
        val addr = cur + U"x120"
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          temp_value := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(5)
        }
      }.otherwise{
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(5)){
      when(regionNodeIndex === temp_value(31 downto 0)){
        state := overall_state.states(6)
      }.otherwise{
        cur_depth := cur_depth + U(1)
        val addr = cur + Mux(from_head, U"xd0", U"xd8")
        issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
          cur := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(4)
        }
      }
    }

    is(overall_state.states(6)){
      when(cur === U(0) || cur_depth >= max_depth){
        newAllocRegion := U(0)
        state := overall_state.states(10)
      }.otherwise{
        newAllocRegion := cur
        val addr = cur + U"xd0"
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          next := rd(GCElementWidth - 1 downto 0)
          prev := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          state := overall_state.states(7)
        }
      }
    }

    is(overall_state.states(7)){
      val addr = Mux(prev === U(0), free_list_ptr + U"x28", prev + U"xd0")
      issueReq(io.Mreq, addr, True, getWstrb(8), next, issued) { _ =>
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(8)){
      val addr = Mux(next === U(0), free_list_ptr + U"x30", prev + U"xd8")
      issueReq(io.Mreq, addr, True, getWstrb(8), prev, issued) { _ =>
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      val addr = newAllocRegion + U"xd0"
      issueReq(io.Mreq, addr, True, getWstrb(16), U(0), issued) { _ =>
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      val addr = free_list_ptr + U"x10"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        active_node_ids := rd(31 downto 0)
        state := overall_state.states(11)
      }
    }

    is(overall_state.states(11)){
      when(newAllocRegion === U(0)){
        when(active_node_ids === U(0)){
          newAllocRegion := U(0)
          state := overall_state.states(16)
        }.otherwise{
          val addr = free_list_ptr + Mux(from_head, U"x28", U"x30")
          issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
            newAllocRegion := rd(GCElementWidth - 1 downto 0)
            state := overall_state.states(12)
          }
        }
      }.otherwise{
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(12)){
       val addr = newAllocRegion + Mux(from_head, U"xd0", U"xd8")
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        temp_value := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(13)){
      val addr = free_list_ptr + Mux(from_head, U"x28", U"x30")
      issueReq(io.Mreq, addr, True, getWstrb(8), temp_value, issued) { _ =>
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      val addr_head = Mux(temp_value === U(0), free_list_ptr + U"x30", temp_value + U"xd8")
      val addr_tail = Mux(temp_value === U(0), free_list_ptr + U"x28", temp_value + U"xd0")
      val addr = Mux(from_head, addr_head, addr_tail)
      issueReq(io.Mreq, addr, True, getWstrb(8), U(0), issued) { _ =>
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(15)){
      val addr = newAllocRegion + Mux(from_head, U"xd0", U"xd8")
      issueReq(io.Mreq, addr, True, getWstrb(8), U(0), issued) { _ =>
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      when(newAllocRegion === U(0)){
        resetState()
      }.otherwise{
        val addr = free_list_ptr + U"x38"
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          temp_value := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(17)
        }
      }
    }

    is(overall_state.states(17)){
      when(temp_value === newAllocRegion){
        val addr = free_list_ptr + U"x38"
        issueReq(io.Mreq, addr, True, getWstrb(8), U(0), issued) { _ =>
          state := overall_state.states(18)
        }
      }.otherwise{
        state := overall_state.states(18)
      }
    }

    is(overall_state.states(18)){
      val addr = free_list_ptr + U"x10"
      val wrireValue = active_node_ids - U(1)
      issueReq(io.Mreq, addr, True, getWstrb(4), wrireValue, issued) { _ =>
        resetState()
      }
    }
  }
}

object GCAllocFreeRegionVerilog extends App{
  Config.spinal.generateVerilog(new GCAllocFreeRegion())
}