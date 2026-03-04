package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCNewGCAlloc extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToNewGCAlloc = slave(new GCToNewGCAlloc)
    val ToAllocFreeRegion = master(new GCToAllocFreeRegion)
    val ConfigIO = slave(new GCNewGCAllocConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ToNewGCAlloc.clearOut()
  io.ToAllocFreeRegion.clearIn()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(18)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val regionPtr = RegInit(U(0, GCElementWidth bits))

  val newAllocRegion = RegInit(U(0, GCElementWidth bits))

  val callGrowIRQ = RegInit(False)

  val heap_region_type = RegInit(U(0, 8 bits))
  val region_node_index = RegInit(U(0, 32 bits))
  val grow_array_ptr = RegInit(U(0, GCElementWidth bits))
  val policy_ptr = RegInit(U(0, GCElementWidth bits))
  val expand_failure = RegInit(U(0, 8 bits))
  val array_max = RegInit(U(0, 32 bits))
  val array_len = RegInit(U(0, 32 bits))
  val data_ptr = RegInit(U(0, GCElementWidth bits))
  val count_per_node = RegInit(U(0, GCElementWidth bits))

  switch(state){
    is(overall_state.states(0)){
      io.ToNewGCAlloc.Ready := True
      when(io.ToNewGCAlloc.Valid && io.ToNewGCAlloc.Ready){
        regionPtr := io.ToNewGCAlloc.regionPtr
        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      val addr = regionPtr + U"x30"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        region_node_index := rd(31 downto 0)
        heap_region_type := Mux(rd(135 downto 128) === U(1), U(10), U(3)).resized
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      val addr = io.ConfigIO.G1h + U"x400"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        grow_array_ptr := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      val addr = io.ConfigIO.G1h + U"x430"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        policy_ptr := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      io.ToAllocFreeRegion.Valid := True
      io.ToAllocFreeRegion.heapRegionType := heap_region_type
      io.ToAllocFreeRegion.regionNodeIndex := region_node_index
      when(io.ToAllocFreeRegion.Valid && io.ToAllocFreeRegion.Ready){
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      when(io.ToAllocFreeRegion.Done){
        newAllocRegion := io.ToAllocFreeRegion.newAllocRegion
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(6)){
      val addr = io.ConfigIO.G1h + U"x370"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        expand_failure := rd(7 downto 0)
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      when(newAllocRegion === U(0) && expand_failure  =/= U(0)){
        // @todo send interrupt
      }.otherwise{
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(8)){
      // @todo interrupt wake
    }

    is(overall_state.states(9)){
      when(newAllocRegion =/= U(0)){
        val addr = newAllocRegion + U"xbc"
        issueReq(io.Mreq, addr, True, getWstrb(4), heap_region_type, issued) { _ =>
          state := overall_state.states(10)
        }
      }.otherwise{
        io.ToNewGCAlloc.Done := True
        io.ToNewGCAlloc.newAllocRegion := newAllocRegion
        state := overall_state.states(0)
      }
    }

    is(overall_state.states(10)){
      when(heap_region_type === U(3)){
        issueReq(io.Mreq, grow_array_ptr, False, U(0), U(0), issued) { rd =>
          array_len := rd(31 downto 0)
          array_max := rd(63 downto 32)
          data_ptr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          state := overall_state.states(11)
        }
      }.otherwise{
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(11)){
      when(array_len === array_max){
        // @todo send interrupt
        callGrowIRQ := True
      }.otherwise{
        overall_state.states(12)
      }
    }

    is(overall_state.states(12)){
      // @todo wait interrupt
      when(!callGrowIRQ){
        val writeValue = array_len + U(1)
        issueReq(io.Mreq, grow_array_ptr, True, getWstrb(4), writeValue, issued){ _ =>
          state := overall_state.states(13)
        }
      }
    }

    is(overall_state.states(13)){
      val addr = (data_ptr + array_len * U(8)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, True, getWstrb(8), newAllocRegion, issued){ _ =>
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      val addr = newAllocRegion + U"xb0"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        count_per_node := rd(GCElementWidth - 1 downto 0)
        array_max := rd(GCElementWidth + 31 downto GCElementWidth)
        array_len := rd(GCElementWidth + 63 downto GCElementWidth + 32)
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(15)){
      val temp_value = Mux((array_len & U(2)) =/= U(0), U(2),
        Mux((array_len & U(10)) =/= U(0), U(0), U(1)))
      when(temp_value =/= U(1)){
        val addr = count_per_node + U"xf0"
        issueReq(io.Mreq, addr, True, getWstrb(4), temp_value, issued) { _ =>
          state := overall_state.states(16)
        }
      }.otherwise{
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      val addr = io.ConfigIO.G1h + U"x590"
      issueReq(io.Mreq, addr, True, getWstrb(8), count_per_node, issued) { _ =>
        state := overall_state.states(17)
      }
    }

    is(overall_state.states(17)){
      val needs_remset_update = (array_len & U(10)) === U(0)
      val addr = (count_per_node + array_max * U(2)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, True, getWstrb(1), needs_remset_update.asUInt, issued) { _ =>
        io.ToNewGCAlloc.Done := True
        io.ToNewGCAlloc.newAllocRegion := newAllocRegion
        state := overall_state.states(0)
      }
    }
  }
}

object GCNewGCAllocVerilog extends App{
  Config.spinal.generateVerilog(new GCNewGCAlloc())
}