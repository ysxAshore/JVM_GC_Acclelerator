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
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ToNewGCAlloc.clearOut()
  io.ToAllocFreeRegion.clearIn()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(18)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  def sendToAllocFreeRegion(select: Bool): Unit = {
    io.ToAllocFreeRegion.Valid := True
    io.ToAllocFreeRegion.heapRegionType := region_ptr_type(destAttrIdx)
    io.ToAllocFreeRegion.regionNodeIndex := region_ptr_node_index(destAttrIdx)
    when(io.ToAllocFreeRegion.Valid && io.ToAllocFreeRegion.Ready){
      state := Mux(select, overall_state.states(5), overall_state.states(3))
      again_call_alloc_free := select
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val regionPtr = RegInit(U(0, GCElementWidth bits))
  val destAttrIdx = RegInit(U(0, 1 bits))

  val newAllocRegion = RegInit(U(0, GCElementWidth bits))

  val callGrowIRQ = RegInit(False)

  val array_max = RegInit(U(0, 32 bits))
  val array_len = RegInit(U(0, 32 bits))
  val data_ptr = RegInit(U(0, GCElementWidth bits))
  val remset_ptr = RegInit(U(0, GCElementWidth bits))
  val hrm_index = RegInit(U(0, 32 bits))
  val again_call_alloc_free = RegInit(False)

  val region_ptr_valid = Vec(RegInit(False), 2)
  val region_ptr_type = Vec(RegInit(U(0, 8 bits)), 2)
  val region_ptr_node_index = Vec(RegInit(U(0, 32 bits)), 2)

  val grow_array_ptr_valid = RegInit(False)
  val grow_array_ptr = RegInit(U(0, GCElementWidth bits))

  val region_attr_base_valid = RegInit(False)
  val region_attr_base = RegInit(U(0, GCElementWidth bits))

  val alloc_free_region_done = RegInit(False)
  val alloc_free_region = RegInit(U(0, GCElementWidth bits))
  when(io.ToAllocFreeRegion.Done){
    alloc_free_region_done := True
    alloc_free_region := io.ToAllocFreeRegion.newAllocRegion
  }


  switch(state){
    is(overall_state.states(0)){
      io.ToNewGCAlloc.Ready := True
      when(io.ToNewGCAlloc.Valid && io.ToNewGCAlloc.Ready){
        regionPtr := io.ToNewGCAlloc.regionPtr
        destAttrIdx := io.ToNewGCAlloc.destAttrIdx
        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      when(!region_ptr_valid(destAttrIdx)) {
        val addr = regionPtr + U"x30"
        issueReq(io.Mreq, addr, False, U(17), U(0), issued) { rd =>
          region_ptr_node_index(destAttrIdx) := rd(31 downto 0)
          region_ptr_type(destAttrIdx) := Mux(rd(135 downto 128) === U(1), U(10), U(3)).resized
          region_ptr_valid(destAttrIdx) := True
          state := overall_state.states(2)
        }
      }.otherwise{
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      sendToAllocFreeRegion(False)
    }

    is(overall_state.states(3)){
      when(!grow_array_ptr_valid){
        val addr = io.ConfigIO.G1h + U"x400"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          grow_array_ptr := rd(GCElementWidth - 1 downto 0)
          grow_array_ptr_valid := True
          state := overall_state.states(4)
        }
      }.otherwise{
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      when(!region_attr_base_valid){
        val addr = io.ConfigIO.G1h + U"x590"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          region_attr_base_valid := True
          region_attr_base := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(5)
        }
      }.otherwise{
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      when(io.ToAllocFreeRegion.Done || alloc_free_region_done){
        alloc_free_region_done := False
        newAllocRegion := Mux(alloc_free_region_done, alloc_free_region, io.ToAllocFreeRegion.newAllocRegion)
        state := Mux(again_call_alloc_free, overall_state.states(8), overall_state.states(6))
      }
    }

    is(overall_state.states(6)){
      when(newAllocRegion === U(0)){
        // @todo send interrupt
      }.otherwise{
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(7)){
      // @todo interrupt wake and send to alloc_free_region
      sendToAllocFreeRegion(True)
    }

    is(overall_state.states(8)){
      val addr = newAllocRegion + U"xbc"
      issueReq(io.Mreq, addr, True, U(4), region_ptr_type(destAttrIdx), issued) { _ =>
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      when(region_ptr_type(destAttrIdx) === U(3)){
        issueReq(io.Mreq, grow_array_ptr, False, U(16), U(0), issued) { rd =>
          array_len := rd(31 downto 0)
          array_max := rd(63 downto 32)
          data_ptr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          state := overall_state.states(10)
        }
      }.otherwise{
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(10)){
      when(array_len === array_max){
        // @todo send interrupt
        callGrowIRQ := True
      }.otherwise{
        callGrowIRQ := False
        overall_state.states(11)
      }
    }

    is(overall_state.states(11)){
      // @todo wait interrupt
      when(!callGrowIRQ){
        val writeValue = array_len + U(1)
        issueReq(io.Mreq, grow_array_ptr, True, U(4), writeValue, issued){ _ =>
          state := overall_state.states(12)
        }
      }
    }

    is(overall_state.states(12)){
      val addr = (data_ptr + array_len * U(8)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, True, U(8), newAllocRegion, issued){ _ =>
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(13)){
      val addr = newAllocRegion + U"xb0"
      issueReq(io.Mreq, addr, False, U(12), U(0), issued) { rd =>
        remset_ptr := rd(GCElementWidth - 1 downto 0)
        hrm_index := rd(GCElementWidth + 31 downto GCElementWidth)
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      val temp_value = Mux((region_ptr_type(destAttrIdx) & U(2)) =/= U(0), U(2),
        Mux((region_ptr_type(destAttrIdx) & U(10)) =/= U(0), U(0), U(1)))
      when(temp_value =/= U(1)){
        val addr = remset_ptr + U"xf0"
        issueReq(io.Mreq, addr, True, U(4), temp_value, issued) { _ =>
          state := overall_state.states(15)
        }
      }.otherwise{
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(15)){
      val needs_remset_update = (region_ptr_type(destAttrIdx) & U"x10") === U(0)
      val addr = (region_attr_base + hrm_index * 2).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, True, U(1), needs_remset_update.asUInt, issued) { _ =>
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