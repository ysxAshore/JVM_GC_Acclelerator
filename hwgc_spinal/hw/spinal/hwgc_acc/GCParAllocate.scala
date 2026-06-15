package hwgc_acc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
class GCParAllocate extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToParAllocate = slave(new GCToParAllocate)
    val ToDoAllocate = master(new GCToDoAllocate)
    val ConfigIO = slave(new GCParAllocateConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ToParAllocate.clearOut()
  io.ToDoAllocate.clearIn()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(10)(_ => newElement())
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

  val nodeIndex =  RegInit(U(0, 8 bits))
  val destAttrIdx = RegInit(U(0, 1 bits))
  val minWordSize = RegInit(U(0, GCElementWidth bits))
  val allocatorPtr = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val actualPlabSize = RegInit(U(0, GCElementWidth bits))
  val destObjPtr = RegInit(U(0, GCElementWidth bits))

  val region_ptr_off10 = RegInit(U(0, GCElementWidth bits))
  val region_ptr_off18 = RegInit(U(0, GCElementWidth bits))
  val region_ptr_off20 = RegInit(False)
  val bot_updates = RegInit(False)
  val write_lock0 = RegInit(False)
  val blk_start = RegInit(U(0, GCElementWidth bits))
  val blk_end = RegInit(U(0, GCElementWidth bits))
  val end_index = RegInit(U(0, GCElementWidth bits))
  val begin = RegInit(U(0, GCElementWidth bits))
  val remaining = RegInit(U(0, GCElementWidth bits))
  val iterator = RegInit(U(0, 4 bits))
  val start_ptr = RegInit(U(0, GCElementWidth bits))

  val region_ptr_valid = Vec(RegInit(False), 2)
  val region_ptr_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val alloc_region_valid = Vec(RegInit(False), 2)
  val alloc_region_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  val alloc_about_valid = Vec(RegInit(False), 2)
  val alloc_top_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val alloc_end_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val alloc_bottom_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  val par_allocate_valid = RegInit(False)
  val bot_part_ptr = RegInit(U(0, GCElementWidth bits))
  val next_offset_threshold = RegInit(U(0, GCElementWidth bits))
  val index = RegInit(U(0, GCElementWidth bits))
  val bot_ptr = RegInit(U(0, GCElementWidth bits))

  val bot_ptr_about_valid = RegInit(False)
  val array = RegInit(U(0, GCElementWidth bits))
  val reserved_start = RegInit(U(0, GCElementWidth bits))

  val cm_valid = RegInit(False)
  val cm = RegInit(U(0, GCElementWidth bits))
  val root_regions_array_valid = RegInit(False)
  val root_regions_array = RegInit(U(0, GCElementWidth bits))
  val idx = RegInit(U(0, GCElementWidth bits))

  switch(state){
    is(overall_state.states(0)){
      io.ToParAllocate.Ready := True
      when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
        nodeIndex := io.ToParAllocate.NodeIndex
        minWordSize := io.ToParAllocate.MinWordSize
        destAttrIdx := io.ToParAllocate.DestAttrIdx
        allocatorPtr := io.ToParAllocate.AllocatorPtr
        desiredWordSize := io.ToParAllocate.DesiredWordSize
        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      when(!region_ptr_valid(destAttrIdx)){
        when(destAttrIdx === 0){
          val addr = allocatorPtr + U"x28"
          issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
            region_ptr_valid(destAttrIdx) := True
            region_ptr_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
            state := overall_state.states(2)
          }
        }.otherwise{
          region_ptr_valid(destAttrIdx) := True
          region_ptr_cache(destAttrIdx) := allocatorPtr + U"x30"
          state := overall_state.states(2)
        }
      }.otherwise{
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      when(!alloc_region_valid(destAttrIdx)){
        val addr = region_ptr_cache(destAttrIdx) + U"x8"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          alloc_region_valid(destAttrIdx) := True
          alloc_region_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(3)
        }
      }.otherwise{
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      io.ToDoAllocate.Valid := True
      io.ToDoAllocate.regionPtr := region_ptr_cache(destAttrIdx)
      io.ToDoAllocate.allocRegion := alloc_region_valid(destAttrIdx)
      io.ToDoAllocate.NodeIndex := nodeIndex
      io.ToDoAllocate.DestAttrIdx := destAttrIdx
      io.ToDoAllocate.MinWordSize := minWordSize
      io.ToDoAllocate.AllocatorPtr := allocatorPtr
      io.ToDoAllocate.DesiredWordSize := desiredWordSize

      when(io.ToDoAllocate.Valid && io.ToDoAllocate.Ready){
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      when(io.ToDoAllocate.Done){
        when(io.ToDoAllocate.updateCacheValid){
          when(region_ptr_valid(0) && io.ToDoAllocate.updateRegionPtr === region_ptr_cache(0) && alloc_region_valid(0)){
            alloc_region_cache(0) := io.ToDoAllocate.updateRegion
          }.elsewhen(region_ptr_valid(1) && io.ToDoAllocate.updateRegionPtr === region_ptr_cache(1) && alloc_region_valid(1)){
            alloc_region_cache(1) := io.ToDoAllocate.updateRegion
          }
        }
        io.ToParAllocate.Done := True
        io.ToParAllocate.DestObjPtr := io.ToDoAllocate.DestObjPtr
        io.ToParAllocate.ActualPlabSize := io.ToDoAllocate.ActualPlabSize
        state := overall_state.states(0)
      }
    }
  }
}

object GCParAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCParAllocate())
}