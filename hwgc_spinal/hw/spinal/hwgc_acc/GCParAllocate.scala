package hwgc_acc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
class GCParAllocate extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToParAllocate = slave(new GCToParAllocate)
    val ToDoAllocate = master(new GCToDoAllocate)
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

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val nodeIndex =  RegInit(U(0, 8 bits))
  val destAttrIdx = RegInit(U(0, 1 bits))
  val minWordSize = RegInit(U(0, GCElementWidth bits))
  val allocatorPtr = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val region_ptr_valid = Vec(RegInit(False), 2)
  val region_ptr_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val alloc_region_valid = Vec(RegInit(False), 2)
  val alloc_region_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  when(io.ToDoAllocate.updateCacheValid){
    when(region_ptr_valid(0) && io.ToDoAllocate.updateRegionPtr === region_ptr_cache(0) && alloc_region_valid(0)){
      alloc_region_cache(0) := io.ToDoAllocate.updateRegion
    }.elsewhen(region_ptr_valid(1) && io.ToDoAllocate.updateRegionPtr === region_ptr_cache(1) && alloc_region_valid(1)){
      alloc_region_cache(1) := io.ToDoAllocate.updateRegion
    }
  }

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
      io.ToDoAllocate.allocRegion := alloc_region_cache(destAttrIdx)
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