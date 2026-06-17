package hwgc_allocate

import hwgc_top.{GCTopParameters, WrapInc}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCAllocateArb(count: Int) extends Module with GCTopParameters {
  val io = new Bundle {
    val ToDoAllocates = Vec(slave(new GCToDoAllocate), count)
    val ToDoAllocate = master(new GCToDoAllocate)
  }

  for (i <- 0 until count) {
    io.ToDoAllocates(i).Ready := False
    io.ToDoAllocates(i).Done := False
    io.ToDoAllocates(i).DestObjPtr := U(0)
    io.ToDoAllocates(i).ActualPlabSize := U(0)

    io.ToDoAllocates(i).updateCacheValid := io.ToDoAllocate.updateCacheValid
    io.ToDoAllocates(i).updateRegionPtr := io.ToDoAllocate.updateRegionPtr
    io.ToDoAllocates(i).updateRegion := io.ToDoAllocate.updateRegion

  }
  io.ToDoAllocate.clearIn()

  if(count == 1) {
    // ===== count == 1：无需仲裁，直接转发 =====
    val busy = RegInit(False)

    when(!busy) {
      io.ToDoAllocate.Valid := io.ToDoAllocates(0).Valid
      io.ToDoAllocate.regionPtr := io.ToDoAllocates(0).regionPtr
      io.ToDoAllocate.allocRegion := io.ToDoAllocates(0).allocRegion
      io.ToDoAllocate.NodeIndex := io.ToDoAllocates(0).NodeIndex
      io.ToDoAllocate.DestAttrIdx := io.ToDoAllocates(0).DestAttrIdx
      io.ToDoAllocate.MinWordSize := io.ToDoAllocates(0).MinWordSize
      io.ToDoAllocate.AllocatorPtr := io.ToDoAllocates(0).AllocatorPtr
      io.ToDoAllocate.DesiredWordSize := io.ToDoAllocates(0).DesiredWordSize

      io.ToDoAllocates(0).Ready := io.ToDoAllocate.Ready

      when(io.ToDoAllocates(0).Valid && io.ToDoAllocate.Ready) {
        busy := True
      }
    }.otherwise {
      when(io.ToDoAllocate.Done) {
        io.ToDoAllocates(0).Done := True
        io.ToDoAllocates(0).DestObjPtr := io.ToDoAllocate.DestObjPtr
        io.ToDoAllocates(0).ActualPlabSize := io.ToDoAllocate.ActualPlabSize

        busy := False
      }
    }

  } else {
    val selWidth = log2Up(count)

    val busy = RegInit(False)
    val rrPtr = RegInit(U(0, selWidth bits))
    val selectedIdx = Reg(UInt(selWidth bits))

    // === 组合逻辑：轮询仲裁 ===
    val reqVec = Vec(Bool(), count)
    val idxVec = Vec(UInt(selWidth bits), count)

    for(i <- 0 until count) {
      idxVec(i) := WrapInc(rrPtr, count, U(i, selWidth bits))
      reqVec(i) := io.ToDoAllocates(idxVec(i)).Valid
    }

    val hasValid = reqVec.asBits.orR

    val currentSelected = PriorityMux(
      (0 until count).map { i =>
        reqVec(i) -> idxVec(i)
      }
    )

    when(!busy) {
      io.ToDoAllocate.Valid := hasValid
      io.ToDoAllocate.regionPtr := io.ToDoAllocates(currentSelected).regionPtr
      io.ToDoAllocate.allocRegion := io.ToDoAllocates(currentSelected).allocRegion
      io.ToDoAllocate.NodeIndex := io.ToDoAllocates(currentSelected).NodeIndex
      io.ToDoAllocate.DestAttrIdx := io.ToDoAllocates(currentSelected).DestAttrIdx
      io.ToDoAllocate.MinWordSize := io.ToDoAllocates(currentSelected).MinWordSize
      io.ToDoAllocate.AllocatorPtr := io.ToDoAllocates(currentSelected).AllocatorPtr
      io.ToDoAllocate.DesiredWordSize := io.ToDoAllocates(currentSelected).DesiredWordSize

      when(hasValid) {
        io.ToDoAllocates(currentSelected).Ready := io.ToDoAllocate.Ready

        when(io.ToDoAllocate.Ready) {
          busy := True
          selectedIdx := currentSelected
          rrPtr := WrapInc(currentSelected, count, U(1, selWidth bits))
        }
      }
    }.otherwise {
      when(io.ToDoAllocate.Done) {
        io.ToDoAllocates(selectedIdx).Done := True
        io.ToDoAllocates(selectedIdx).DestObjPtr := io.ToDoAllocate.DestObjPtr
        io.ToDoAllocates(selectedIdx).ActualPlabSize := io.ToDoAllocate.ActualPlabSize

        busy := False
      }
    }
  }
}