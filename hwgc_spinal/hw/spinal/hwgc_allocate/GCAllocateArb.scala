package hwgc_acc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class GCAllocateArb(count: Int) extends Module with GCParameters {
  val io = new Bundle {
    val ToDoAllocates = Vec(slave(new GCToDoAllocate), count)
    val ToDoAllocate = master(new GCToDoAllocate)
  }

  for (i <- 0 until count) {
    io.ToDoAllocates(i).clearOut()
  }
  io.ToDoAllocate.clearIn()

  // 用 1 位 busy 标志替代 4 状态 FSM，减少逻辑层级
  val busy = RegInit(False)
  val rrPtr = RegInit(U(0, log2Up(count) bits))
  val selectedIdx = Reg(UInt(log2Up(count) bits))

  // === 组合逻辑：轮询仲裁（优先编码器） ===
  val reqVec = Vec((0 until count).map { i =>
    val idx = (rrPtr + U(i)).resize(log2Up(count) bits)
    io.ToDoAllocates(idx).Valid
  })
  val hasValid = reqVec.asBits.orR
  val arbIdx = PriorityEncoder(reqVec.asBits) // 优先编码器，取最低位 1
  val currentSelected = (rrPtr + arbIdx).resize(log2Up(count) bits)

  when(!busy) {
    // ===== 空闲态：仲裁 + 转发，同一拍完成 =====
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
        rrPtr := (currentSelected + 1).resize(log2Up(count) bits)
      }
    }
  }.otherwise {
    // ===== 忙态：等待 GCDoAllocate 完成 =====
    when(io.ToDoAllocate.Done) {
      // 结果组合直通给被选中的加速器（不寄存，省 1 拍 + 省 5 个寄存器）
      io.ToDoAllocates(selectedIdx).Done := True
      io.ToDoAllocates(selectedIdx).DestObjPtr := io.ToDoAllocate.DestObjPtr
      io.ToDoAllocates(selectedIdx).ActualPlabSize := io.ToDoAllocate.ActualPlabSize

      // updateCache 信号广播给所有加速器
      for (i <- 0 until count) {
        io.ToDoAllocates(i).updateCacheValid := io.ToDoAllocate.updateCacheValid
        io.ToDoAllocates(i).updateRegionPtr := io.ToDoAllocate.updateRegionPtr
        io.ToDoAllocates(i).updateRegion := io.ToDoAllocate.updateRegion
      }
      busy := False
    }
  }
}