package hwgc_acc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCToParAllocateArbiter(accCount: Int)
  extends Module
    with GCParameters
    with HWParameters {

  require(accCount >= 1)

  private val selWidth = log2Up(accCount max 2)

  val io = new Bundle {
    // 来自多个 GCAllocate / 加速器
    val FromAcc = Vec(slave(new GCToParAllocate), accCount)

    // 发给真正的 ParAllocate
    val ToParAllocate = master(new GCToParAllocate)
  }

  // ---------------------------------------------------------------------------
  // Default assignments
  // ---------------------------------------------------------------------------

  io.ToParAllocate.Valid := False
  io.ToParAllocate.NodeIndex := U(0, io.ToParAllocate.NodeIndex.getWidth bits)
  io.ToParAllocate.MinWordSize := U(0, io.ToParAllocate.MinWordSize.getWidth bits)
  io.ToParAllocate.DesiredWordSize := U(0, io.ToParAllocate.DesiredWordSize.getWidth bits)
  io.ToParAllocate.DestAttrIdx := U(0, io.ToParAllocate.DestAttrIdx.getWidth bits)
  io.ToParAllocate.AllocatorPtr := U(0, io.ToParAllocate.AllocatorPtr.getWidth bits)

  for (i <- 0 until accCount) {
    io.FromAcc(i).Ready := False
    io.FromAcc(i).Done := False
    io.FromAcc(i).DestObjPtr := U(0, io.FromAcc(i).DestObjPtr.getWidth bits)
    io.FromAcc(i).ActualPlabSize := U(0, io.FromAcc(i).ActualPlabSize.getWidth bits)
  }

  // ---------------------------------------------------------------------------
  // State
  // ---------------------------------------------------------------------------

  val busy  = RegInit(False)
  val owner = Reg(UInt(selWidth bits)) init 0

  // Round-robin pointer.
  // 下一次仲裁从 rrPtr 开始找 Valid。
  val rrPtr = Reg(UInt(selWidth bits)) init 0

  // ---------------------------------------------------------------------------
  // Request arbitration
  // ---------------------------------------------------------------------------

  val reqBits = Bits(accCount bits)
  for (i <- 0 until accCount) {
    reqBits(i) := io.FromAcc(i).Valid
  }

  val hasReq = reqBits.orR

  // Rotate reqBits by rrPtr, then select first valid.
  // Example:
  //   rrPtr = 2
  //   original order: 0,1,2,3
  //   arb order:      2,3,0,1
  val doubledReq = reqBits ## reqBits
  val rotatedReq = (doubledReq >> rrPtr).resize(accCount)

  val grantOhRot = OHMasking.first(rotatedReq)
  val grantRotIdx = OHToUInt(grantOhRot).resize(selWidth)

  val grantSum = rrPtr.resize(selWidth + 1) + grantRotIdx.resize(selWidth + 1)

  val grantIdx = UInt(selWidth bits)
  grantIdx := Mux(
    grantSum >= U(accCount, selWidth + 1 bits),
    (grantSum - U(accCount, selWidth + 1 bits)).resize(selWidth),
    grantSum.resize(selWidth)
  )

  val nextRrPtr = UInt(selWidth bits)
  nextRrPtr := (grantIdx + 1).resize(selWidth)
  when(grantIdx === U(accCount - 1, selWidth bits)) {
    nextRrPtr := U(0, selWidth bits)
  }

  // ---------------------------------------------------------------------------
  // Idle: accept one request and send it to real ParAllocate
  // ---------------------------------------------------------------------------

  when(!busy) {
    io.ToParAllocate.Valid := hasReq

    io.ToParAllocate.NodeIndex := io.FromAcc(grantIdx).NodeIndex
    io.ToParAllocate.MinWordSize := io.FromAcc(grantIdx).MinWordSize
    io.ToParAllocate.DesiredWordSize := io.FromAcc(grantIdx).DesiredWordSize
    io.ToParAllocate.DestAttrIdx := io.FromAcc(grantIdx).DestAttrIdx
    io.ToParAllocate.AllocatorPtr := io.FromAcc(grantIdx).AllocatorPtr

    for (i <- 0 until accCount) {
      io.FromAcc(i).Ready :=
        hasReq &&
          io.ToParAllocate.Ready &&
          grantIdx === U(i, selWidth bits)
    }

    when(hasReq && io.ToParAllocate.Ready) {
      busy := True
      owner := grantIdx
      rrPtr := nextRrPtr
    }
  }

  // ---------------------------------------------------------------------------
  // Busy: wait for real ParAllocate result, then route it back to owner
  // ---------------------------------------------------------------------------

  when(busy && io.ToParAllocate.Done) {
    busy := False

    for (i <- 0 until accCount) {
      when(owner === U(i, selWidth bits)) {
        io.FromAcc(i).Done := True
        io.FromAcc(i).DestObjPtr := io.ToParAllocate.DestObjPtr
        io.FromAcc(i).ActualPlabSize := io.ToParAllocate.ActualPlabSize
      }
    }
  }
}