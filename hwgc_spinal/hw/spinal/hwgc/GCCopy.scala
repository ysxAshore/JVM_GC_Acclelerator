package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCCopy extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val fromProcess = slave(new GCToCopy)
    val readMReq = master(new LocalMMUIO)
    val writeMReq = master(new LocalMMUIO)
  }

  val BeatSize = U(MMUDataWidth / GCElementWidth)

  val task_valid = RegInit(False)
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))

  val totalSize = RegInit(U(0, 32 bits))
  val readRemainSize = RegInit(U(0, 32 bits))
  val writeReqRemainSize = RegInit(U(0, 32 bits))
  val writeResRemainSize = RegInit(U(0, 32 bits))

  val head = RegInit(U(0, log2Up(GCCopyEntry) bits))
  val tail = RegInit(U(0, log2Up(GCCopyEntry) bits))
  val queue_empty = head === tail
  val queue_full = WrapInc(tail, GCCopyEntry) === head // 牺牲一个slot
  // Seq(...) 是在 生成硬件时（即编译/综合阶段）确定的静态结构
  // 不能用一个 UInt 信号（例如 idx: UInt）直接去索引 Seq, 因为 Seq.apply(idx) 要求 idx 是编译时常量（Int）
  // 需要使用Vec来支持动态索引
  val copyDataQueue = Vec.fill(GCCopyEntry)(RegInit(U(0, MMUDataWidth bits)))

  io.fromProcess.Ready := !task_valid
  io.fromProcess.Done := task_valid && writeResRemainSize === U(0) && queue_empty

  when(io.fromProcess.Valid && io.fromProcess.Ready){
    task_valid := True
    srcOopPtr := io.fromProcess.SrcOopPtr
    destOopPtr := io.fromProcess.DestOopPtr

    totalSize := io.fromProcess.Size

    readRemainSize := io.fromProcess.Size
    writeReqRemainSize := io.fromProcess.Size
    writeResRemainSize := io.fromProcess.Size

    head := U(0)
    tail := U(0)
  }.elsewhen(io.fromProcess.Done){
    task_valid := False
  }


  val currentReadOffset = totalSize - readRemainSize
  io.readMReq.Request.valid := task_valid && readRemainSize > 0 && !queue_full
  io.readMReq.Request.payload.RequestVirtualAddr := (srcOopPtr + currentReadOffset * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
  io.readMReq.Request.payload.RequestSourceID := io.readMReq.ConherentRequsetSourceID.payload
  io.readMReq.Request.payload.RequestType_isWrite := False
  io.readMReq.Request.payload.RequestWStrb := U(0)
  io.readMReq.Request.payload.RequestData := U(0)
  io.readMReq.Response.ready := !queue_full // 只有队列有空间才能接收MMU Response

  when(io.readMReq.Request.fire){
    readRemainSize := Mux(readRemainSize >= BeatSize, readRemainSize - BeatSize, U(0))
  }

  when(io.readMReq.Response.fire){
    copyDataQueue(tail) := io.readMReq.Response.payload.ResponseData
    tail := WrapInc(tail, GCCopyEntry)
  }

  val currentWriteOffset = totalSize - writeReqRemainSize
  val isLastWriteBeat = writeReqRemainSize <= BeatSize
  val thisBeatWriteLen = Mux(isLastWriteBeat, writeReqRemainSize, BeatSize) * U(8)

  io.writeMReq.Request.valid := task_valid && (writeReqRemainSize > U(0)) && !queue_empty
  io.writeMReq.Request.payload.RequestVirtualAddr := (destOopPtr + currentWriteOffset * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
  io.writeMReq.Request.payload.RequestSourceID := io.writeMReq.ConherentRequsetSourceID.payload
  io.writeMReq.Request.payload.RequestType_isWrite := True
  io.writeMReq.Request.payload.RequestWStrb := getWstrb(thisBeatWriteLen)
  io.writeMReq.Request.payload.RequestData := copyDataQueue(head)
  io.writeMReq.Response.ready := True

  // write always success
  when(io.writeMReq.Request.fire){
    writeReqRemainSize := Mux(writeReqRemainSize >= BeatSize, writeReqRemainSize - BeatSize, U(0))
    head := WrapInc(head, GCCopyEntry)
  }

  when(io.writeMReq.Response.fire){
    writeResRemainSize := Mux(writeResRemainSize >= BeatSize, writeResRemainSize - BeatSize, U(0))
  }
}

object GCCopyVerilog extends App{
  Config.spinal.generateVerilog(new GCCopy())
}