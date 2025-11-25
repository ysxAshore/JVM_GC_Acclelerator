package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCCopy extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val fromProcess = slave(new GCProcess2Copy)
    val readMReq = master(new LocalMMUIO)
    val writeMReq = master(new LocalMMUIO)
  }

  // default value
  io.readMReq.Response.ready := False
  io.writeMReq.Response.ready := False

  val task_valid = RegInit(False)
  val srcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val destOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val readSize = RegInit(U(0, MMUDataWidth bits))
  val writeSize = RegInit(U(0, MMUDataWidth bits))

  val head = RegInit(U(0, log2Up(GCCopyEntry) bits))
  val tail = RegInit(U(0, log2Up(GCCopyEntry) bits))
  val queue_empty = head === tail
  val queue_full = ((tail + U(1)) & U(GCCopyEntry - 1)) === head // 牺牲一个slot
  // Seq(...) 是在 生成硬件时（即编译/综合阶段）确定的静态结构
  // 不能用一个 UInt 信号（例如 idx: UInt）直接去索引 Seq, 因为 Seq.apply(idx) 要求 idx 是编译时常量（Int）
  // 需要使用Vec来支持动态索引
  val copyDataQueue = Vec.fill(GCCopyEntry)(new Bundle {
    val data  = RegInit(U(0, MMUDataWidth bits))
  })

  io.fromProcess.Ready := !task_valid
  io.fromProcess.Done := task_valid && readSize === U(0) && writeSize === U(0) && queue_empty

  when(io.fromProcess.Valid && io.fromProcess.Ready){
    task_valid := True
    srcOopPtr := io.fromProcess.SrcOopPtr
    destOopPtr := io.fromProcess.DestOopPtr
    readSize := io.fromProcess.Size - U(1)
    writeSize := io.fromProcess.Size - U(1)
    head := U(0)
    tail := U(0)
  }.elsewhen(io.fromProcess.Done){
    task_valid := False
  }

  io.readMReq.Request.valid := task_valid && readSize =/= U(0) && !queue_full
  io.readMReq.Request.payload.RequestVirtualAddr := (srcOopPtr + readSize * GCObjectPtr_Size).resized
  io.readMReq.Request.payload.RequestSourceID := io.readMReq.ConherentRequsetSourceID.payload
  io.readMReq.Request.payload.RequestType_isWrite := False
  io.readMReq.Request.payload.RequestWStrb := U(0)
  io.readMReq.Request.payload.RequestData := U(0)

  io.writeMReq.Request.valid := task_valid && writeSize =/= U(0) && !queue_empty
  io.writeMReq.Request.payload.RequestVirtualAddr := (destOopPtr + writeSize * GCObjectPtr_Size).resized
  io.writeMReq.Request.payload.RequestSourceID := io.writeMReq.ConherentRequsetSourceID.payload
  io.writeMReq.Request.payload.RequestType_isWrite := True
  io.writeMReq.Request.payload.RequestWStrb := allBytesOnes
  io.writeMReq.Request.payload.RequestData := copyDataQueue(head).data

  when(io.readMReq.Request.fire){
    readSize := readSize - U(1)
  }
  when(io.readMReq.Response.fire){
    copyDataQueue(tail).data := io.readMReq.Response.ResponseData
    tail := WrapInc(tail, GCCopyEntry)
  }
  when(io.writeMReq.Response.fire){
    writeSize := writeSize - U(1)
    head := WrapInc(head, GCCopyEntry)
  }
}

object GCCopyVerilog extends App{
  Config.spinal.generateVerilog(new GCCopy())
}