package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapInc}

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/*
读请求按顺序发出
        ↓
响应乱序回来(第几个 read beat 对应复制流里的第几个字节开始 有多少有效字节 要写到目标地址哪里)
        ↓
ROB 任意 ready slot 都可以消费
        ↓
根据该 slot 的 readIdx 计算它对应的目标地址
        ↓
直接发写请求
*/
class GCCopy extends Component with HWParameters with GCParameters with GCTopParameters {
  val io = new Bundle {
    val ToCopy    = slave(new GCToCopy)
    val readMReq  = master(new LocalMMUIO)
    val writeMReq = master(new LocalMMUIO)
  }

  val BeatBytes    = LineBytesNum
  val BeatBytesU32 = U(BeatBytes, 32 bits)
  val OffBits      = log2Up(BeatBytes)
  val RobPtrBits   = log2Up(GCCopyEntry)
  val BeatLenBits  = log2Up(BeatBytes + 1)

  def alignDown(x: UInt, width: Int): UInt = x & ~U(BeatBytes - 1, width bits)

  // generate write mask for unaligned writes
  //     byteOffset 表示从当前 beat 的第几个 byte 开始写。
  //     byteLen 表示写多少 byte。
  def genMask(byteLen: UInt, byteOffset: UInt): UInt = {
    val ret = UInt(BeatBytes bits)
    for (i <- 0 until BeatBytes) {
      ret(i) := (U(i, BeatLenBits bits) >= byteOffset.resize(BeatLenBits)) &&
        (U(i, BeatLenBits bits) < (byteOffset.resize(BeatLenBits) + byteLen.resize(BeatLenBits)))
    }
    ret
  }

  val task_valid   = RegInit(False)
  val zeroTaskDone = RegInit(False)

  val srcPtr    = RegInit(U(0, GCElementWidth bits))
  val dstPtr    = RegInit(U(0, GCElementWidth bits))
  val totalSize = RegInit(U(0, 32 bits))

  val srcBase = RegInit(U(0, GCElementWidth bits))
  val srcOff  = RegInit(U(0, OffBits bits))

  // read req status
  val readBeatCount = RegInit(U(0, 32 bits)) // 一共需要发多少个 aligned read beat
  val readReqIdx    = RegInit(U(0, 32 bits)) // 已经发出了多少个 read beat

  // inorder consume slot pool
  val slotBusy = Vec.fill(GCCopyEntry)(RegInit(False))
  val slotRespValid = Vec.fill(GCCopyEntry)(RegInit(False))
  val slotReqIdx = Vec.fill(GCCopyEntry)(RegInit(U(0, 32 bits))) // 靠slotReqIdx算这个 beat 对应的逻辑偏移和目标地址
  val slotData = Vec.fill(GCCopyEntry)(RegInit(U(0, MMUDataWidth bits)))

  val sourceId2RobSlot = Reg(Vec(Seq.fill(LLCSourceMaxNum)(U(0, RobPtrBits bits)))) // SourceID -> ROB slot

  // write req status
  // 一个 read beat 对应的有效数据，写到目标地址时可能跨越目标 beat 边界。
  // 因此一个 slot 最多拆成两个写请求：
  //    write0：写当前目标 aligned beat 的尾部
  //    write1：如果跨界，写下一个 aligned beat 的头部
  val writeActive = RegInit(False)
  val writeSecond = RegInit(False)
  val writeSlot = RegInit(U(0, RobPtrBits bits))
  val needWrite1 = RegInit(False)
  val writeAddr0 = RegInit(U(0, MMUAddrWidth bits))
  val writeAddr1 = RegInit(U(0, MMUAddrWidth bits))
  val writeData0 = RegInit(U(0, MMUDataWidth bits))
  val writeData1 = RegInit(U(0, MMUDataWidth bits))
  val writeStrb0 = RegInit(U(0, BeatBytes bits))
  val writeStrb1 = RegInit(U(0, BeatBytes bits))
  val writeSize0 = RegInit(U(0, LineBytesNumBitSize bits))
  val writeSize1 = RegInit(U(0, LineBytesNumBitSize bits))

  // ToCopy 命令接收与 Done
  io.ToCopy.cmd.ready := !task_valid

  val anySlotBusy = slotBusy.asBits.orR // ROB 中是否有任何槽位正在使用

  // 乱序消费版本的完成条件： 1. 所有 read 请求已经发完 2. 所有 slot 都已经释放 3. 写侧没有 pending 的拆分写
  // slotBusy 会覆盖：
  //   - 已发读请求但响应还没回来
  //   - 响应已经回来但还没写
  //   - 正在拆成 write0/write1 写出的 slot
  val taskDoneNow = task_valid && (readReqIdx === readBeatCount) && !anySlotBusy && !writeActive
  io.ToCopy.Done := zeroTaskDone || taskDoneNow

  when(io.ToCopy.cmd.fire) {
    srcPtr := io.ToCopy.cmd.payload.SrcOopPtr
    dstPtr := io.ToCopy.cmd.payload.DestOopPtr
    totalSize := io.ToCopy.cmd.payload.Size

    srcBase := alignDown(io.ToCopy.cmd.payload.SrcOopPtr, GCElementWidth)
    srcOff  := io.ToCopy.cmd.payload.SrcOopPtr(OffBits - 1 downto 0)

    readReqIdx := 0
    readBeatCount := 0

    writeActive := False
    writeSecond := False
    needWrite1 := False

    for (i <- 0 until GCCopyEntry) {
      slotBusy(i)      := False
      slotRespValid(i) := False
      slotReqIdx(i)    := 0
      slotData(i)      := 0
    }

    task_valid := True

    when(io.ToCopy.cmd.payload.Size === 0) {
      zeroTaskDone := True
    } otherwise {
      zeroTaskDone := False
      // 计算读多少个 beat (SrcOff + Size + BeatBytes - 1) / BeatBytes === ceil((SrcOff + Size) / BeatBytes)
      readBeatCount := ((io.ToCopy.cmd.payload.SrcOopPtr(OffBits - 1 downto 0) + io.ToCopy.cmd.payload.Size + U(BeatBytes - 1, 32 bits)) >> OffBits).resize(32)
    }
  } elsewhen zeroTaskDone {
    task_valid := False
    zeroTaskDone := False
  } elsewhen taskDoneNow {
    task_valid := False
  }

  // 空闲 slot 选择
  val freeMask = Bits(GCCopyEntry bits)
  for (i <- 0 until GCCopyEntry) {
    freeMask(i) := !slotBusy(i)
  }
  val hasFreeSlot = freeMask.orR
  val allocOH = OHMasking.first(freeMask)
  val allocSlot = OHToUInt(allocOH).resize(RobPtrBits)

  // 读请求发送
  val readCanAlloc = task_valid && (readReqIdx =/= readBeatCount) && hasFreeSlot // 只要： 1. 当前有任务 2. 还有 read beat 没发 3. 有空闲 slot 就继续发 aligned full-beat read。
  val readReqValid = readCanAlloc
  val curReadAddr = (srcBase + (readReqIdx.resize(GCElementWidth) |<< OffBits)).resize(MMUAddrWidth) // 读请求的地址总是对齐的(按readReqIdx * BeatBytes对齐)

  io.readMReq.Request.valid := readReqValid
  io.readMReq.Request.payload.RequestVirtualAddr := curReadAddr
  io.readMReq.Request.payload.RequestSourceID := io.readMReq.ConherentRequsetSourceID.payload
  io.readMReq.Request.payload.RequestType_isWrite := False
  io.readMReq.Request.payload.RequestWStrb := 0
  io.readMReq.Request.payload.RequestData := 0
  io.readMReq.Request.payload.RequestSize := U(BeatBytes, LineBytesNumBitSize bits)
  io.readMReq.Request.payload.NeedResponse := True
  io.readMReq.Request.payload.NeedDoCmpxChg := False

  val readReqFire = io.readMReq.Request.fire

  when(readReqFire) {
    val sid = io.readMReq.ConherentRequsetSourceID.payload.resized

    sourceId2RobSlot(sid) := allocSlot // soruceID -> rob_index

    slotBusy(allocSlot) := True
    slotReqIdx(allocSlot) := readReqIdx
    slotRespValid(allocSlot) := False

    readReqIdx := readReqIdx + 1
  }

  // 读响应接收 用 ResponseSourceID 查 sourceId2RobSlot，填回对应 slot
  val respSid  = io.readMReq.Response.payload.ResponseSourceID.resized
  val respSlot = sourceId2RobSlot(respSid) // 由Resp ID 查找对应的 ROB slot

  io.readMReq.Response.ready := task_valid && slotBusy(respSlot) && !slotRespValid(respSlot)

  when(io.readMReq.Response.fire) {
    slotData(respSlot) := io.readMReq.Response.payload.ResponseData
    slotRespValid(respSlot) := True
  }

  // 消费 slot 选择
  val consumeMask = Bits(GCCopyEntry bits)
  for (i <- 0 until GCCopyEntry) {
    consumeMask(i) := slotBusy(i) && slotRespValid(i)
  }
  val hasConsumeSlot = consumeMask.orR
  val consumeOH = OHMasking.first(consumeMask)
  val consumeSlot = OHToUInt(consumeOH).resize(RobPtrBits)

  // 将 consume slot 转换成写请求
  //
  // 当写侧没有 pending 任务时，选择一个已经返回的 slot。
  // 根据 its slotReqIdx 计算：
  //   1. 这个 read beat 对应 copy 流中的逻辑偏移
  //   2. 这个 beat 有多少有效 byte
  //   3. 目标地址在哪里
  //   4. 是否需要拆成两个 masked write
  when(task_valid && hasConsumeSlot && !writeActive){
    val idx = slotReqIdx(consumeSlot)
    val data = slotData(consumeSlot)

    val logicalOffset = (idx.resize(32) << OffBits).resize(32) // 当前 read beat 相对 srcBase 的 byte 起点
    val logicalStart32 = Mux(idx === 0, U(0, 32 bits), logicalOffset - srcOff.resize(32)) // 当前 beat 对应 copy 逻辑流中的起点 (dstAddr + logicalStart32)

    val validStartInBeat = Mux(idx === 0, srcOff.resize(32), U(0, 32 bits)) // 当前 beat 的有效数据在 beat 内的起点
    val maxValidBytesThisBeat32 = BeatBytesU32 - validStartInBeat // 当前 beat 理论上最多能贡献多少有效 byte
    val remainBytes32 = totalSize - logicalStart32 // copy 任务还剩多少 byte 没覆盖
    val validBytes32 = Mux(remainBytes32 <= maxValidBytesThisBeat32, remainBytes32, maxValidBytesThisBeat32) // 当前 beat 实际贡献多少有效 byte

    // 把第一拍的无效前缀丢掉 之后 shiftedData 的低 byte 就是当前 slot 的第一个有效 byte
    val shiftedData = data |>> (validStartInBeat << 3)

    val dstByteAddr = dstPtr + logicalStart32.resize(GCElementWidth)
    val dstAddrAligned = alignDown(dstByteAddr, GCElementWidth).resize(MMUAddrWidth) // 目标地址向下对齐到 beat
    val dstOff = dstByteAddr(OffBits - 1 downto 0) // 目标地址在 beat 内的偏移
    val space0 = BeatBytesU32 - dstOff.resize(32) // 当前目标 beat 还能写多少 byte
    val len0 = Mux(validBytes32 <= space0, validBytes32, space0)// 第一个写请求写多少 byte
    val len1 = validBytes32 - len0 // 如果跨越目标 beat 边界，第二个写请求写剩余 byte

    writeSlot := consumeSlot
    writeAddr0 := dstAddrAligned // write0：写当前 aligned beat
    writeData0 := (shiftedData |<< (dstOff << 3)).resized
    writeStrb0 := genMask(len0.resize(BeatLenBits), dstOff)
    writeSize0 := len0.resize(LineBytesNumBitSize)

    // write1：如果跨界，写下一个 aligned beat
    writeAddr1 := (dstAddrAligned + U(BeatBytes, MMUAddrWidth bits)).resized
    writeData1 := (shiftedData |>> (len0.resize(BeatLenBits) << 3)).resized
    writeStrb1 := genMask(len1.resize(BeatLenBits), U(0, OffBits bits))
    writeSize1 := len1.resize(LineBytesNumBitSize)

    needWrite1 := len1 =/= 0
    writeActive := True
    writeSecond := False

    // 这个 slot 已经被写侧拿走，防止下一拍重复选择 slotBusy 仍然保持 True，直到 write0/write1 全部完成。
    slotRespValid(consumeSlot) := False
  }

  io.writeMReq.Request.valid := writeActive
  io.writeMReq.Request.payload.RequestVirtualAddr := Mux(writeSecond, writeAddr1, writeAddr0)
  io.writeMReq.Request.payload.RequestSourceID := io.writeMReq.ConherentRequsetSourceID.payload
  io.writeMReq.Request.payload.RequestType_isWrite := True
  io.writeMReq.Request.payload.RequestWStrb := Mux(writeSecond, writeStrb1, writeStrb0)
  io.writeMReq.Request.payload.RequestData := Mux(writeSecond, writeData1, writeData0)
  io.writeMReq.Request.payload.RequestSize := Mux(writeSecond, writeSize1, writeSize0)
  io.writeMReq.Request.payload.NeedResponse := False
  io.writeMReq.Request.payload.NeedDoCmpxChg := False

  val writeFire = io.writeMReq.Request.fire

  when(writeFire) {
    when(!writeSecond && needWrite1) {
      writeSecond := True
    } otherwise {
      // 当前 slot 的所有写请求都完成，释放 slot
      writeActive := False
      writeSecond := False
      needWrite1 := False
      slotBusy(writeSlot) := False
    }
  }

  io.writeMReq.Response.ready := True

  val counter = RegInit(U(0, 64 bits))
  when(task_valid){
    counter := counter + 1
  }
}

object GCCopy2Verilog extends App {
  Config.spinal.generateVerilog(new GCCopy())
}