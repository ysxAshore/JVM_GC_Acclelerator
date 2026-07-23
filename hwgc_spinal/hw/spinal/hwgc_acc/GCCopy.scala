package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}
import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class GCCopyForwardPort() extends Bundle with IMasterSlave with HWParameters with GCTopParameters with GCParameters {
  val valid = Bool()
  val addr = UInt(MMUAddrWidth bits)
  val size = UInt(LineBytesNumBitSize bits)
  
  // epoch 和预解码字段用于减少 Fetch READ_REQ 路径上的地址计算
  // 当 epoch 不匹配时，GCCopy 会忽略预解码结果，并根据当前 Copy 状态重新计算
  val epoch = UInt(GCCopyEpochWidth bits)
  val predecodedValid = Bool()
  val firstBeatIdx = UInt(32 bits)
  val firstByteOffset = UInt(log2Up(LineBytesNum) bits)

  val stall = Bool() // stall = true：查询范围内存在尚未从源内存返回的数据，Fetch 必须等待
  val mask = Bits(LineBytesNum bits) //  mask 某位为 1：对应请求字节可以直接从 Copy 内部转发
  val data = UInt(MMUDataWidth bits)

  override def asMaster(): Unit = {
    out(valid, addr, size, epoch, predecodedValid, firstBeatIdx, firstByteOffset)
    in(stall, mask, data)
  }
}

class GCCopy extends Component with HWParameters with GCParameters with GCTopParameters {
  val io = new Bundle {
    val ToCopy = slave(new GCToCopy)

    val readMReq = master(new LocalMMUIO)
    val writeMReq = master(new LocalMMUIO)

    val FwdMain = slave(GCCopyForwardPort())
    val FwdPush = slave(GCCopyForwardPort())
    val FwdPre = slave(GCCopyForwardPort())

    val State = out(GCCopyPublicState())
  }

  val BeatBytes = LineBytesNum
  val BeatBytesU32 = U(BeatBytes, 32 bits)
  val OffBits = log2Up(BeatBytes)
  val RobPtrBits = log2Up(GCCopyEntry)
  val BeatLenBits = log2Up(BeatBytes + 1)

  val CompletionEntries = GCCopyCompletionEntry
  val CompletionPtrBits = log2Up(CompletionEntries)

  val WcbEntries = GCCopyWriteCombineEntry
  val WcbPtrBits = log2Up(WcbEntries)

  // 当前实现通过截取低位完成取模，因此相关表项数量必须是 2 的幂
  require((GCCopyEntry & (GCCopyEntry - 1)) == 0)
  require((CompletionEntries & (CompletionEntries - 1)) == 0)
  require((WcbEntries & (WcbEntries - 1)) == 0)
  require(CompletionEntries >= GCCopyEntry) // CompletionEntries >= GCCopyEntry 的约束确保了完成窗口不会成为瓶颈：最多只会有 GCCopyEntry 个 beat 同时在飞行，而完成窗口至少那么大，所以永远不会因为窗口满而阻塞

  // 将地址向下对齐到一个 MMU beat
  def alignDown(x: UInt, width: Int): UInt = x & ~U(BeatBytes - 1, width bits)

  /**
   * 根据字节起始偏移和长度生成一个 beat 内的字节掩码
   * byteOffset 表示第一个有效字节所在位置
   * byteLen 表示从该位置开始的有效字节数量
   */
  def genMask(byteLen: UInt, byteOffset: UInt): UInt = {
    val result = UInt(BeatBytes bits)

    for (i <- 0 until BeatBytes) {
      result(i) := U(i, BeatLenBits bits) >= byteOffset.resize(BeatLenBits) && 
                   U(i, BeatLenBits bits) < byteOffset.resize(BeatLenBits) + byteLen.resize(BeatLenBits)
    }

    result
  }

  // 将每字节 1 bit 的写掩码扩展为每字节 8 bit 的数据位掩码
  def expandByteMask(mask: UInt): UInt = {
    val result = UInt(MMUDataWidth bits)
    for (i <- 0 until BeatBytes) {
      result(i * 8 + 7 downto i * 8) := Mux(mask(i), U"8'xFF", U"8'x00")
    }
    result
  }

  val taskValid = RegInit(False) // 当前是否存在活跃 Copy 事务
  val zeroTaskDone = RegInit(False) // 零长度任务使用单独脉冲完成

  val copyEpoch = RegInit(U(0, GCCopyEpochWidth bits))

  // 原始 Copy 参数
  val srcPtr = RegInit(U(0, GCElementWidth bits))
  val dstPtr = RegInit(U(0, GCElementWidth bits))
  val totalSize = RegInit(U(0, 32 bits))

  // 源地址按 beat 对齐后的基地址，以及首 beat 内的源字节偏移。
  val srcBase = RegInit(U(0, GCElementWidth bits))
  val srcOff = RegInit(U(0, OffBits bits))

  // 源 beat 总数和下一个准备发出的读取 beat 编号
  val readBeatCount = RegInit(U(0, 32 bits)) // ceil((srcOff + size) / BeatBytes)
  val readReqIdx = RegInit(U(0, 32 bits))

  // 连续完成前缀状态
  // ROB slot 可以乱序释放，但 committedBytes 只能按 beat 顺序向前推进
  val nextCommitIdx = RegInit(U(0, 32 bits))
  val committedBytes = RegInit(U(0, 32 bits))

  // 向 Fetch 暴露当前 Copy 状态
  io.State.Active := taskValid
  io.State.Epoch := copyEpoch
  io.State.SrcPtr := srcPtr
  io.State.DstPtr := dstPtr
  io.State.SrcBase := srcBase
  io.State.TotalSize := totalSize
  io.State.CommittedBytes := committedBytes

  val slotBusy = Vec.fill(GCCopyEntry)(RegInit(False))
  val slotForwardValid = Vec.fill(GCCopyEntry)(RegInit(False)) // 对应源 beat 数据是否已经返回，可以用于 Fetch forwarding
  val slotReqIdx = Vec.fill(GCCopyEntry)(RegInit(U(0, 32 bits))) // slot 中保存的完整逻辑 beat 编号，用于防止环形索引别名
  val slotData = Vec.fill(GCCopyEntry)(RegInit(U(0, MMUDataWidth bits)))

  // 一个源 beat 最多跨越两个目标 cache line，因此保存两个独立片段
  val frag0Pending = Vec.fill(GCCopyEntry)(RegInit(False))
  val frag0Addr = Vec.fill(GCCopyEntry)(RegInit(U(0, MMUAddrWidth bits)))
  val frag0Data = Vec.fill(GCCopyEntry)(RegInit(U(0, MMUDataWidth bits)))
  val frag0Mask = Vec.fill(GCCopyEntry)(RegInit(U(0, BeatBytes bits)))

  val frag1Pending = Vec.fill(GCCopyEntry)(RegInit(False))
  val frag1Addr = Vec.fill(GCCopyEntry)(RegInit(U(0, MMUAddrWidth bits)))
  val frag1Data = Vec.fill(GCCopyEntry)(RegInit(U(0, MMUDataWidth bits)))
  val frag1Mask = Vec.fill(GCCopyEntry)(RegInit(U(0, BeatBytes bits)))

  // 每个源 beat 已进入 WCB、但目标写响应尚未返回的 line 数量 2 bit 因为一个bit最多跨越两个cache line
  // 只有当该计数器归零、且两个 fragment 都不再 pending 时，该 beat 才算真正"完成"
  val slotPendingLineAck = Vec.fill(GCCopyEntry)(RegInit(U(0, 2 bits)))

  // MMU 读取响应 SourceID 到 ROB slot 的映射
  val sourceId2RobSlot = Reg(Vec(Seq.fill(LLCSourceMaxNum)(U(0, RobPtrBits bits))))

  // 每个 ROB slot 的组合地址映射
  /*
  设 idx = slotReqIdx(i)                        // 该 slot 的逻辑 beat 编号
     logicalOffset = idx * BeatBytes           // 该 beat 在源地址空间中的起始偏移
     slotLogicalStart = logicalOffset - srcOff // 减去首 beat 的源内偏移，得到逻辑复制偏移
                                               // 特殊：idx=0 时直接为 0
     slotValidStart = idx==0 ? srcOff : 0      // 仅首 beat 有内部偏移，其余 beat 从头开始
  */
  val slotLogicalStart = Vec(UInt(32 bits), GCCopyEntry) // 该 beat 第一个有效源字节对应的逻辑复制偏移
  val slotValidStart = Vec(UInt(32 bits), GCCopyEntry) // 该 beat 中第一个有效源字节在 beat 内的偏移
  val slotValidBytes = Vec(UInt(32 bits), GCCopyEntry) // 该 beat 实际参与复制的有效字节数量
  val slotDstStart = Vec(UInt(GCElementWidth bits), GCCopyEntry) // 该 beat 第一个有效字节对应的目标地址

  for (i <- 0 until GCCopyEntry) {
    val idx = slotReqIdx(i)
    val logicalOffset = (idx.resize(32) << OffBits).resize(32)

    slotLogicalStart(i) := Mux(idx === 0, U(0, 32 bits), logicalOffset - srcOff.resize(32))
    slotValidStart(i) := Mux(idx === 0, srcOff.resize(32), U(0, 32 bits))

    val maxValid = BeatBytesU32 - slotValidStart(i)
    val remain = Mux(slotLogicalStart(i) < totalSize, totalSize - slotLogicalStart(i), U(0, 32 bits))

    slotValidBytes(i) := Mux(remain <= maxValid, remain, maxValid)
    slotDstStart(i) := (dstPtr + slotLogicalStart(i).resize(GCElementWidth)).resize(GCElementWidth)
  }

  // 有界乱序完成窗口
  // ROB 中的 beat 可以乱序完成目标写入（beat 5 可能比 beat 3 先写回），但 committedBytes 必须按顺序推进——beat 0 完成之前，不能说"已提交 64 字节"
  // 因此需要一个中间结构来记录"已完成但尚未提交"的 beat
  val completionValid = Vec.fill(CompletionEntries)(RegInit(False)) // completionValid 表示对应槽位包含有效完成信息
  val completionTag = Vec.fill(CompletionEntries)(RegInit(U(0, 32 bits))) // completionTag 保存完整 beat 编号，防止低位索引环绕后误命中

  // 获取逻辑 beat 在完成窗口中的环形索引
  def completionIndex(beatIdx: UInt): UInt = beatIdx(CompletionPtrBits - 1 downto 0)

  // 判断某个逻辑 beat 是否已经完成目标内存写入
  def completionHit(beatIdx: UInt): Bool = {
    val index = completionIndex(beatIdx)
    completionValid(index) && completionTag(index) === beatIdx
  }

  /* 
  具体数值例子（BeatBytes = 64, srcOff = 12）：
    源地址: 0x100C (对齐后 srcBase = 0x1000, srcOff = 12)
    目标地址: 0x2000 (对齐后)

    源 Beat 0: [0x1000 - 0x103F]
      有效部分: 字节 12~63（52 字节）
      目标起始偏移 = 0 (目标地址刚好对齐)
      → frag0: 目标 line 0x2000, 偏移 0~51（52 字节）
      → 没有 frag1（52 ≤ 64）

    源 Beat 1: [0x1040 - 0x107F]
      有效部分: 字节 0~63（完整 64 字节）
      目标地址 = 0x2000 + 52 = 0x2034
      目标 line 起始 = 0x2000（对齐），偏移 = 0x34 = 52

      → frag0: 目标 line 0x2000, 偏移 52~63（12 字节）
           ↑ 和 Beat 0 的 frag0 是同一个目标 line！
      → frag1: 目标 line 0x2040, 偏移 0~51（52 字节）
  */
  // 源 Beat 0 和 Beat 1 都需要写入目标 line 0x2000！如果直接各写各的，第二次写入会覆盖第一次的结果（或者需要读-改-写）
  // WCB先把两个片段的数据和掩码合并到同一个缓冲区，等所有字节都齐了，再一次性写入
  /* 
  WCB 的生命周期分三个阶段：
      阶段 1: 空闲    wcbValid=false, wcbIssued=false
      阶段 2: 收集中  wcbValid=true,  wcbIssued=false  ← 可以继续接收片段
      阶段 3: 已发出  wcbValid=true,  wcbIssued=true   ← 等待写响应，不能接收新片段 Issued就是用来控制防止在写请求发出后又有新片段被错误的合并
  */
  val wcbValid = Vec.fill(WcbEntries)(RegInit(False))
  val wcbIssued = Vec.fill(WcbEntries)(RegInit(False))
  val wcbAddr = Vec.fill(WcbEntries)(RegInit(U(0, MMUAddrWidth bits)))
  val wcbData = Vec.fill(WcbEntries)(RegInit(U(0, MMUDataWidth bits)))
  val wcbMask = Vec.fill(WcbEntries)(RegInit(U(0, BeatBytes bits)))

  // 当源 beat 和目标 line 宽度相同时，一个目标 line 最多由两个源 beat 贡献
  // 追踪的目的：当写响应返回时（确认目标 line 已经写入内存），需要知道是哪些源 beat 的哪些片段在这个 WCB 中，以便正确递减 slotPendingLineAck 计数器
  val wcbContrib0Valid = Vec.fill(WcbEntries)(RegInit(False))
  val wcbContrib0Slot = Vec.fill(WcbEntries)(RegInit(U(0, RobPtrBits bits)))
  val wcbContrib0Tag = Vec.fill(WcbEntries)(RegInit(U(0, 32 bits)))

  val wcbContrib1Valid = Vec.fill(WcbEntries)(RegInit(False))
  val wcbContrib1Slot = Vec.fill(WcbEntries)(RegInit(U(0, RobPtrBits bits)))
  val wcbContrib1Tag = Vec.fill(WcbEntries)(RegInit(U(0, 32 bits)))

  // MMU 写响应 SourceID 到 WCB slot 的映射
  val sourceId2WcbSlot = Reg(Vec(Seq.fill(LLCSourceMaxNum)(U(0, WcbPtrBits bits))))

  /**
   * 计算目标 line 中属于当前 Copy 范围的全部字节
   * WCB 收集到该掩码覆盖的全部字节后，才允许发出目标写请求
   */
  def expectedDestinationMask(lineAddr: UInt): UInt = {
    val result = UInt(BeatBytes bits)
    val copyEnd = (dstPtr + totalSize.resize(GCElementWidth)).resize(GCElementWidth)

    for (i <- 0 until BeatBytes) {
      val byteAddr = (lineAddr.resize(GCElementWidth) + U(i, GCElementWidth bits)).resize(GCElementWidth)
      result(i) := taskValid && byteAddr >= dstPtr && byteAddr < copyEnd
    }

    result
  }

  // 命令接收和任务完成
  // 同一时间只允许一个 Copy 事务
  io.ToCopy.cmd.ready := !taskValid

  val anySlotBusy = slotBusy.asBits.orR
  val anyWcbValid = wcbValid.asBits.orR

  // 所有源读取均已发出、连续完成前缀到达结尾，并且 ROB/WCB 均为空时任务完成
  val taskDoneNow = taskValid && readReqIdx === readBeatCount && nextCommitIdx === readBeatCount &&
    !anySlotBusy && !anyWcbValid

  io.ToCopy.Done := zeroTaskDone || taskDoneNow

  when(io.ToCopy.cmd.fire) {
    srcPtr := io.ToCopy.cmd.payload.SrcOopPtr
    dstPtr := io.ToCopy.cmd.payload.DestOopPtr
    totalSize := io.ToCopy.cmd.payload.Size

    srcBase := alignDown(io.ToCopy.cmd.payload.SrcOopPtr, GCElementWidth)
    srcOff := io.ToCopy.cmd.payload.SrcOopPtr(OffBits - 1 downto 0)

    // 初始化读取和提交指针。
    readReqIdx := 0
    readBeatCount := 0
    nextCommitIdx := 0
    committedBytes := 0

    // 新任务使用新的 epoch，使旧任务的 Fetch 预解码信息自动失效
    copyEpoch := copyEpoch + 1

    // 清空 ROB
    for (i <- 0 until GCCopyEntry) {
      slotBusy(i) := False
      slotForwardValid(i) := False
      slotReqIdx(i) := 0
      slotData(i) := 0
      frag0Pending(i) := False
      frag1Pending(i) := False
      slotPendingLineAck(i) := 0
    }

    // 清空完成窗口
    for (i <- 0 until CompletionEntries) {
      completionValid(i) := False
      completionTag(i) := 0
    }

    // 清空 WCB
    for (i <- 0 until WcbEntries) {
      wcbValid(i) := False
      wcbIssued(i) := False
      wcbAddr(i) := 0
      wcbData(i) := 0
      wcbMask(i) := 0
      wcbContrib0Valid(i) := False
      wcbContrib1Valid(i) := False
    }

    taskValid := True

    when(io.ToCopy.cmd.payload.Size === 0) {
      // 零长度任务不产生任何 MMU 请求，下一周期直接完成
      zeroTaskDone := True
    }.otherwise {
      zeroTaskDone := False

      readBeatCount := ((io.ToCopy.cmd.payload.SrcOopPtr(OffBits - 1 downto 0) +
        io.ToCopy.cmd.payload.Size + U(BeatBytes - 1, 32 bits)) >> OffBits).resize(32)
    }
  }.elsewhen(zeroTaskDone) {
    taskValid := False
    zeroTaskDone := False
  }.elsewhen(taskDoneNow) {
    taskValid := False
  }

  // 源读取请求发射
  // beat index 的低位直接选择 ROB slot
  val readSlot = readReqIdx(RobPtrBits - 1 downto 0)

  // 限制读请求不能超出完成窗口容量，防止 completion table 发生模索引覆盖
  val completionDistance = (readReqIdx - nextCommitIdx).resize(32)
  val completionHasCredit = completionDistance < U(CompletionEntries, 32 bits)

  val readCanAllocate = taskValid && readReqIdx =/= readBeatCount &&
    !slotBusy(readSlot) && completionHasCredit

  val currentReadAddr = (srcBase + (readReqIdx.resize(GCElementWidth) |<< OffBits)).resize(MMUAddrWidth)

  io.readMReq.Request.valid := readCanAllocate
  io.readMReq.Request.payload.RequestVirtualAddr := currentReadAddr
  io.readMReq.Request.payload.RequestSourceID := io.readMReq.ConherentRequsetSourceID.payload
  io.readMReq.Request.payload.RequestType_isWrite := False
  io.readMReq.Request.payload.RequestWStrb := 0
  io.readMReq.Request.payload.RequestData := 0
  io.readMReq.Request.payload.RequestSize := U(BeatBytes, LineBytesNumBitSize bits)
  io.readMReq.Request.payload.NeedResponse := True
  io.readMReq.Request.payload.NeedDoCmpxChg := False

  when(io.readMReq.Request.fire) {
    val sourceId = io.readMReq.ConherentRequsetSourceID.payload.resized

    // 记录响应 SourceID 对应的 ROB slot
    sourceId2RobSlot(sourceId) := readSlot

    // 初始化新分配的 ROB slot
    slotBusy(readSlot) := True
    slotForwardValid(readSlot) := False
    slotReqIdx(readSlot) := readReqIdx
    slotPendingLineAck(readSlot) := U(0, 2 bits)
    frag0Pending(readSlot) := False
    frag1Pending(readSlot) := False

    readReqIdx := readReqIdx + 1
  }

  // 源读取响应和目标片段生成
  val readResponseSourceId = io.readMReq.Response.payload.ResponseSourceID.resized
  val readResponseSlot = sourceId2RobSlot(readResponseSourceId)

  // 只接受当前任务中已经分配且尚未接收数据的 slot
  io.readMReq.Response.ready := taskValid && slotBusy(readResponseSlot) && !slotForwardValid(readResponseSlot)

  val readResponseFire = io.readMReq.Response.fire
  val responseData = io.readMReq.Response.payload.ResponseData

  // 去掉首尾 beat 中不属于 Copy 范围的源字节
  val responseValidStart = slotValidStart(readResponseSlot)
  val responseValidBytes = slotValidBytes(readResponseSlot)
  val responseShiftedData = responseData |>> (responseValidStart << 3)

  // 计算有效数据在目标地址空间中的起点
  val responseDestinationByteAddr = dstPtr + slotLogicalStart(readResponseSlot).resize(GCElementWidth)
  val responseDestinationLine0 = alignDown(responseDestinationByteAddr, GCElementWidth).resize(MMUAddrWidth)
  val responseDestinationOffset0 = responseDestinationByteAddr(OffBits - 1 downto 0)

  // 第一个目标 line 可容纳的数据量
  val responseSpace0 = BeatBytesU32 - responseDestinationOffset0.resize(32)
  val responseLength0 = Mux(responseValidBytes <= responseSpace0, responseValidBytes, responseSpace0)

  // 超出第一个目标 line 的部分进入第二个片段
  val responseLength1 = responseValidBytes - responseLength0

  val generatedFragment0Data = (responseShiftedData |<< (responseDestinationOffset0 << 3)).resize(MMUDataWidth)
  val generatedFragment0Mask = genMask(responseLength0.resize(BeatLenBits), responseDestinationOffset0)
  val generatedFragment1Data = (responseShiftedData |>> (responseLength0.resize(BeatLenBits) << 3)).resize(MMUDataWidth)
  val generatedFragment1Mask = genMask(responseLength1.resize(BeatLenBits), U(0, OffBits bits))

  when(readResponseFire) {
    // 保存原始源 beat，供 Fetch forwarding 使用。
    slotData(readResponseSlot) := responseData
    slotForwardValid(readResponseSlot) := True

    // 生成第一个目标 line 片段
    frag0Addr(readResponseSlot) := responseDestinationLine0
    frag0Data(readResponseSlot) := generatedFragment0Data
    frag0Mask(readResponseSlot) := generatedFragment0Mask
    frag0Pending(readResponseSlot) := responseLength0 =/= 0

    // 生成可能存在的第二个目标 line 片段
    frag1Addr(readResponseSlot) := (responseDestinationLine0 + U(BeatBytes, MMUAddrWidth bits)).resized

    frag1Data(readResponseSlot) := generatedFragment1Data
    frag1Mask(readResponseSlot) := generatedFragment1Mask
    frag1Pending(readResponseSlot) := responseLength1 =/= 0
  }

  val wcbReady = Vec(Bool(), WcbEntries)

  for (i <- 0 until WcbEntries) {
    val expected = expectedDestinationMask(wcbAddr(i))

    // 收齐该目标 line 属于 Copy 范围的所有字节后即可发出
    wcbReady(i) := wcbValid(i) && !wcbIssued(i) && ((wcbMask(i) & expected) === expected)
  }

  val FragmentCount = GCCopyEntry * 2

  val fragmentValid = Vec(Bool(), FragmentCount)
  val fragmentAddr = Vec(UInt(MMUAddrWidth bits), FragmentCount)
  val fragmentData = Vec(UInt(MMUDataWidth bits), FragmentCount)
  val fragmentMask = Vec(UInt(BeatBytes bits), FragmentCount)
  val fragmentOwner = Vec(UInt(RobPtrBits bits), FragmentCount)
  val fragmentIsSecond = Vec(Bool(), FragmentCount)
  val fragmentCanMerge = Vec(Bool(), FragmentCount)

  // 将每个 ROB slot 的两个片段展开为统一调度队列
  for (i <- 0 until GCCopyEntry) {
    val first = i * 2
    val second = first + 1

    fragmentValid(first) := slotBusy(i) && frag0Pending(i)
    fragmentAddr(first) := frag0Addr(i)
    fragmentData(first) := frag0Data(i)
    fragmentMask(first) := frag0Mask(i)
    fragmentOwner(first) := U(i, RobPtrBits bits)
    fragmentIsSecond(first) := False

    fragmentValid(second) := slotBusy(i) && frag1Pending(i)
    fragmentAddr(second) := frag1Addr(i)
    fragmentData(second) := frag1Data(i)
    fragmentMask(second) := frag1Mask(i)
    fragmentOwner(second) := U(i, RobPtrBits bits)
    fragmentIsSecond(second) := True

    for (fragment <- 0 until 2) {
      val fragmentIndex = i * 2 + fragment
      val hitVector = Vec(Bool(), WcbEntries)

      for (w <- 0 until WcbEntries) {
        // 已经 ready 或已经 issued 的 WCB 不允许继续合并
        hitVector(w) := wcbValid(w) && !wcbIssued(w) &&
          !wcbReady(w) && wcbAddr(w) === fragmentAddr(fragmentIndex)
      }

      fragmentCanMerge(fragmentIndex) := fragmentValid(fragmentIndex) && hitVector.asBits.orR
    }
  }

  // 查找空闲 WCB 项
  val wcbFreeMask = Bits(WcbEntries bits)

  for (i <- 0 until WcbEntries) {
    wcbFreeMask(i) := !wcbValid(i)
  }

  val hasFreeWcb = wcbFreeMask.orR
  val fragmentValidBits = fragmentValid.asBits
  val mergeableFragmentBits = fragmentCanMerge.asBits

  // 优先选择可以合并到已有 WCB 的片段，减少 WCB 项占用
  val selectedFragmentOH = Bits(FragmentCount bits)
  selectedFragmentOH := 0

  when(mergeableFragmentBits.orR) {
    selectedFragmentOH := OHMasking.first(mergeableFragmentBits)
  }.elsewhen(hasFreeWcb && fragmentValidBits.orR) {
    selectedFragmentOH := OHMasking.first(fragmentValidBits)
  }

  val selectedFragmentValid = selectedFragmentOH.orR
  val selectedFragmentIndex = OHToUInt(selectedFragmentOH)

  val selectedFragmentAddr = fragmentAddr(selectedFragmentIndex)
  val selectedFragmentData = fragmentData(selectedFragmentIndex)
  val selectedFragmentMask = fragmentMask(selectedFragmentIndex)
  val selectedFragmentOwner = fragmentOwner(selectedFragmentIndex)
  val selectedFragmentIsSecond = fragmentIsSecond(selectedFragmentIndex)

  // 查询选中片段是否命中可继续合并的 WCB
  val selectedWcbHitMask = Bits(WcbEntries bits)

  for (i <- 0 until WcbEntries) {
    selectedWcbHitMask(i) := wcbValid(i) && !wcbIssued(i) &&
      !wcbReady(i) && wcbAddr(i) === selectedFragmentAddr
  }

  val selectedWcbHit = selectedWcbHitMask.orR

  // 默认选择第一个空闲 WCB；命中已有 WCB 时改为命中项
  val selectedWcbIndex = UInt(WcbPtrBits bits)
  selectedWcbIndex := OHToUInt(OHMasking.first(wcbFreeMask)).resize(WcbPtrBits)

  when(selectedWcbHit) {
    selectedWcbIndex := OHToUInt(OHMasking.first(selectedWcbHitMask)).resize(WcbPtrBits)
  }

  val fragmentAccepted =
    selectedFragmentValid && (selectedWcbHit || hasFreeWcb)

  when(fragmentAccepted) {
    val ownerTag = slotReqIdx(selectedFragmentOwner)

    when(selectedWcbHit) {
      // 使用字节掩码把新片段合并进已有 WCB
      val expandedMask = expandByteMask(selectedFragmentMask)

      wcbData(selectedWcbIndex) := ((wcbData(selectedWcbIndex) & ~expandedMask) | (selectedFragmentData & expandedMask)).resized
      wcbMask(selectedWcbIndex) := wcbMask(selectedWcbIndex) | selectedFragmentMask

      // 当前结构假定一个目标 line 最多只有两个源 beat 贡献
      assert(!wcbContrib1Valid(selectedWcbIndex))

      wcbContrib1Valid(selectedWcbIndex) := True
      wcbContrib1Slot(selectedWcbIndex) := selectedFragmentOwner
      wcbContrib1Tag(selectedWcbIndex) := ownerTag
    }.otherwise {
      // 使用空闲项创建新的 WCB line
      wcbValid(selectedWcbIndex) := True
      wcbIssued(selectedWcbIndex) := False
      wcbAddr(selectedWcbIndex) := selectedFragmentAddr
      wcbData(selectedWcbIndex) := selectedFragmentData
      wcbMask(selectedWcbIndex) := selectedFragmentMask

      wcbContrib0Valid(selectedWcbIndex) := True
      wcbContrib0Slot(selectedWcbIndex) := selectedFragmentOwner
      wcbContrib0Tag(selectedWcbIndex) := ownerTag

      wcbContrib1Valid(selectedWcbIndex) := False
    }

    // 片段进入 WCB 后，从 ROB 的待调度片段中移除
    when(selectedFragmentIsSecond) {
      frag1Pending(selectedFragmentOwner) := False
    }.otherwise {
      frag0Pending(selectedFragmentOwner) := False
    }
  }

  // 目标写请求发射
  // 每周期选择一个已经收齐数据、但尚未发出的 WCB
  val writeIssueMask = wcbReady.asBits
  val writeIssueOH = OHMasking.first(writeIssueMask)
  val writeIssueValid = writeIssueMask.orR
  val writeIssueIndex = OHToUInt(writeIssueOH).resize(WcbPtrBits)

  io.writeMReq.Request.valid := writeIssueValid
  io.writeMReq.Request.payload.RequestVirtualAddr := wcbAddr(writeIssueIndex)
  io.writeMReq.Request.payload.RequestSourceID := io.writeMReq.ConherentRequsetSourceID.payload
  io.writeMReq.Request.payload.RequestType_isWrite := True
  io.writeMReq.Request.payload.RequestWStrb := wcbMask(writeIssueIndex)
  io.writeMReq.Request.payload.RequestData := wcbData(writeIssueIndex)
  io.writeMReq.Request.payload.RequestSize := U(BeatBytes, LineBytesNumBitSize bits)
  io.writeMReq.Request.payload.NeedResponse := True
  io.writeMReq.Request.payload.NeedDoCmpxChg := False

  val writeRequestFire = io.writeMReq.Request.fire

  when(writeRequestFire) {
    val sourceId = io.writeMReq.ConherentRequsetSourceID.payload.resized

    // 保存写响应 SourceID 到 WCB 项的映射
    sourceId2WcbSlot(sourceId) := writeIssueIndex

    // 已发出的 WCB 不允许继续接收片段
    wcbIssued(writeIssueIndex) := True
  }

  // 目标写响应和源 beat 乱序完成
  val writeResponseSourceId = io.writeMReq.Response.payload.ResponseSourceID.resized
  val writeResponseWcbIndex = sourceId2WcbSlot(writeResponseSourceId)

  io.writeMReq.Response.ready := taskValid && wcbValid(writeResponseWcbIndex) && wcbIssued(writeResponseWcbIndex)
  
  val writeResponseFire = io.writeMReq.Response.fire

  // 对每个 ROB slot 判断当前写响应是否确认了它贡献的目标 line
  val responseContributor = Vec(Bool(), GCCopyEntry)

  for (i <- 0 until GCCopyEntry) {
    val slotIndex = U(i, RobPtrBits bits)

    val contributor0Hit =
      wcbContrib0Valid(writeResponseWcbIndex) &&
        wcbContrib0Slot(writeResponseWcbIndex) === slotIndex &&
        wcbContrib0Tag(writeResponseWcbIndex) === slotReqIdx(i)

    val contributor1Hit =
      wcbContrib1Valid(writeResponseWcbIndex) &&
        wcbContrib1Slot(writeResponseWcbIndex) === slotIndex &&
        wcbContrib1Tag(writeResponseWcbIndex) === slotReqIdx(i)

    responseContributor(i) := writeResponseFire && slotBusy(i) && (contributor0Hit || contributor1Hit)
  }

  for (i <- 0 until GCCopyEntry) {
    // 当前周期是否有该 slot 的一个片段进入 WCB
    val fragmentIncrement = fragmentAccepted && selectedFragmentOwner === U(i, RobPtrBits bits)

    // 当前周期是否有该 slot 的一个 WCB 写响应返回
    val responseDecrement = responseContributor(i)

    // 组合计算同时处理加一和减一后的未确认目标 line 数量
    val pendingAfterEvents =
      slotPendingLineAck(i).resize(3) +
        fragmentIncrement.asUInt.resize(3) -
        responseDecrement.asUInt.resize(3)

    // 判断接受片段后是否仍有片段留在 ROB 外部等待调度
    val fragment0Remains =
      frag0Pending(i) &&
        !(fragmentAccepted &&
          selectedFragmentOwner === U(i, RobPtrBits bits) &&
          !selectedFragmentIsSecond)

    val fragment1Remains =
      frag1Pending(i) &&
        !(fragmentAccepted &&
          selectedFragmentOwner === U(i, RobPtrBits bits) &&
          selectedFragmentIsSecond)

    // 更新正在等待目标写响应的 line 数量。
    when(fragmentIncrement && !responseDecrement) {
      slotPendingLineAck(i) := slotPendingLineAck(i) + 1
    }.elsewhen(!fragmentIncrement && responseDecrement) {
      assert(slotPendingLineAck(i) =/= 0)
      slotPendingLineAck(i) := slotPendingLineAck(i) - 1
    }

    // 一个源 beat 的全部片段都进入 WCB，且所有相关目标写均已确认后完成。
    val beatCompletesNow =
      responseDecrement &&
        pendingAfterEvents === 0 &&
        !fragment0Remains &&
        !fragment1Remains

    when(beatCompletesNow) {
      val completedBeat = slotReqIdx(i)
      val completedIndex = completionIndex(completedBeat)

      // 如果完成窗口槽位有效，只允许它属于同一个逻辑 beat。
      assert(
        !completionValid(completedIndex) ||
          completionTag(completedIndex) === completedBeat
      )

      completionValid(completedIndex) := True
      completionTag(completedIndex) := completedBeat

      // 完成信息已经进入 completion window，因此 ROB 数据槽可以立即释放。
      slotBusy(i) := False
      slotForwardValid(i) := False
      slotPendingLineAck(i) := 0
    }
  }

  when(writeResponseFire) {
    // 写响应返回后释放对应 WCB。
    wcbValid(writeResponseWcbIndex) := False
    wcbIssued(writeResponseWcbIndex) := False
    wcbMask(writeResponseWcbIndex) := U(0, BeatBytes bits)
    wcbContrib0Valid(writeResponseWcbIndex) := False
    wcbContrib1Valid(writeResponseWcbIndex) := False
  }

  // --------------------------------------------------------------------------
  // 连续完成前缀提交
  // --------------------------------------------------------------------------

  val commitIndex = completionIndex(nextCommitIdx)

  // 只有完成窗口头部对应 nextCommitIdx 时，连续前缀才能向前推进。
  val commitHeadDone =
    taskValid &&
      nextCommitIdx < readBeatCount &&
      completionValid(commitIndex) &&
      completionTag(commitIndex) === nextCommitIdx

  val commitLogicalOffset =
    (nextCommitIdx.resize(32) << OffBits).resize(32)

  val commitLogicalStart =
    Mux(
      nextCommitIdx === 0,
      U(0, 32 bits),
      commitLogicalOffset - srcOff.resize(32)
    )

  val commitValidStart =
    Mux(
      nextCommitIdx === 0,
      srcOff.resize(32),
      U(0, 32 bits)
    )

  val commitMaximumValid = BeatBytesU32 - commitValidStart

  val commitRemaining =
    Mux(
      commitLogicalStart < totalSize,
      totalSize - commitLogicalStart,
      U(0, 32 bits)
    )

  val commitValidBytes =
    Mux(
      commitRemaining <= commitMaximumValid,
      commitRemaining,
      commitMaximumValid
    )

  when(commitHeadDone) {
    completionValid(commitIndex) := False
    committedBytes := committedBytes + commitValidBytes
    nextCommitIdx := nextCommitIdx + 1
  }

  // --------------------------------------------------------------------------
  // Fetch 直接索引 forwarding 和读取响应同周期 bypass
  // --------------------------------------------------------------------------

  /**
   * 为一个 Fetch forwarding 端口生成组合查询逻辑。
   *
   * 对请求中的每个字节分别判断：
   *   1. 是否位于当前 Copy 的目标地址范围；
   *   2. 目标内存中是否已经可见；
   *   3. 若尚不可见，源数据是否存在于 ROB 或当前读取响应；
   *   4. 如果数据未返回，则拉高 stall。
   */
  def driveForwardLookup(port: GCCopyForwardPort): Unit = {
    // 默认无转发、无阻塞。
    port.stall := False
    port.mask := 0
    port.data := 0

    val copyEnd =
      (dstPtr + totalSize.resize(GCElementWidth)).resize(GCElementWidth)

    // 只有 epoch 匹配当前事务时，才允许使用 Fetch 提供的预解码信息。
    val predecodeMatches =
      port.predecodedValid && port.epoch === copyEpoch

    when(port.valid && taskValid) {
      for (byte <- 0 until BeatBytes) {
        // 查询长度内的字节才参与匹配。
        val laneRequested =
          U(byte, LineBytesNumBitSize bits) <
            port.size.resize(LineBytesNumBitSize)

        val requestByteAddr =
          (port.addr + U(byte, MMUAddrWidth bits)).resize(MMUAddrWidth)

        val requestByteAddrGc =
          requestByteAddr.resize(GCElementWidth)

        // 判断当前查询字节是否位于 Copy 目标范围中。
        val inCopyRange =
          requestByteAddrGc >= dstPtr &&
            requestByteAddrGc < copyEnd

        // 从目标地址反推出 Copy 逻辑偏移及对应源地址。
        val logicalOffset =
          (requestByteAddrGc - dstPtr).resize(32)

        val sourceByteAddr =
          (srcPtr + logicalOffset.resize(GCElementWidth)).resize(GCElementWidth)

        // 未使用预解码信息时，从地址完整计算 beat 编号和字节偏移。
        val calculatedBeatIdx =
          ((sourceByteAddr - srcBase) >> OffBits).resize(32)

        val calculatedByteOffset =
          sourceByteAddr(OffBits - 1 downto 0)

        // 预解码结果以请求第一个字节为起点，逐字节线性递增。
        val predecodedLinearByte =
          port.firstByteOffset.resize(OffBits + 1) +
            U(byte, OffBits + 1 bits)

        val predecodedBeatIdx =
          port.firstBeatIdx +
            (predecodedLinearByte >> OffBits).resize(32)

        val predecodedByteOffset =
          predecodedLinearByte(OffBits - 1 downto 0)

        val queryBeatIdx = UInt(32 bits)
        val queryByteOffset = UInt(OffBits bits)

        queryBeatIdx := calculatedBeatIdx
        queryByteOffset := calculatedByteOffset

        when(predecodeMatches) {
          queryBeatIdx := predecodedBeatIdx
          queryByteOffset := predecodedByteOffset
        }

        // ROB 使用 beat index 低位直接寻址，并通过完整 tag 防止别名。
        val querySlot =
          queryBeatIdx(RobPtrBits - 1 downto 0)

        // 连续提交范围内的数据已写入内存。
        // 非连续但已经完成的 beat 也可以直接从目标内存读取。
        val memoryVisible =
          logicalOffset < committedBytes ||
            completionHit(queryBeatIdx)

        // 查询数据是否已经保存在 ROB 中。
        val robHit =
          slotBusy(querySlot) &&
            slotForwardValid(querySlot) &&
            slotReqIdx(querySlot) === queryBeatIdx

        // 当前源读取响应与查询匹配时，直接使用响应数据，不等待写入 slotData。
        val responseBypassHit =
          readResponseFire &&
            readResponseSlot === querySlot &&
            slotReqIdx(readResponseSlot) === queryBeatIdx

        val selectedSourceData =
          Mux(
            responseBypassHit,
            io.readMReq.Response.payload.ResponseData,
            slotData(querySlot)
          )

        val forwardedByte =
          selectedSourceData.subdivideIn(8 bits)(queryByteOffset)

        // 目标内存尚不可见时，必须从 Copy 内部转发或阻塞 Fetch。
        when(laneRequested && inCopyRange && !memoryVisible) {
          when(responseBypassHit || robHit) {
            port.mask(byte) := True
            port.data(byte * 8 + 7 downto byte * 8) := forwardedByte
          }.otherwise {
            port.stall := True
          }
        }
      }
    }
  }

  // 三个 Fetch 流水线端口共用相同的 forwarding 逻辑。
  driveForwardLookup(io.FwdMain)
  driveForwardLookup(io.FwdPush)
  driveForwardLookup(io.FwdPre)

  val counter = RegInit(U(0, 64 bits))

  when(taskValid) {
    counter := counter + 1
  }
}

/** 单独生成 GCCopy Verilog 的入口。 */
object GCCopy2Verilog extends App {
  Config.spinal.generateVerilog(new GCCopy())
}