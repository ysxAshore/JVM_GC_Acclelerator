package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

// ============================================================================
// GCCopyReadMeta —— 向 Copy 模块发起 store-buffer 查询时携带的元数据
//
// 当某个任务标记为"来自 GCTrace 推送"（fromTracePush = true），说明该对象
// 可能正处于 Copy 过程中（从 from-space 搬到 to-space）。此时 Fetch 读取
// OOP/MarkWord 不应直接发 MMU 请求，而应先查询 Copy 的 store-buffer
//
// 字段说明：
//   needQuery       — 是否需要向 Copy 模块发起查询（仅 fromTracePush 任务为真）
//   epoch           — 当前 Copy 事务的 epoch，用于版本匹配
//   predecodedValid — 预解码信息是否有效（避免 Fetch 在关键路径上计算 beat 索引）
//   firstBeatIdx    — 如果命中 Copy buffer，数据在源端第几个 beat 中
//   firstByteOffset — 数据在 beat 内的字节偏移
// ============================================================================
case class GCCopyReadMeta() extends Bundle with GCTopParameters with GCParameters with HWParameters {
  val needQuery       = Bool()
  val epoch           = UInt(GCCopyEpochWidth bits)
  val predecodedValid = Bool()
  val firstBeatIdx    = UInt(32 bits)
  val firstByteOffset = UInt(log2Up(LineBytesNum) bits)
}

// ============================================================================
// GcFetchData —— Fetch 流水线中携带的完整任务上下文
//
// 每个任务经过 Fetch 流水线后，需要携带以下信息交给下游处理单元：
//
//   task           — 任务基地址（OOP 指针，已去除低位 tag 和 TracePushTagBit）
//   oopType        — OOP 类型：NotArrayOop(普通对象) / PartialArrayOop(部分数组)
//   fromObj        — 从 oop 中读取出的引用目标对象地址（即 oop 指向的对象的基地址）
//   markWord       — 目标对象的 Mark Word（GC 标记/转发/锁信息）
//   klassPtr       — 目标对象的 Klass 指针（类型元数据指针）
//   srcLength      — 源对象长度（32位，用于数组类型的长度信息）
//
//   fromTracePush  — 该任务是否由 GCTrace 实时推送（而非从 TaskStack 出栈）为 true 时需要查询 Copy store-buffer
//   oopCopyMeta    — OOP 读取时的 Copy 转发查询元数据
// ============================================================================
case class GcFetchData() extends Bundle with GCTopParameters with GCParameters with HWParameters {
  val task      = UInt(GCElementWidth bits)
  val oopType   = UInt(GCOopTypeWidth bits)
  val fromObj   = UInt(GCElementWidth bits)
  val markWord  = UInt(GCElementWidth bits)
  val klassPtr  = UInt(GCElementWidth bits)
  val srcLength = UInt(32 bits)

  val fromTracePush = Bool()
  val oopCopyMeta    = GCCopyReadMeta()
}

// ============================================================================
// GCFetch — 从 TaskStack 获取任务，通过 MMU 读取 OOP 和 MarkWord，
//           最终分发到 ArrayProcess 或 OopProcess 处理单元
//
// 顶层数据流：
//   TaskStack ──Pop──> preBuf(环形预取缓冲) ──> mainFsm ──> Fetch2ArrayProcess
//         │                                                      │
//         └──PrePop──> preFsm 提前完成 OOP + MW 读取 ─────────────┘
//                                                               Fetch2OopProcess
//   GCTrace ──Trace2Fetch──> pushFsm ──> mainFsm ───────────────>
//                                                               (二选一)
//
// 每条任务的处理流程（两阶段读取）：
//   阶段1: 读 OOP  — 从 task 地址读取 8 字节，解码得到 fromObj（目标对象地址）
//         特殊处理：PartialArrayOop 跳过 OOP 读取，因为 task 本身就是 fromObj
//   阶段2: 读 MW   — 从 fromObj 地址读取 MarkWord(8B) + KlassPtr(8B) [+可选 Length(4B)]
//
// 三条独立 MMU 端口（避免相互阻塞）：
//   MainMreq — mainFsm 使用，处理 Pop 出的主任务
//   PushMreq — pushFsm 使用，处理 Trace2Fetch 实时推送
//   PreMreq  — preFsm 使用，批量预取
//
// Copy Store-Buffer 转发：
//   当任务来自 GCTrace（fromTracePush=true）时，目标对象可能正被 Copy 搬运。
//   此时通过 CopyFwd* 端口查询 Copy 模块的 store-buffer：
//     1. 完全命中 (fullFwd)    — 数据全部在 buffer 中，跳过 MMU 请求
//     2. 部分命中              — 合并 buffer 数据和 MMU 响应
//     3. stall               — 数据正在传输中，等待 Copy 完成
// ============================================================================
class GCFetch extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val MainMreq = master(new LocalMMUIO)
    val PushMreq = master(new LocalMMUIO)
    val PreMreq  = master(new LocalMMUIO)

    // 与 TaskStack 的接口：
    //   Pop    — 主消费端口，mainFsm 从此获取待处理任务
    //   PrePop — 预取端口，preFsm 从此提前获取未来任务
    val toFetch          = slave(new GCToFetch)
    val gcWriteSrcOopPtr = slave(new GCWriteSrcOopPtr) // Copy2Survivor 写入 srcOopPtr 的转发通知（用于 MarkWord 缓存更新）
    val Trace2Fetch      = slave Stream UInt(GCElementWidth bits) // GCTrace 实时推送的任务流（优先级高于 Pop）
    val CopyDone         = in Bool()
    val CopyState        = in(GCCopyPublicState()) // Copy 模块当前事务的公共状态（用于计算转发地址范围）

    // Copy store-buffer 转发查询端口（每条流水线一个）
    //   valid/addr/size/epoch — Fetch 发出查询
    //   stall/mask/data       — Copy 返回结果
    val CopyFwdMain = master(GCCopyForwardPort())
    val CopyFwdPush = master(GCCopyForwardPort())
    val CopyFwdPre  = master(GCCopyForwardPort())

    val Fetch2ArrayProcess = master(new GCToProcessUnit)
    val Fetch2OopProcess   = master(new GCToProcessUnit)
    val ConfigIO           = slave(new GCFetchConfigIO)
    val DebugTimeStamp     = in UInt(64 bits)
  }

  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid  := False
    m.Request.payload.clearAll()
    m.Response.ready := False
  }

  // 没有用issuedReq
  def driveReadReq(m: LocalMMUIO, addr: UInt, sizeBytes: UInt): Unit = {
    m.Request.valid := True

    m.Request.payload.NeedResponse        := True
    m.Request.payload.NeedDoCmpxChg       := False
    m.Request.payload.RequestSize         := sizeBytes.resize(LineBytesNumBitSize)
    m.Request.payload.RequestWStrb        := U(0)
    m.Request.payload.RequestData         := U(0)
    m.Request.payload.RequestType_isWrite := False
    m.Request.payload.RequestSourceID     := m.ConherentRequsetSourceID.payload
    m.Request.payload.RequestVirtualAddr  := addr
  }

  // 辅助函数：Copy Store-Buffer 转发查询
  def driveCopyFwd(port: GCCopyForwardPort, addr: UInt, sizeBytes: UInt, meta: GCCopyReadMeta): Unit = {
    port.valid           := meta.needQuery
    port.addr            := addr.resize(MMUAddrWidth)
    port.size            := sizeBytes.resize(LineBytesNumBitSize)
    port.epoch           := meta.epoch
    port.predecodedValid := meta.predecodedValid
    port.firstBeatIdx    := meta.firstBeatIdx
    port.firstByteOffset := meta.firstByteOffset
  }

  // 根据 sizeBytes 生成字节有效掩码 例: sizeBytes=5, LineBytesNum=32 → ret[4:0]=1, ret[31:5]=0
  def requestedByteMask(sizeBytes: UInt): Bits = {
    val ret = Bits(LineBytesNum bits)
    for (i <- 0 until LineBytesNum) {
      ret(i) := U(i, LineBytesNumBitSize bits) < sizeBytes.resize(LineBytesNumBitSize)
    }
    ret
  }

  // 将字节掩码扩展为位掩码（每个有效字节对应 8 个 1 位）例: mask[0]=1, mask[1]=0 → ret[7:0]=0xFF, ret[15:8]=0x00
  def byteMaskToBits(mask: Bits): Bits = {
    val ret = Bits(MMUDataWidth bits)
    for (i <- 0 until LineBytesNum) {
      ret(i * 8 + 7 downto i * 8) := Mux(mask(i), B"8'xFF", B"8'x00")
    }
    ret
  }

  // 合并 Copy store-buffer 转发数据与 MMU 响应数 对于 Copy 已覆盖的字节用 fwdData，其余用 memoryData
  def mergeCopyForward(memoryData: UInt, fwdMask: Bits, fwdData: UInt): UInt = {
    val bitMask = byteMaskToBits(fwdMask)
    ((memoryData.asBits & ~bitMask) | (fwdData.asBits & bitMask)).asUInt
  }

  clearMreq(io.MainMreq)
  clearMreq(io.PushMreq)
  clearMreq(io.PreMreq)

  io.toFetch.Pop.ready    := False
  io.toFetch.PrePop.ready := False
  io.Trace2Fetch.ready    := False

  def clearCopyFwd(port: GCCopyForwardPort): Unit = {
    port.valid           := False
    port.addr            := 0
    port.size            := 0
    port.epoch           := 0
    port.predecodedValid := False
    port.firstBeatIdx    := 0
    port.firstByteOffset := 0
  }

  clearCopyFwd(io.CopyFwdMain)
  clearCopyFwd(io.CopyFwdPush)
  clearCopyFwd(io.CopyFwdPre)

  io.Fetch2ArrayProcess.clearOut()
  io.Fetch2OopProcess.clearOut()

  require(GCElementWidth > TracePushTagBit)

  val CopyOffBits = log2Up(LineBytesNum)
  val TracePushTagMask = U(BigInt(1) << TracePushTagBit, GCElementWidth bits) // TracePushTagBit 表示 用的哪一位表示task source

  def stripTraceTag(payload: UInt): UInt = (payload & ~TracePushTagMask).resize(GCElementWidth) // 对task去掉task source

  // 对于task 去掉 trace tag 以及 oop tag
  def taskBaseFromPayload(payload: UInt): UInt = {
    val untagged = stripTraceTag(payload)
    val lowTag = untagged(GCOopTagWidth - 1 downto 0).resize(GCElementWidth)
    (untagged - lowTag).resize(GCElementWidth)
  }

  // 提前计算Copy Forward 的请求信息 省一拍的关键路径延迟
  def prepareCopyReadMeta(addr: UInt, sizeBytes: UInt, fromTracePush: Bool, meta: GCCopyReadMeta): Unit = {
    val reqEnd = addr.resize(GCElementWidth) + sizeBytes.resize(GCElementWidth)
    val copyEnd = io.CopyState.DstPtr + io.CopyState.TotalSize.resize(GCElementWidth)
    val fullyInsideCopyRange = addr.resize(GCElementWidth) >= io.CopyState.DstPtr && reqEnd <= copyEnd // 判断边界
    val logicalOffset = addr.resize(GCElementWidth) - io.CopyState.DstPtr
    val sourceByteAddr = io.CopyState.SrcPtr + logicalOffset

    meta.needQuery := fromTracePush
    meta.epoch     := io.CopyState.Epoch
    meta.predecodedValid := fromTracePush && io.CopyState.Active && fullyInsideCopyRange
    meta.firstBeatIdx := ((sourceByteAddr - io.CopyState.SrcBase) >> CopyOffBits).resize(32) // 每次请求时 是以32B对齐的请求的 所以-SrcBase可以看到是第几个源
    meta.firstByteOffset := sourceByteAddr(CopyOffBits - 1 downto 0) // 在该源请求内的第几个偏移
  }

  // ============================================================================
  // Task 接收与解析
  //
  // receiveTask 将一个原始 task payload（来自 Pop/PrePop/Trace2Fetch）解析为
  // GcFetchData 结构：
  //   — 提取 bit[63] 判断是否来自 Trace 推送
  //   — 提取低位 OopTag 判断对象类型（普通对象 vs 部分数组）
  //   — 计算 taskBase（去除所有 tag 后的纯地址）
  //   — 为 OOP 读取阶段准备好 Copy 转发元数据
  //
  //   PartialArrayOop 特殊处理：task 本身就是 fromObj（数组片段起始地址），
  //   不需要再读 OOP
  // ============================================================================
  def receiveTask(payload: UInt, data: GcFetchData): Unit = {
    val fromTrace = payload(TracePushTagBit)
    val untagged  = stripTraceTag(payload)
    val lowTag = untagged(GCOopTagWidth - 1 downto 0)
    val taskBase = taskBaseFromPayload(payload)

    data.oopType := Mux(
      lowTag === U(PartialArrayTag, GCOopTagWidth bits),
      U(PartialArrayOop, GCOopTypeWidth bits),
      U(NotArrayOop, GCOopTypeWidth bits)
    )
    data.task          := taskBase
    data.fromTracePush := fromTrace

    prepareCopyReadMeta(
      taskBase,
      U(8, LineBytesNumBitSize bits),
      fromTrace,
      data.oopCopyMeta
    )
  }

  def decodeReadOopResp(rd: UInt): UInt = Mux(
    io.ConfigIO.UseCompressedOop,
    (io.ConfigIO.CompressedOopBase + (rd(31 downto 0).resize(GCElementWidth) << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth),
    rd(GCElementWidth - 1 downto 0)
  )

  // fillKlassLen — 从读取的原始数据中提取 KlassPtr 和对象长度
  // MMU 响应数据布局（256 bit = 32 字节线）：
  //   非压缩Klass: [Length(32b)][KlassPtr(64b)][MarkWord(64b)]
  //   压缩Klass:   [KlassPtr(32b)][Length(32b)][MarkWord(64b)]
  def fillKlassLen(rd: UInt, data: GcFetchData): Unit = {
    data.klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

    data.srcLength := Mux(
      io.ConfigIO.UseCompressedKlassPointers,
      rd(GCElementWidth * 2 - 1 downto GCElementWidth + 32),
      rd(GCElementWidth * 2 + 31 downto GCElementWidth * 2)
    )
  }

  def fillMwKlassLen(rd: UInt, data: GcFetchData): Unit = {
    data.markWord := rd(GCElementWidth - 1 downto 0)
    fillKlassLen(rd, data)
  }

  def copyFetchContextWithoutMw(dst: GcFetchData, src: GcFetchData): Unit = {
    dst.task          := src.task
    dst.oopType       := src.oopType
    dst.fromObj       := src.fromObj
    dst.fromTracePush := src.fromTracePush
    dst.oopCopyMeta   := src.oopCopyMeta
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCFetch<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  def driveProcessUnit(target: GCToProcessUnit, payload: GcFetchData, effectiveMarkWord: UInt): Unit = {
    target.cmd.valid             := True
    target.cmd.payload.Task      := payload.task
    target.cmd.payload.OopType   := payload.oopType
    target.cmd.payload.SrcOopPtr := payload.fromObj
    target.cmd.payload.MarkWord  := effectiveMarkWord
    target.cmd.payload.KlassPtr  := payload.klassPtr
    target.cmd.payload.SrcLength := payload.srcLength
  }

  val oopReadSize = U(8, LineBytesNumBitSize bits)
  val mwReadSize = Mux(
    io.ConfigIO.UseCompressedKlassPointers,
    U(16),
    U(20)
  ).resize(LineBytesNumBitSize)

  val main_data = RegInit(GcFetchData().getZero)
  val push_data = RegInit(GcFetchData().getZero)

  // ============================================================================
  // Copy 部分转发寄存器
  //
  // 当 MMU 读请求发送时，采样 Copy 返回的 mask/data。
  // 每条流水线最多一个未完成的 MMU 读取，所以每条流水线一对寄存器即可。
  // MMU 响应返回时，用 mergeCopyForward() 合并转发数据。
  // ============================================================================
  val mainFwdMask = RegInit(B(0, LineBytesNum bits))
  val mainFwdData = RegInit(U(0, MMUDataWidth bits))
  val pushFwdMask = RegInit(B(0, LineBytesNum bits))
  val pushFwdData = RegInit(U(0, MMUDataWidth bits))
  val preFwdMask  = RegInit(B(0, LineBytesNum bits))
  val preFwdData  = RegInit(U(0, MMUDataWidth bits))

  // ============================================================================
  // PreFetch 环形缓冲区
  //
  // preBuf[0..PreFetchBufferNum-1] — 预取任务上下文环形缓冲区
  // preBufDone[i]                  — 第 i 个槽位的任务是否已完成 OOP+MW 读取
  //
  // 环形指针：
  //   buf_top    — 写入端（preFsm 将新预取任务追加到此位置）
  //   buf_bottom — 读取端（mainFsm 从此位置消费已完成的任务）
  //   buf_count  — 当前缓冲区中的有效条目数
  //   buf_work   — preFsm 当前正在处理的槽位索引
  //
  //  buf_capacity = PreFetchBufferNum
  //  buf_free      = buf_capacity - buf_count
  //
  // 生命周期：
  //   1. preFsm 预取任务 → 写入 buf_top，buf_count++
  //   2. preFsm 完成 OOP+MW 读取 → preBufDone[buf_work] = True
  //   3. mainFsm Pop 到已完成条目 → 从 buf_bottom 取出，buf_count--
  // ============================================================================
  val preBuf = Vec.fill(PreFetchBufferNum)(RegInit(GcFetchData().getZero))
  val preBufDone = Vec.fill(PreFetchBufferNum)(RegInit(False))

  val buf_top = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_bottom = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_count = RegInit(U(0, PreFetchBufferWidth + 1 bits))

  val buf_capacity = U(PreFetchBufferNum, PreFetchBufferWidth + 1 bits)
  val buf_free = buf_capacity - buf_count

  val buf_work = RegInit(U(0, PreFetchBufferWidth bits))

  // pushFollowRem — Push-follow 模式下剩余待 PrePop 的任务数
  //
  // 当 TaskStack 有一批新的 Push burst 之后，这些新任务需要通过 PrePop 输送给 Fetch
  // 这批任务应优先于缓冲区中已有的旧预取任务被消费。
  // pushFollowRem 记录了这批次中还剩多少个任务需要 PrePop
  //
  // 两种模式：
  //   Normal 模式：PushCount==0 且 pushFollowRem==0，新 PrePop 追加到 buf_top
  //   Push-follow 模式：PushCount!=0 或 pushFollowRem!=0，新 Push 的任务插入到
  //                     buf_bottom 之前，优先被 mainFsm 消费
  val pushFollowRem = RegInit(U(0, 32 bits))

  def bufInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, PreFetchBufferNum, step).resize(PreFetchBufferWidth)
  def bufDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, PreFetchBufferNum, step).resize(PreFetchBufferWidth)
  def resetSlot(idx: UInt): Unit = preBufDone(idx) := False

  val mainIsIdle     = Bool()
  val mainIsWaitDone = Bool()
  val pushIsIdle     = Bool()

  val mainGotoReadOop = Bool()
  val mainGotoSend    = Bool()

  mainGotoReadOop := False
  mainGotoSend    := False

  val targetDone = Mux(
    main_data.oopType === U(NotArrayOop),
    io.Fetch2OopProcess.Done,
    io.Fetch2ArrayProcess.Done
  )

  val targetDoneSeen = RegInit(False)
  when(targetDone && !mainIsWaitDone) {
    targetDoneSeen := True
  }

  // mainFsm Pop 到预取条目但 preFsm 尚未完成该条目的 OOP+MW 读取，
  // 此时需要等待 preFsm 完成 buf_bottom 对应的条目
  val waitForPrefetch = RegInit(False)

  // ============================================================================
  // MarkWord 转发缓存（Forwarding Cache）
  //
  // 问题场景（RAW 冒险）：
  //   1. Fetch 读取了对象 A 的 MarkWord（旧值，如 unlocked 状态）
  //   2. Copy2Survivor 将 A 拷贝到 to-space，并安装了转发 MarkWord
  //   3. Fetch 在 SEND 时应使用新 MarkWord，而非已读取的旧值
  //
  // 解决方案：一个小型全相联缓存，存储 Copy2Survivor 的转发映射. 满后会覆盖最旧的槽位
  // 查询优先级（由高到低）：
  //   1. 当前周期的 writeForward 通知
  //   2. 转发缓存中的历史条目
  //   3. fallback（之前读取的 MarkWord）
  //
  // 清理条件：所有流水线空闲 + 预取缓冲为空时，安全清空
  // ============================================================================
  val ForwardCacheEntries = 1 << log2Up(PreFetchBufferNum + 4)

  val fwdCacheValid = Vec.fill(ForwardCacheEntries)(RegInit(False))
  val fwdCacheObj = Vec.fill(ForwardCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val fwdCacheValue = Vec.fill(ForwardCacheEntries)(RegInit(U(0, GCElementWidth bits)))
  val fwdCacheReplacePtr = RegInit(U(0, log2Up(ForwardCacheEntries) bits))

  val incomingFwdValid = io.gcWriteSrcOopPtr.writeForward.valid
  val incomingFwdObj = io.gcWriteSrcOopPtr.writeForward.payload.srcOopPtr
  val incomingFwdValue = io.gcWriteSrcOopPtr.writeForward.payload.writeValue

  val incomingFwdHitVec = Bits(ForwardCacheEntries bits)

  for (i <- 0 until ForwardCacheEntries) {
    incomingFwdHitVec(i) := fwdCacheValid(i) && fwdCacheObj(i) === incomingFwdObj
  }

  val incomingFwdHit = incomingFwdHitVec.orR
  val incomingFwdHitIndex = OHToUInt(incomingFwdHitVec)

  when(incomingFwdValid) {
    when(incomingFwdHit) {
      fwdCacheValue(incomingFwdHitIndex) := incomingFwdValue
    } otherwise {
      fwdCacheValid(fwdCacheReplacePtr) := True
      fwdCacheObj(fwdCacheReplacePtr) := incomingFwdObj
      fwdCacheValue(fwdCacheReplacePtr) := incomingFwdValue
      fwdCacheReplacePtr := fwdCacheReplacePtr + U(1, fwdCacheReplacePtr.getWidth bits)
    }
  }.elsewhen(mainIsIdle && pushIsIdle && buf_count === U(0, buf_count.getWidth bits) && !waitForPrefetch) {
    for (i <- 0 until ForwardCacheEntries) {
      fwdCacheValid(i) := False
    }
    fwdCacheReplacePtr := 0
  }

  def resolveForwardMark(obj: UInt, fallback: UInt): UInt = {
    val resolved = UInt(GCElementWidth bits)

    resolved := fallback

    for (i <- 0 until ForwardCacheEntries) {
      when(fwdCacheValid(i) && fwdCacheObj(i) === obj) {
        resolved := fwdCacheValue(i)
      }
    }

    when(incomingFwdValid && incomingFwdObj === obj) {
      resolved := incomingFwdValue
    }

    resolved
  }

  // ============================================================================
  // Main StateMachine — 主任务处理流水线
  //
  // 状态流转（以普通对象为例）：
  //   IDLE ─Pop.fire──> READ_OOP_REQ ──> READ_OOP_RESP ──> READ_MW_REQ
  //     ^                                                    │
  //     │                                                    v
  //   WAIT_DONE <────────────── SEND <──────────────── READ_MW_RESP
  //
  // IDLE 特殊路径：
  //   — 如果 Pop 到的任务在预取缓冲中已完成（preBufDone=True）：
  //     直接 goto(SEND)，跳过所有 MMU 读取
  //   — 如果 Pop 到的任务在预取缓冲中但尚未完成：
  //     设置 waitForPrefetch=True，等待 preFsm 完成
  // ============================================================================
  val mainFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State   // 等待 Copy 转发或发出 OOP 读取请求
    val READ_OOP_RESP = new State   // 等待 OOP 读取的 MMU 响应
    val READ_MW_REQ   = new State   // 等待 Copy 转发或发出 MarkWord+Klass+Len 读取请求
    val READ_MW_RESP  = new State   // 等待 MarkWord 读取的 MMU 响应
    val SEND          = new State   // 向 ArrayProcess 或 OopProcess 分发任务
    val WAIT_DONE     = new State   // 等待下游处理单元完成

    IDLE.whenIsActive {
      // Pop 允许条件（所有条件同时满足）：
      //   1. pushFsm 空闲（不在处理 Trace2Fetch 任务）
      //   2. 没有 Trace2Fetch 待处理（优先处理实时推送）
      //   3. 没有等待预取完成
      //   4. pushFollowRem == 0（不在 Push-follow 模式中）
      val fetchPushFollowActive = pushFollowRem =/= U(0, 32 bits)
      io.toFetch.Pop.ready := pushIsIdle && !io.Trace2Fetch.valid && !waitForPrefetch && !fetchPushFollowActive

      when(io.toFetch.Pop.fire) {
        val popBase = taskBaseFromPayload(io.toFetch.Pop.payload)
        val bottomValid = buf_count =/= U(0, buf_count.getWidth bits)
        val bottomHit = bottomValid && preBuf(buf_bottom).task === popBase

        // 场景1: 预取缓冲命中 + 已完成 → 直接分发，跳过 MMU 读取
        when(bottomHit && preBufDone(buf_bottom)) {
          main_data := preBuf(buf_bottom)

          resetSlot(buf_bottom)
          buf_bottom := bufInc(buf_bottom, U(1, PreFetchBufferWidth bits))
          buf_count := buf_count - U(1, buf_count.getWidth bits)

          goto(SEND)

        // 场景2: 预取缓冲命中但尚未完成 → 等待 preFsm
        }.elsewhen(bottomHit) {
          waitForPrefetch := True

        // 场景3: 预取缓冲未命中 → 执行完整的 OOP+MW 读取流程
        }.otherwise {
          receiveTask(io.toFetch.Pop.payload, main_data)
          main_data.fromObj := U(0)
          goto(READ_OOP_REQ)
        }

      // 场景4: waitForPrefetch 等待的条目已完成 → 直接分发
      }.elsewhen(
        waitForPrefetch &&
          buf_count =/= U(0, buf_count.getWidth bits) &&
          preBufDone(buf_bottom)
      ) {
        waitForPrefetch := False
        main_data       := preBuf(buf_bottom)

        resetSlot(buf_bottom)
        buf_bottom := bufInc(buf_bottom,U(1, PreFetchBufferWidth bits))
        buf_count := buf_count - U(1, buf_count.getWidth bits)

        goto(SEND)
      }
    }

    // READ_OOP_REQ — 阶段1：读取 OOP（8字节），解码得到 fromObj
    //
    // PartialArrayOop 快速路径：
    //   task 本身就是 fromObj，跳过 OOP 读取，直接进入 READ_MW_REQ。
    //
    // 普通对象路径：
    //   先查询 Copy store-buffer（driveCopyFwd）：
    //     — fullFwd（完全命中）：直接用 Copy 数据解码 fromObj → READ_MW_REQ
    //     — 部分命中/未命中：发送 MMU 读请求 → READ_OOP_RESP
    //     — stall：Copy 正在传输中，等待
    READ_OOP_REQ.whenIsActive {
      // PartialArrayOop: task 就是 fromObj，跳过 OOP 读取
      when(main_data.oopType === U(PartialArrayOop)) {
        main_data.fromObj := main_data.task

        goto(READ_MW_REQ)

      }.otherwise {
        driveCopyFwd(io.CopyFwdMain, main_data.task, oopReadSize, main_data.oopCopyMeta)

        val reqMask = requestedByteMask(oopReadSize)
        val fullFwd = (io.CopyFwdMain.mask & reqMask) === reqMask

        when(!io.CopyFwdMain.stall) {
          when(fullFwd) {
            val newFromObj = decodeReadOopResp(io.CopyFwdMain.data)

            main_data.fromObj := newFromObj

            goto(READ_MW_REQ)

          }.otherwise {
            driveReadReq(io.MainMreq, main_data.task, oopReadSize)

            when(io.MainMreq.Request.fire) {
              mainFwdMask := io.CopyFwdMain.mask
              mainFwdData := io.CopyFwdMain.data
              goto(READ_OOP_RESP)
            }
          }
        }
      }
    }

    READ_OOP_RESP.whenIsActive {
      io.MainMreq.Response.ready := True

      when(io.MainMreq.Response.fire) {
        val rd = mergeCopyForward(io.MainMreq.Response.payload.ResponseData, mainFwdMask, mainFwdData)
        val newFromObj = decodeReadOopResp(rd)

        main_data.fromObj := newFromObj

        goto(READ_MW_REQ)
      }
    }

    READ_MW_REQ.whenIsActive {
      driveReadReq(
        io.MainMreq,
        main_data.fromObj,
        mwReadSize
      )

      when(io.MainMreq.Request.fire) {
        goto(READ_MW_RESP)
      }
    }

    READ_MW_RESP.whenIsActive {
      io.MainMreq.Response.ready := True

      when(io.MainMreq.Response.fire) {
        fillMwKlassLen(io.MainMreq.Response.payload.ResponseData, main_data)
        goto(SEND)
      }
    }

    SEND.whenIsActive {
      val isOop = main_data.oopType === U(NotArrayOop)
      val dispatchMarkWord = resolveForwardMark(main_data.fromObj, main_data.markWord)

      when(isOop) {
        driveProcessUnit(io.Fetch2OopProcess, main_data, dispatchMarkWord)
      }.otherwise {
        driveProcessUnit(io.Fetch2ArrayProcess, main_data, dispatchMarkWord)
      }

      val unitFire = Mux(
        isOop,
        io.Fetch2OopProcess.cmd.fire,
        io.Fetch2ArrayProcess.cmd.fire
      )

      when(unitFire) {
        goto(WAIT_DONE)

        dbg(Seq("Dispatch Task=", main_data.task, " OopType=", main_data.oopType, " SrcOopPtr=", main_data.fromObj, " MarkWord=", dispatchMarkWord, " KlassPtr=", main_data.klassPtr, " success!"))
      }
    }

    WAIT_DONE.whenIsActive {
      when(targetDone || targetDoneSeen) {
        targetDoneSeen := False
        goto(IDLE)

        dbg(Seq("Task=", main_data.task, " done"))
      }
    }

    always {
      when(mainGotoSend) {
        goto(SEND)
      }.elsewhen(mainGotoReadOop) {
        goto(READ_OOP_REQ)
      }
    }
  }

  // ============================================================================
  // Push StateMachine — 处理 GCTrace 实时推送的任务
  //
  // 与 mainFsm 的交互：
  //   — mainFsm 空闲时：直接将 Trace2Fetch 任务接管到 main_data，
  //     通过 mainGotoReadOop 旁路信号让 mainFsm 跳转到 READ_OOP_REQ
  //   — mainFsm 忙碌时：在 push_data 中独立完成 OOP+MW 读取，
  //     完成后将上下文迁移到 main_data（等待 mainFsm 空闲）
  //
  // 状态：IDLE → READ_OOP_REQ → READ_OOP_RESP → READ_MW_REQ
  //                                            → READ_MW_RESP → SEND
  // 处理流程与 mainFsm 的普通对象路径完全对等，只是使用独立的 pushFsm 资源。
  // ============================================================================
  val pushFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State
    val READ_OOP_RESP = new State
    val READ_MW_REQ   = new State
    val READ_MW_RESP  = new State
    val SEND          = new State

    IDLE.whenIsActive {
      io.Trace2Fetch.ready := True

      when(io.Trace2Fetch.fire) {
        val payload = io.Trace2Fetch.payload

        when(mainIsIdle) {
          receiveTask(payload, main_data)
          main_data.fromObj := U(0)
          mainGotoReadOop   := True

        }.otherwise {
          receiveTask(payload, push_data)
          push_data.fromObj := U(0)
          goto(READ_OOP_REQ)
        }
      }
    }

    READ_OOP_REQ.whenIsActive {
      when(push_data.oopType === U(PartialArrayOop)) {
        push_data.fromObj := push_data.task

        goto(READ_MW_REQ)

      }.otherwise {
        driveCopyFwd(io.CopyFwdPush, push_data.task, oopReadSize, push_data.oopCopyMeta)

        val reqMask = requestedByteMask(oopReadSize)
        val fullFwd = (io.CopyFwdPush.mask & reqMask) === reqMask

        when(!io.CopyFwdPush.stall) {
          when(fullFwd) {
            val newFromObj = decodeReadOopResp(io.CopyFwdPush.data)

            push_data.fromObj := newFromObj

            goto(READ_MW_REQ)

          }.otherwise {
            driveReadReq(io.PushMreq, push_data.task, oopReadSize)

            when(io.PushMreq.Request.fire) {
              pushFwdMask := io.CopyFwdPush.mask
              pushFwdData := io.CopyFwdPush.data
              goto(READ_OOP_RESP)
            }
          }
        }
      }
    }

    READ_OOP_RESP.whenIsActive {
      io.PushMreq.Response.ready := True

      when(io.PushMreq.Response.fire) {
        val rd = mergeCopyForward(io.PushMreq.Response.payload.ResponseData, pushFwdMask, pushFwdData)
        val newFromObj = decodeReadOopResp(rd)

        push_data.fromObj := newFromObj

        goto(READ_MW_REQ)
      }
    }

    READ_MW_REQ.whenIsActive {
      driveReadReq(io.PushMreq, push_data.fromObj, mwReadSize)

      when(io.PushMreq.Request.fire) {
        goto(READ_MW_RESP)
      }
    }

    READ_MW_RESP.whenIsActive {
      io.PushMreq.Response.ready := True

      when(io.PushMreq.Response.fire) {
        val rd = io.PushMreq.Response.payload.ResponseData

        when(mainIsIdle) {
          copyFetchContextWithoutMw(main_data, push_data)
          fillMwKlassLen(rd, main_data)

          mainGotoSend := True
          goto(IDLE)

        }.otherwise {
          fillMwKlassLen(rd,push_data)
          goto(SEND)
        }
      }
    }

    SEND.whenIsActive {
      when(mainIsIdle) {
        // No field is overwritten in this branch, so the whole-bundle copy
        // remains safe here.
        main_data    := push_data
        mainGotoSend := True
        goto(IDLE)
      }
    }
  }

  // ============================================================================
  // PreFetch StateMachine — 预取任务并提前完成 OOP+MW 读取
  //
  // 两种工作模式：
  //
  // 1. Normal 模式（PushCount==0 且 pushFollowRem==0）：
  //    从 TaskStack 的 PrePop 端口获取未来任务，完成读取后写入 preBuf[buf_top]，
  //    同时 buf_top 递增、buf_count 递增。
  //
  // 2. Push-follow 模式（PushCount!=0 或 pushFollowRem!=0）：
  //    TaskStack 刚完成一批 Push burst，这些新任务需要优先被 Fetch 消费。
  //    此时 preFsm 将新任务插入到 buf_bottom 之前（即环形缓冲的消费端），
  //    覆盖掉最旧的预取条目。如果缓冲区满了，会丢弃最旧的条目（buf_top 回退）。
  //
  //    子模式 2a: stackPushFollowActive（PushCount ≠ 0）
  //       — 这是 Push burst 后的第一个 PrePop 周期
  //       — pushFollowRem = PushCount - 1（记录还剩多少任务需要 PrePop）
  //       — 将新任务插入到 buf_bottom - PushCount 的位置
  //       — 覆盖掉超出缓冲区容量的旧条目
  //
  //    子模式 2b: fetchPushFollowActive（pushFollowRem ≠ 0）
  //       — 后续的 PrePop 周期
  //       — pushFollowRem 递减
  //       — 新任务追加到 buf_work 的下一个位置
  //
  // 与 mainFsm 的交互：
  //   — 如果 mainFsm 处于 waitForPrefetch 状态，且 preFsm 正在处理
  //     buf_bottom 对应的条目，完成时会直接将结果交给 main_data 并触发
  //     mainGotoSend，避免不必要的缓冲往返。
  // ============================================================================
  val preFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State
    val READ_OOP_RESP = new State
    val READ_MW_REQ   = new State
    val READ_MW_RESP  = new State

    IDLE.whenIsActive {
      val stackPushFollowActive = io.toFetch.PushCount =/= U(0, 32 bits)
      val fetchPushFollowActive = pushFollowRem =/= U(0, 32 bits)

      when(!stackPushFollowActive && !fetchPushFollowActive) {
        io.toFetch.PrePop.ready := !waitForPrefetch && buf_free =/= U(0, buf_free.getWidth bits)

        when(io.toFetch.PrePop.fire) {
          buf_work := buf_top
          resetSlot(buf_top)

          buf_top := bufInc(buf_top, U(1, PreFetchBufferWidth bits))
          buf_count := buf_count + U(1, buf_count.getWidth bits)

          receiveTask(io.toFetch.PrePop.payload, preBuf(buf_top))

          goto(READ_OOP_REQ)
        }

      }.otherwise {
        io.toFetch.PrePop.ready := !waitForPrefetch

        when(io.toFetch.PrePop.fire) {
          when(!stackPushFollowActive) {
            pushFollowRem := pushFollowRem - U(1, 32 bits)

            val idx = bufInc(buf_work, U(1, PreFetchBufferWidth bits))

            buf_work := idx
            resetSlot(idx)

            buf_count := buf_count + U(1, buf_count.getWidth bits)

            receiveTask(io.toFetch.PrePop.payload, preBuf(idx))

            goto(READ_OOP_REQ)

          }.otherwise {
            val pushCount = Mux(
              io.toFetch.PushCount > U(PreFetchBufferNum, 32 bits),
              U(PreFetchBufferNum, 32 bits),
              io.toFetch.PushCount
            )

            val pushCountSmall = pushCount.resize(buf_count.getWidth)

            val idx = bufDec(buf_bottom, pushCountSmall)

            when(buf_free >= pushCountSmall) {
              buf_count := buf_count + U(1, buf_count.getWidth bits)

            }.otherwise {
              val dropNum = pushCountSmall - buf_free
              buf_top := bufDec(buf_top, dropNum)
              buf_count := (buf_count + buf_free - pushCountSmall + U(1, buf_count.getWidth bits)).resized
            }

            buf_work      := idx
            buf_bottom    := idx
            pushFollowRem := pushCount - U(1, 32 bits)

            resetSlot(idx)

            receiveTask(io.toFetch.PrePop.payload,preBuf(idx))

            goto(READ_OOP_REQ)
          }
        }
      }
    }

    READ_OOP_REQ.whenIsActive {
      when(preBuf(buf_work).oopType === U(PartialArrayOop)) {
        preBuf(buf_work).fromObj := preBuf(buf_work).task

        goto(READ_MW_REQ)

      }.otherwise {
        driveCopyFwd(io.CopyFwdPre, preBuf(buf_work).task, oopReadSize, preBuf(buf_work).oopCopyMeta)

        val reqMask = requestedByteMask(oopReadSize)
        val fullFwd = (io.CopyFwdPre.mask & reqMask) === reqMask

        when(!io.CopyFwdPre.stall) {
          when(fullFwd) {
            val newFromObj = decodeReadOopResp(io.CopyFwdPre.data)

            preBuf(buf_work).fromObj := newFromObj

            goto(READ_MW_REQ)

          }.otherwise {
            driveReadReq(io.PreMreq, preBuf(buf_work).task, oopReadSize)

            when(io.PreMreq.Request.fire) {
              preFwdMask := io.CopyFwdPre.mask
              preFwdData := io.CopyFwdPre.data
              goto(READ_OOP_RESP)
            }
          }
        }
      }
    }

    READ_OOP_RESP.whenIsActive {
      io.PreMreq.Response.ready := True

      when(io.PreMreq.Response.fire) {
        val rd = mergeCopyForward(io.PreMreq.Response.payload.ResponseData, preFwdMask, preFwdData)
        val newFromObj = decodeReadOopResp(rd)

        preBuf(buf_work).fromObj := newFromObj

        goto(READ_MW_REQ)
      }
    }

    READ_MW_REQ.whenIsActive {
      driveReadReq(io.PreMreq, preBuf(buf_work).fromObj, mwReadSize)

      when(io.PreMreq.Request.fire) {
        goto(READ_MW_RESP)
      }
    }

    READ_MW_RESP.whenIsActive {
      io.PreMreq.Response.ready := True

      when(io.PreMreq.Response.fire) {
        val rd = io.PreMreq.Response.payload.ResponseData
        val currentFromObj = preBuf(buf_work).fromObj

        val finalMw = resolveForwardMark(currentFromObj, rd(GCElementWidth - 1 downto 0))

        when(waitForPrefetch && mainIsIdle && buf_work === buf_bottom) {
          waitForPrefetch := False

          copyFetchContextWithoutMw(main_data, preBuf(buf_bottom))
          main_data.markWord := finalMw
          fillKlassLen(rd, main_data)

          mainGotoSend := True

          resetSlot(buf_bottom)
          buf_bottom := bufInc(buf_bottom, U(1, PreFetchBufferWidth bits))
          buf_count := buf_count - U(1, buf_count.getWidth bits)

        }.otherwise {
          preBuf(buf_work).markWord := finalMw

          fillKlassLen(rd, preBuf(buf_work))

          preBufDone(buf_work) := True
        }

        goto(IDLE)
      }
    }
  }

  // 转发通知修补已固化的 MarkWord
  // 此代码块放在所有 FSM 之后，以确保同周期的陈旧 MMU 响应不会覆盖Copy2Survivor 的转发通知。resolveForwardMark() 也在 SEND 中调用，
  // 覆盖了与分发并发到达的转发通知
  when(incomingFwdValid) {
    when(main_data.fromObj === incomingFwdObj) {
      main_data.markWord := incomingFwdValue
    }

    when(push_data.fromObj === incomingFwdObj) {
      push_data.markWord := incomingFwdValue
    }

    for (i <- 0 until PreFetchBufferNum) {
      when(preBufDone(i) && preBuf(i).fromObj === incomingFwdObj) {
        preBuf(i).markWord := incomingFwdValue
      }
    }
  }

  mainIsIdle := mainFsm.isActive(mainFsm.IDLE)
  mainIsWaitDone := mainFsm.isActive(mainFsm.WAIT_DONE)
  pushIsIdle := pushFsm.isActive(pushFsm.IDLE)
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(
    new GCFetch()
  )
}