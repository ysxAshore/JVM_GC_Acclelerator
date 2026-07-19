package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/**
 * GC 任务栈
 *
 * 模块使用片上环形栈保存活跃任务；片上任务过多时从栈底 SpillOut 到主存队列JVM queue，
 * 片上任务不足时再从主存队列JVM queue中 ReadBack。由于主存储 stack_data 是同步读 RAM，
 * 模块在栈顶设置 TopCache，为 Pop 和 PrePop 提供低延迟数据
 *
 * 数据方向：
 *   Push -> 片上栈顶；Pop/PrePop <- TopCache；
 *   SpillOut: 片上栈底 -> JVM queue；ReadBack: JVM queue -> 片上栈底。
 */
class GCTaskStack extends Module with GCTopParameters with GCParameters with HWParameters {
  val io = new Bundle {
    val toFetch        = master(new GCToFetch) // Pop and PrePop
    val toStack        = slave(new GCToStack)  // Push exclude lastPush
    val Mreq           = master(new LocalMMUIO)
    val ConfigIO       = slave(new GCTaskStackConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True
  io.ConfigIO.Done := False
  io.ConfigIO.config.ready := False

  // queue_bottom 使用软件/JVM 队列的逻辑索引；stack 指针使用片上环形栈索引
  val queuePtrWidth = 32
  val stackPtrWidth = log2Up(GCTaskStack_Entry)

  // stack_top 指向片上栈顶；stack_bottom 指向片上栈底边界
  // 两者相等表示片上栈为空，并牺牲一个槽位区分 full/empty
  val stack_top    = RegInit(U(0, stackPtrWidth bits))
  val stack_bottom = RegInit(U(0, stackPtrWidth bits))

  // JVM queue 元素数组基地址及当前有效元素数量/底部逻辑位置
  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom     = RegInit(U(0, queuePtrWidth bits))

  // 片上任务主存储：同步读 RAM. Pop / PrePop 不直接读取它，而是通过 TopCache
  val stack_data = Mem(UInt(GCElementWidth bits), GCTaskStack_Entry)

  // 按物理stack entry保存PrePop标记, 物理位置被新数据覆盖时必须清零
  // 即使任务被挤出TopCache又重新Refill, 该标记仍可避免同一个任务被重复PrePop
  val prefetched = Vec.fill(GCTaskStack_Entry)(RegInit(False))

  // 环形指针辅助函数
  def stkInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskStack_Entry, step)
  def stkDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskStack_Entry, step)
  def queInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def queDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)

  // JVM queue 的每个 GCElement 占 8 字节，将逻辑索引转换成 MMU 字节地址
  def elemAddr(idx: UInt): UInt = (queue_elems_base + (idx.resize(MMUAddrWidth) << 3)).resize(MMUAddrWidth)

  val stk_nextTop = stkInc(stack_top, U(1, stackPtrWidth bits))
  val stk_prevTop = stkDec(stack_top, U(1, stackPtrWidth bits))

  // 环形栈容量统计。保留一个槽位，因此最大可用数为 Entry-1
  val task_empty = stack_top === stack_bottom // 硬件栈队列空
  val task_usage = (stack_top - stack_bottom).resize(stackPtrWidth + 1) // 硬件栈已用项数
  val task_free  = U(GCTaskStack_Entry - 1, stackPtrWidth + 1 bits) - task_usage // 牺牲一个槽判断满

  // 使用迟滞阈值避免 SpillOut 和 ReadBack 在边界附近来回切换
  val need_spillOut = task_usage >= U(GCTaskStack_SpillNeed + 4, task_usage.getWidth bits)
  val need_readback = (task_usage <= U(GCTaskStack_ReadNeed - 4, task_usage.getWidth bits)) && (queue_bottom =/= U(0, queuePtrWidth bits))

  // TopCache：同步 RAM 上方的栈顶缓存
  // offset 0 对应 stack_top；offset 1 对应 stack_top-1，依次向 stack_bottom 延伸。
  // topCacheIdx 保存每个缓存项对应的物理 stack_data 索引；有效项范围为
  // [0, topCacheCount)。TopCache 容量取扫描窗口的两倍，以隐藏 refill 延迟。
  val TopCacheDepth = PreFetchScanWindow << 1
  val topCacheCountWidth  = log2Up(TopCacheDepth + 1)
  val topCacheOffsetWidth = log2Up(TopCacheDepth)

  val topCacheData       = Vec.fill(TopCacheDepth)(Reg(UInt(GCElementWidth bits)))
  val topCacheIdx        = Vec.fill(TopCacheDepth)(Reg(UInt(stackPtrWidth bits)))
  val topCachePrefetched = Vec.fill(TopCacheDepth)(RegInit(False))
  val topCacheCount      = RegInit(U(0, topCacheCountWidth bits))

  def cacheOffsetValid(offset: UInt): Bool = offset.resize(topCacheCountWidth) < topCacheCount

  val topCacheEmpty = topCacheCount === U(0, topCacheCountWidth bits)
  val topCacheFull  = topCacheCount === U(TopCacheDepth, topCacheCountWidth bits)

  // Push-follow PrePop：Push burst 结束后，优先把刚 Push 的任务提供给 Fetch
  //   而普通PrePop不检查offset0 但是刚 Push 完时，希望 Fetch 尽快看到新任务，
  //   尤其是最新 Push 的栈顶任务。因此 Push-follow 模式会特殊地从 offset 0 开始
  // push_count: 记录最近一轮 Push burst 中积累的任务数量
  // not_prefetched: 仍处于Push-follow PrePop中 禁止普通PrePop
  // pushPrePopRem: 记录第一次 Push-follow PrePop 完成后，还有多少个任务需要继续 PrePop
  // pushPrePopOffset: 记录后续 Push-follow PrePop 从哪个 TopCache offset 读取
  //   第一次固定读取offset0 之后从1递增
  val push_count       = RegInit(U(0, 32 bits))
  val not_prefetch     = RegInit(False)
  val pushPrePopRem    = RegInit(U(0, 32 bits))
  val pushPrePopOffset = RegInit(U(1, topCacheOffsetWidth bits))

  // 状态机状态可见信号
  val inWork = Bool()

  // Pop / Push 接口
  val pushCanAccept = inWork && task_free =/= U(0, task_free.getWidth bits) // 状态机在Work状态 且 stack_data 没有满
  io.toStack.Push.ready := pushCanAccept

  // push-follow PrePop 未处理完时禁止普通 Pop，避免新任务的观察次序混乱(Pop会让offset TopCache左移)
  val popBlockedByPushFollow = (push_count =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))
  val popAvailable = !topCacheEmpty // TopCache 不空 即可做Pop操作
  io.toFetch.Pop.valid := inWork && popAvailable && !popBlockedByPushFollow
  io.toFetch.Pop.payload := topCacheData(0) // 每次取bias 0 数据

  val pushFire = io.toStack.Push.fire
  val popFire  = io.toFetch.Pop.fire

  // 若本周期 Pop 消费了一个刚 Push 的任务，Fetch 看到的 PushCount 同拍减一
  val pushCountForFetch = Mux(popFire && push_count =/= U(0, 32 bits), push_count - U(1, 32 bits), push_count)
  io.toFetch.PushCount := pushCountForFetch

  // PrePop 候选选择
  // 普通模式扫描 offset 1..PreFetchScanWindow，offset 0 留给 Pop 选择离栈顶最近且从未 PrePop 的项
  // push-follow模式 按照指定的offset依次选择刚Push的任务
  val normalCandidates    = Vec(Bool(), PreFetchScanWindow) // 表示第i个候选是否有效
  val normalCandidateOffs = Vec(UInt(topCacheOffsetWidth bits), PreFetchScanWindow) // 第i个候选对应的TopCache offset
  for (i <- 1 until PreFetchScanWindow + 1) { // 在 [1, PreFetchScanWindow] 区间内扫描
    val off = U(i, topCacheOffsetWidth bits)
    // topCache entry 有效 且 没有被 PrePop过(topCache and hardware_stack)
    normalCandidates(i - 1) := cacheOffsetValid(off) && !topCachePrefetched(off) && !prefetched(topCacheIdx(off))
    normalCandidateOffs(i - 1) := off
  }
  val normalFirstOH = OHMasking.first(normalCandidates.asBits) // 从小序开始选
  val normalPrePopOffset = MuxOH(normalFirstOH, normalCandidateOffs)

  // 只要存在刚 Push 的任务，或者 Push-follow 尚未完成，就进入 Push-follow 模式
  //    第一次Push-follow PrePop pushCountForFetch != 0, offset 取 0 (完成后push_count 会 清 0)
  //    后续的Push-follow PrePop pushPrePopRem != 0, offset 取 pushPrePopoffset
  val pushFollowPrePopMode = (pushCountForFetch =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))
  val pushFollowOffset = Mux(pushCountForFetch =/= U(0, 32 bits), U(0, topCacheOffsetWidth bits), pushPrePopOffset)
  val prefetchOffset = Mux(pushFollowPrePopMode, pushFollowOffset, normalPrePopOffset)

  val prefetchHit = Mux(pushFollowPrePopMode, cacheOffsetValid(pushFollowOffset), normalFirstOH.orR)

  // Push/Pop 会移动缓存项，禁止与 PrePop 同拍，避免给移动前后错误的项打标记
  val prefetchBlockedByStackMove = pushFire || popFire
  io.toFetch.PrePop.valid := inWork && prefetchHit && !not_prefetch && !prefetchBlockedByStackMove
  io.toFetch.PrePop.payload := topCacheData(prefetchOffset)
  val preFire = io.toFetch.PrePop.fire

  // TopCache Refill: 当 TopCache 没满时，从同步 RAM stack_data 中读取更深的栈元素，并按顺序追加到 TopCache 尾部
  // 由于stack_data.readSync数据在下一拍才能返回, 所以refill是单级流水, 可以做到连续每周期补充一个缓存项
  //    第N拍发送refill请求 第N+1拍得到refill response并尝试加到TopCache
  val refillReq       = Bool() // 本周期是否向 stack_data 发起同步读
  val refillReqIdx    = UInt(stackPtrWidth bits) // 本周期读取的stack data索引
  val refillRespIdx   = Reg(UInt(stackPtrWidth bits)) // 本周期返回的 refillData 是否有效, 在refillReq有效时 锁存一拍refillReqIdx 这样就知道refilldata对应什么
  val refillRespValid = RegInit(False) // 本周期返回数据对应的物理栈索引

  val refillData = stack_data.readSync(refillReqIdx, refillReq) // 第二个参数是使能信号

  // 发起新的 refill request
  // 栈中有更多的数据 避免读到stack_bottom之外
  //
  // Push 同拍不发新的 refill：
  //   因为 Push 会更新 stack_top 和 TopCache 排列。
  //   下一拍再基于新的 stack_top/topCacheCount 重新计算 refill 地址。
  //
  // TopCache 满时，只有 Pop 同拍才允许预读
  // 若本拍会 append 已返回的数据，新 refill 必须再向深处多读一项，避免重复读取
  when(refillReq) {
    refillRespIdx := refillReqIdx
  }

  // readSync response valid 延迟一拍
  refillRespValid := refillReq

  val refillAppendAllowed = refillRespValid && !pushFire && (popFire || !topCacheFull) // 和refillReq有效要求差不多 都需要禁止Push改变TopCache关系 topCache不能满或者满时同周期有Pop
  val refillAppendData = refillData
  val refillAppendIdx  = refillRespIdx
  val refillAppendPrefetched = prefetched(refillRespIdx)
  val refillAppendWillHappen = refillAppendAllowed

  // refillOffset = topCacheCount + 本周期是否会 append(如果有append 会占据topCacheCount这个位置 需要往深读一个)
  val refillOffsetWide = topCacheCount.resize(stackPtrWidth + 1) + refillAppendWillHappen.asUInt.resize(stackPtrWidth + 1)
  // TopCache中的offset和stack_data对应关系是
  //   offset0 --- stack_top offset1 --- stack_top - 1 offset2 --- stack_top - 2
  // 因此refillReqIdx = stack_top - refillOffsetWide
  refillReqIdx := stkDec(stack_top, refillOffsetWide.resize(stackPtrWidth))
  val refillHasMoreData = task_usage > refillOffsetWide.resize(task_usage.getWidth)
  refillReq := inWork && !pushFire && refillHasMoreData && (popFire || !topCacheFull)

  // Push 发生时，上一拍发出的 refill response 已经不可信 直接清掉，避免旧 response 在之后被错误 append。
  when(pushFire) {
    refillRespValid := False
  }

  def dbg(msg: Seq[Any]): Unit = {
    if (DebugEnable) {
      report(Seq("[GCTaskStack<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  }

  // Fast path handling
  //
  // 这里统一维护：
  //   1. push_count / push-follow 状态
  //   2. TopCache shift / replace / refill append
  //   3. stack_top 更新
  def handleFastPath(): Unit = {
    // PrePop 标记
    when(preFire) {
      topCachePrefetched(prefetchOffset) := True
      prefetched(topCacheIdx(prefetchOffset)) := True

      dbg(Seq("PrePop from TopCache, offset=", prefetchOffset,
        " index=", topCacheIdx(prefetchOffset),
        " data=", topCacheData(prefetchOffset)
      ))
    }

    // push_count 从 0 开始, 每一次Push Fire都会增加 但是每一次Pop都会减少
    val pushCountAfterPush = push_count + pushFire.asUInt.resize(32)
    val pushCountAfterPop = Mux(
      popFire && push_count =/= U(0, 32 bits),
      pushCountAfterPush - U(1, 32 bits),
      pushCountAfterPush
    )

    push_count := pushCountAfterPop

    // Push 时 禁止PrePop
    when(pushFire) {
      not_prefetch := True
    }

    // LastPush 表示Push已完 可以重新PrePop
    when(io.toStack.LastPush) {
      not_prefetch := False
    }

    when(preFire) {
      // 第一次Push-follow PrePop
      when(pushCountForFetch =/= U(0, 32 bits)) {
        val takeNum = Mux(
          pushCountForFetch > U(PreFetchBufferNum, 32 bits),
          U(PreFetchBufferNum, 32 bits),
          pushCountForFetch
        )

        push_count       := U(0, 32 bits)
        pushPrePopRem    := takeNum - U(1, 32 bits)
        pushPrePopOffset := U(1, topCacheOffsetWidth bits)

      }.elsewhen(pushPrePopRem =/= U(0, 32 bits)) {
        pushPrePopRem    := pushPrePopRem - U(1, 32 bits)
        pushPrePopOffset := pushPrePopOffset + U(1, topCacheOffsetWidth bits)
      }
    }

    // TopCache update
    when(pushFire && popFire) {
      // Push + Pop 同拍： Pop 返回旧 topCacheData(0) Push 写入当前 stack_top 位置，成为新的 top
      // stack_top 不变，topCacheCount 不变
      stack_data.write(stack_top, io.toStack.Push.payload)

      topCacheData(0)       := io.toStack.Push.payload
      topCacheIdx(0)        := stack_top
      topCachePrefetched(0) := False
      prefetched(stack_top) := False

      dbg(Seq("Push+Pop replace top, index=", stack_top, " push=", io.toStack.Push.payload, " pop=", topCacheData(0)))

    }.elsewhen(pushFire) {
      // Push only： 写入 stack_top + 1 TopCache 整体右移，新数据插入 offset 0
      // 如果 TopCache 已满，最后一个 entry 会被挤出 PushFire时不存在TopCacheRefill
      val pushIndex = stk_nextTop
      stack_data.write(pushIndex, io.toStack.Push.payload)

      for (i <- TopCacheDepth - 1 downto 1) {
        topCacheData(i)       := topCacheData(i - 1)
        topCacheIdx(i)        := topCacheIdx(i - 1)
        topCachePrefetched(i) := topCachePrefetched(i - 1)
      }

      topCacheData(0)       := io.toStack.Push.payload
      topCacheIdx(0)        := pushIndex
      topCachePrefetched(0) := False
      prefetched(pushIndex) := False

      when(!topCacheFull) {
        topCacheCount := topCacheCount + U(1, topCacheCountWidth bits)
      }

      stack_top := pushIndex

      dbg(Seq("Push TopCache, index=", pushIndex, " data=", io.toStack.Push.payload))

    }.elsewhen(popFire) {
      // Pop only： 消费 offset 0 TopCache 左移
      // 如果本周期刚好有 refill response 可 append，则插到尾部
      // 没有 refill response 时，topCacheCount 减 1
      for (i <- 0 until TopCacheDepth - 1) {
        topCacheData(i)       := topCacheData(i + 1)
        topCacheIdx(i)        := topCacheIdx(i + 1)
        topCachePrefetched(i) := topCachePrefetched(i + 1)
      }

      topCachePrefetched(TopCacheDepth - 1) := False

      when(refillAppendAllowed) {
        val insertOff = (topCacheCount - U(1, topCacheCountWidth bits)).resize(topCacheOffsetWidth)

        topCacheData(insertOff)       := refillAppendData
        topCacheIdx(insertOff)        := refillAppendIdx
        topCachePrefetched(insertOff) := refillAppendPrefetched

        // Pop 减 1，refill append 加 1，净效果 count 不变
        topCacheCount := topCacheCount

        dbg(Seq("Pop with refill append, popIndex=", stack_top, " appendIndex=", refillAppendIdx, " appendData=", refillAppendData))

      }.otherwise {
        when(!topCacheEmpty) {
          topCacheCount := topCacheCount - U(1, topCacheCountWidth bits)
        }
        dbg(Seq("Pop TopCache, index=", stack_top, " data=", topCacheData(0)))
      }

      stack_top := stk_prevTop
    }.otherwise {
      // 无 Push / Pop： 如果有 refill response，并且 TopCache 未满，则 append 到尾部。
      // 如果 TopCache 已满，response 会被丢弃，不会 hold 这是为了避免旧 response 在 Push 后错序插入
      when(refillAppendAllowed) {
        val insertOff = topCacheCount.resize(topCacheOffsetWidth)

        topCacheData(insertOff)       := refillAppendData
        topCacheIdx(insertOff)        := refillAppendIdx
        topCachePrefetched(insertOff) := refillAppendPrefetched
        topCacheCount                 := topCacheCount + U(1, topCacheCountWidth bits)

        dbg(Seq("TopCache refill append, offset=", insertOff, " index=", refillAppendIdx, " data=", refillAppendData))
      }
    }
  }

  // SpillOut Area: 当片上 stack_data 太满时，从 bottom 端搬数据到 JVM queue。
  // 这里也使用 readSync：
  //   readReq      发起同步读
  //   readPending  下一拍拿到数据并打包
  //   dataValid    发 MMU write request
  val spillOutArea = new Area {
    val issued      = RegInit(False)
    val readPending = RegInit(False)
    val dataValid   = RegInit(False)

    val addrBuf        = Reg(UInt(MMUAddrWidth bits))
    val reqNumBuf      = Reg(UInt(3 bits))
    val stackBottomBuf = Reg(UInt(stackPtrWidth bits))
    val queueBottomBuf = Reg(UInt(queuePtrWidth bits))
    val packDataBuf    = Reg(UInt(MMUDataWidth bits))

    def busy: Bool = issued || readPending || dataValid

    def run(): Unit = {
      val addr            = elemAddr(queue_bottom)
      val emsPerLine      = MMUDataWidth / GCElementWidth
      val offsetInLine    = addr(log2Up(LineBytesNum) - 1 downto 0) >> 3
      val remainingInLine = emsPerLine - offsetInLine
      val reqNum          = remainingInLine

      val spillPtrs = Vec((0 until 4).map(i => stkInc(stack_bottom, U(i + 1, stackPtrWidth bits))))
      val readReq = !issued && !readPending && !dataValid

      val spillData = Vec(
        (0 until 4).map(i => stack_data.readSync(spillPtrs(i), readReq))
      )

      when(readReq) {
        readPending    := True
        addrBuf        := addr
        reqNumBuf      := reqNum.resized
        stackBottomBuf := stack_bottom
        queueBottomBuf := queue_bottom
      }

      when(readPending) {
        readPending := False
        dataValid   := True
        packDataBuf := Cat(spillData.reverse).asUInt.resize(MMUDataWidth)
      }

      when(dataValid) {
        issueReq(io.Mreq, addrBuf, True, (reqNumBuf << 3).resize(LineBytesNumBitSize), packDataBuf, False, False, issued) { _ => }
      }

      when(issued) {
        issued    := False
        dataValid := False

        val newQueueBottom = queInc(queueBottomBuf, reqNumBuf.resize(queuePtrWidth))

        stack_bottom := stkInc(stackBottomBuf, reqNumBuf.resized)
        queue_bottom := newQueueBottom

        dbg(Seq("SpillOut, moveNum=", reqNumBuf, " old queue_bottom=", queueBottomBuf, " new queue_bottom=", newQueueBottom))
      }
    }

    def clear(): Unit = {
      issued      := False
      readPending := False
      dataValid   := False
    }
  }

  // ReadBack Area
  // 当片上 stack_data 太空，并且 JVM queue 中还有任务时， 从 JVM queue 搬一批任务回 stack_data 的 bottom 端
  val readBackArea = new Area {
    val issued = RegInit(False)

    def run(): Unit = {
      val wantNum = U(4, queue_bottom.getWidth bits)
      val queueAvail = queue_bottom
      val queueBottomElements = elemAddr(queue_bottom)(log2Up(LineBytesNum) - 1 downto 0) >> 3
      val reqNumTemp = Mux(wantNum >= queueAvail, queue_bottom, wantNum)

      val reqNum = Mux(
        reqNumTemp >= queueBottomElements && queueBottomElements =/= 0,
        queueBottomElements,
        reqNumTemp
      )

      // 保留 1 个空位，避免环形栈 full / empty 无法区分。
      val freeForReadback = Mux(
        task_free > U(1, task_free.getWidth bits),
        task_free - U(1, task_free.getWidth bits),
        U(0, task_free.getWidth bits)
      )

      val canReceive = Mux(
        freeForReadback >= reqNum.resize(task_free.getWidth),
        reqNum.resize(task_free.getWidth),
        freeForReadback
      )

      val readIndex = queDec(queue_bottom, reqNum)
      val readAddr  = elemAddr(readIndex)

      issueReq(io.Mreq, readAddr, False, reqNum << 3, U(0), True, False, issued) { rd =>
        val elems = rd.subdivideIn(GCElementWidth bits)
        val newQueueBottom = queDec(queue_bottom, canReceive)

        for (i <- 0 until 4) {
          when(i < canReceive) {
            val writeElement = elems((reqNum - 1 - i).resized)
            val wrPtr = stkDec(stack_bottom, U(i, stackPtrWidth bits))

            stack_data.write(wrPtr, writeElement)
            prefetched(wrPtr) := False
          }
        }

        stack_bottom := stkDec(stack_bottom, canReceive)
        queue_bottom := newQueueBottom

        dbg(Seq(
          "ReadBack, reqNum=", reqNum,
          " receive=", canReceive,
          " old queue_bottom=", queue_bottom,
          " new queue_bottom=", newQueueBottom
        ))
      }
    }

    def clear(): Unit = {
      issued := False
    }
  }

  // Task exhausted
  // 注意 TopCache / refill response 也要算进去。 否则可能 stack_top == stack_bottom 时提前结束。
  val task_exhausted = task_empty && queue_bottom === U(0) && topCacheCount === U(0) &&
    !refillRespValid && push_count === U(0) && pushPrePopRem === U(0)

  // FSM
  val fsm = new StateMachine {
    val IDLE: State         = new State with EntryPoint
    val WORK: State         = new State

    inWork := isActive(WORK)

    always {
      when(isEntering(WORK) && isExiting(IDLE)) {
        queue_bottom     := io.ConfigIO.config.payload.TaskQueue_Bottom
        queue_elems_base := io.ConfigIO.config.payload.TaskQueue_ElemsBase.resize(MMUAddrWidth)

        dbg(Seq(
          "Config JVM Queue, Bottom=", io.ConfigIO.config.payload.TaskQueue_Bottom,
          " ElemsBase=", io.ConfigIO.config.payload.TaskQueue_ElemsBase
        ))
      }

      when(isEntering(IDLE) && isExiting(WORK)) {
        io.ConfigIO.Done := True
      }
    }

    IDLE.whenIsActive {
      io.ConfigIO.config.ready := True

      for (i <- 0 until GCTaskStack_Entry) {
        prefetched(i) := False
      }

      for (i <- 0 until TopCacheDepth) {
        topCachePrefetched(i) := False
      }

      stack_top    := U(0, stackPtrWidth bits)
      stack_bottom := U(0, stackPtrWidth bits)
      queue_bottom := U(0, queuePtrWidth bits)

      topCacheCount := U(0, topCacheCountWidth bits)

      push_count       := U(0, 32 bits)
      not_prefetch     := False
      pushPrePopRem    := U(0, 32 bits)
      pushPrePopOffset := U(1, topCacheOffsetWidth bits)

      refillRespValid := False

      spillOutArea.clear()
      readBackArea.clear()

      when(io.ConfigIO.config.fire) {
        goto(WORK)
      }
    }

    WORK.whenIsActive {
      when(!task_exhausted || !io.toFetch.Pop.ready) {
        handleFastPath()

        when(need_spillOut || spillOutArea.busy) {
          spillOutArea.run()
        }.elsewhen(need_readback) {
          readBackArea.run()
        }
      }

      // Fetch Module idle and taskStack localBot == 0
      when(task_exhausted && io.toFetch.Pop.ready) {
        goto(IDLE)
      }
    }
  }
}

object GCTaskStackVerilog extends App {
  Config.spinal.generateVerilog(new GCTaskStack())
}