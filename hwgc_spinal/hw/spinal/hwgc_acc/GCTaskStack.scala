package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/* GCTaskStack
 *
 * readSync + TopCache version
 *
 * 设计目标：
 *   1. stack_data 使用同步读，利于 BRAM / SRAM 推断和时序收敛。
 *   2. Pop / PrePop 不直接等待 readSync，而是从栈顶寄存器缓存 TopCache 中返回。
 *   3. readSync 在后台 refill TopCache，把同步读 1 拍延迟隐藏起来。
 *
 * 关键修复：
 *   旧版本带 refillHold，TopCache 满后如果发生 Push，旧 refill response
 *   可能被 hold 住，之后错误 append，导致像 BF10 被 BCE8 替换这种错序。
 *
 *   现在取消 refillHold：
 *     - refill response 当拍能 append 就 append
 *     - 不能 append 就直接丢弃
 *     - Push 发生时清掉 in-flight refill response
 *
 *   这样 Push 改变 TopCache offset 后，不会再插入旧 offset 下读回来的错误数据。
 */
class GCTaskStack extends Module with GCTopParameters with GCParameters with HWParameters {
  val io = new Bundle {
    val toFetch        = master(new GCToFetch)
    val toStack        = slave(new GCToStack)
    val Mreq           = master(new LocalMMUIO)
    val ConfigIO       = slave(new GCTaskStackConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // Default outputs
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ConfigIO.TaskStackDone := False
  io.ConfigIO.TaskReady := False

  // Stack / Queue pointers
  val stackPtrWidth = log2Up(GCTaskStack_Entry)
  val queuePtrWidth = 32

  val stack_top    = RegInit(U(0, stackPtrWidth bits))
  val stack_bottom = RegInit(U(0, stackPtrWidth bits))

  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom     = RegInit(U(0, queuePtrWidth bits))

  // 主存储：同步读 RAM. Pop / PrePop 不直接从这里出，而是通过 TopCache
  val stack_data = Mem(UInt(GCElementWidth bits), GCTaskStack_Entry)

  // 全局 prefetched 标记 表示某个物理 stack entry 是否已经被 PrePop 过
  val prefetched = Vec.fill(GCTaskStack_Entry)(RegInit(False))

  // Helper functions
  def stkInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskStack_Entry, step)
  def stkDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskStack_Entry, step)
  def queInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def queDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def elemAddr(idx: UInt): UInt = (queue_elems_base + (idx.resize(MMUAddrWidth) << 3)).resize(MMUAddrWidth)

  val stk_nextTop = stkInc(stack_top, U(1, stackPtrWidth bits))
  val stk_prevTop = stkDec(stack_top, U(1, stackPtrWidth bits))

  val task_empty = stack_top === stack_bottom // 硬件栈队列空
  val task_usage = (stack_top - stack_bottom).resize(stackPtrWidth + 1) // 硬件栈已用项数
  val task_free  = U(GCTaskStack_Entry - 1, stackPtrWidth + 1 bits) - task_usage // 牺牲一个槽判断满

  val need_spillOut = task_usage >= U(GCTaskStack_SpillNeed + 4, task_usage.getWidth bits)
  val need_readback = (task_usage <= U(GCTaskStack_ReadNeed - 4, task_usage.getWidth bits)) && (queue_bottom =/= U(0, queuePtrWidth bits))

  // TopCache(pop从这里取)
  // offset 0 是stack_top 对应数据，offset 越大越靠近 stack_bottom。
  //    topCacheData(0) = 当前 stack_top 对应的数据
  //    topCacheData(1) = stack_top - 1
  //    topCacheData(2) = stack_top - 2
  // topCacheIdx 用于记录每个 cache entry 对应的物理 stack index。
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

  // Push-follow PrePop bookkeeping
  // push_count: 记录最近 Push 进来的任务数量。
  // pushPrePopRem: 第一次 push-follow PrePop 之后，还剩几个 pushed entry 要继续 PrePop。
  // pushPrePopOffset: 后续 push-follow PrePop 从 topCacheData(offset) 取数据。
  val push_count       = RegInit(U(0, 32 bits))
  val not_prefetch     = RegInit(False)
  val pushPrePopRem    = RegInit(U(0, 32 bits))
  val pushPrePopOffset = RegInit(U(1, topCacheOffsetWidth bits))

  // State visibility
  val inWork = Bool()

  // Pop / Push interface
  // LastPush: LastPush 不是进入 TaskStack 的 payload. 它只是通知 Push burst 结束，可以重新允许 PrePop。
  val pushCanAccept = inWork && task_free =/= U(0, task_free.getWidth bits)
  io.toStack.Push.ready := pushCanAccept

  // pop 被 连续的 几个 push 阻塞
  val popBlockedByPushFollow = (push_count =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))
  val popAvailable = !topCacheEmpty
  io.toFetch.Pop.valid := inWork && popAvailable && !popBlockedByPushFollow
  io.toFetch.Pop.payload := topCacheData(0)

  val pushFire = io.toStack.Push.fire
  val popFire  = io.toFetch.Pop.fire

  // PushCount 给 GCFetch 使用 Fetch 的 Pop.ready 不能组合依赖 PushCount，否则会形成组合环。
  val pushCountForFetch = Mux(popFire && push_count =/= U(0, 32 bits), push_count - U(1, 32 bits), push_count)
  io.toFetch.PushCount := pushCountForFetch

  // PrePop candidate selection from TopCache
  // 普通 PrePop： 扫描 topCacheData(1..PreFetchScanWindow)，不扫描 offset 0. offset 0 留给 Pop
  // push-follow PrePop： 第一次返回 offset 0. 后续返回 offset 1, 2, ...
  val normalCandidates    = Vec(Bool(), PreFetchScanWindow)
  val normalCandidateOffs = Vec(UInt(topCacheOffsetWidth bits), PreFetchScanWindow)

  for (i <- 1 until PreFetchScanWindow + 1) {
    val off = U(i, topCacheOffsetWidth bits)
    normalCandidates(i - 1) := cacheOffsetValid(off) && !topCachePrefetched(off) && !prefetched(topCacheIdx(off))
    normalCandidateOffs(i - 1) := off
  }

  val normalFirstOH = OHMasking.first(normalCandidates.asBits)
  val pushFollowPrePopMode = (pushCountForFetch =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))
  val pushFollowOffset = Mux(pushCountForFetch =/= U(0, 32 bits), U(0, topCacheOffsetWidth bits), pushPrePopOffset)
  val normalPrePopOffset = MuxOH(normalFirstOH, normalCandidateOffs)
  val prefetchOffset = Mux(pushFollowPrePopMode, pushFollowOffset, normalPrePopOffset)

  val prefetchHit = Mux(pushFollowPrePopMode, cacheOffsetValid(pushFollowOffset), normalFirstOH.orR)

  // 为了简化 TopCache shift / 标记逻辑，PrePop 不和 Push/Pop 同拍
  val prefetchBlockedByStackMove = pushFire || popFire

  io.toFetch.PrePop.valid := inWork && prefetchHit && !not_prefetch && !prefetchBlockedByStackMove
  io.toFetch.PrePop.payload := topCacheData(prefetchOffset)
  val preFire = io.toFetch.PrePop.fire

  // TopCache refill using stack_data.readSync
  //   readSync response 如果本周期不能 append，就直接丢弃。
  //   TopCache 满后如果发生 Push，TopCache 会整体右移并挤出 tail。
  //   Push 之前发出的 refill response 是基于旧 stack_top / old offset 算的。
  //   如果把它 hold 住，之后再 append，就可能把 BCE8 这种更深处的数据
  //   错误插入，导致 BF10 被跳过。
  val refillRespValid = RegInit(False)
  val refillRespIdx   = Reg(UInt(stackPtrWidth bits))

  val refillReq    = Bool()
  val refillReqIdx = UInt(stackPtrWidth bits)

  val refillData = stack_data.readSync(refillReqIdx, refillReq)

  // 只有 response 回来并且当拍可以接收时才 append。
  // Push 同拍不 append，因为 Push 会改变 TopCache offset 关系。
  val refillAppendAllowed = refillRespValid && !pushFire && (popFire || !topCacheFull)
  val refillAppendData = refillData
  val refillAppendIdx  = refillRespIdx

  val refillAppendPrefetched = prefetched(refillRespIdx)

  // 如果本周期 append 了一个返回值，那么新的 refill request 要继续往更深处读。
  val refillAppendWillHappen = refillAppendAllowed

  val refillOffsetWide = topCacheCount.resize(stackPtrWidth + 1) + refillAppendWillHappen.asUInt.resize(stackPtrWidth + 1)

  refillReqIdx := stkDec(stack_top, refillOffsetWide.resize(stackPtrWidth))

  val refillHasMoreData = task_usage > refillOffsetWide.resize(task_usage.getWidth)

  // 发起新的 refill request。
  //
  // Push 同拍不发新的 refill：
  //   因为 Push 会更新 stack_top 和 TopCache 排列。
  //   下一拍再基于新的 stack_top/topCacheCount 重新计算 refill 地址。
  //
  // TopCache 满时，只有 Pop 同拍才允许预读。
  refillReq := inWork && !pushFire && refillHasMoreData && (popFire || !topCacheFull)

  when(refillReq) {
    refillRespIdx := refillReqIdx
  }

  // readSync response valid 延迟一拍。
  refillRespValid := refillReq

  // Push 发生时，上一拍发出的 refill response 已经不可信。
  // 直接清掉，避免旧 response 在之后被错误 append。
  when(pushFire) {
    refillRespValid := False
  }

  // Debug helper
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

      dbg(Seq(
        "PrePop from TopCache, offset=", prefetchOffset,
        " index=", topCacheIdx(prefetchOffset),
        " data=", topCacheData(prefetchOffset)
      ))
    }

    // push_count / push-follow bookkeeping
    val pushCountAfterPush = push_count + pushFire.asUInt.resize(32)
    val pushCountAfterPop = Mux(
      popFire && push_count =/= U(0, 32 bits),
      pushCountAfterPush - U(1, 32 bits),
      pushCountAfterPush
    )

    push_count := pushCountAfterPop

    when(pushFire) {
      not_prefetch := True
    }

    // LastPush 是 push burst 结束信号。
    // 它不进入 TaskStack，只表示可以重新开启 PrePop。
    when(io.toStack.LastPush) {
      not_prefetch := False
    }

    when(preFire) {
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
      // Push + Pop 同拍：
      //   Pop 返回旧 topCacheData(0)
      //   Push 写入当前 stack_top 位置，成为新的 top
      //   stack_top 不变，topCacheCount 不变
      stack_data.write(stack_top, io.toStack.Push.payload)

      topCacheData(0)       := io.toStack.Push.payload
      topCacheIdx(0)        := stack_top
      topCachePrefetched(0) := False
      prefetched(stack_top) := False

      dbg(Seq(
        "Push+Pop replace top, index=", stack_top,
        " push=", io.toStack.Push.payload,
        " pop=", topCacheData(0)
      ))

    }.elsewhen(pushFire) {
      // Push only：
      //   写入 stack_top + 1
      //   TopCache 整体右移，新数据插入 offset 0
      //
      // 如果 TopCache 已满，最后一个 entry 会被挤出。
      // 被挤出的数据没有丢，因为它仍然在 stack_data 中。
      // 后续 Pop 造成空位时，会重新从 stack_data refill 回来。
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

      dbg(Seq(
        "Push TopCache, index=", pushIndex,
        " data=", io.toStack.Push.payload
      ))

    }.elsewhen(popFire) {
      // Pop only：
      //   消费 offset 0
      //   TopCache 左移
      //
      // 如果本周期刚好有 refill response 可 append，则插到尾部。
      // 没有 refill response 时，topCacheCount 减 1。
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

        // Pop 减 1，refill append 加 1，净效果 count 不变。
        topCacheCount := topCacheCount

        dbg(Seq(
          "Pop with refill append, popIndex=", stack_top,
          " appendIndex=", refillAppendIdx,
          " appendData=", refillAppendData
        ))

      }.otherwise {
        when(!topCacheEmpty) {
          topCacheCount := topCacheCount - U(1, topCacheCountWidth bits)
        }

        dbg(Seq(
          "Pop TopCache, index=", stack_top,
          " data=", topCacheData(0)
        ))
      }

      stack_top := stk_prevTop

    }.otherwise {
      // 无 Push / Pop：
      //   如果有 refill response，并且 TopCache 未满，则 append 到尾部。
      //
      // 注意：
      //   如果 TopCache 已满，response 会被丢弃，不会 hold。
      //   这是为了避免旧 response 在 Push 后错序插入。
      when(refillAppendAllowed) {
        val insertOff = topCacheCount.resize(topCacheOffsetWidth)

        topCacheData(insertOff)       := refillAppendData
        topCacheIdx(insertOff)        := refillAppendIdx
        topCachePrefetched(insertOff) := refillAppendPrefetched
        topCacheCount                 := topCacheCount + U(1, topCacheCountWidth bits)

        dbg(Seq(
          "TopCache refill append, offset=", insertOff,
          " index=", refillAppendIdx,
          " data=", refillAppendData
        ))
      }
    }
  }

  // ============================================================================
  // SpillOut Area
  //
  // 当片上 stack_data 太满时，从 bottom 端搬数据到 JVM queue。
  //
  // 这里也使用 readSync：
  //   readReq      发起同步读
  //   readPending  下一拍拿到数据并打包
  //   dataValid    发 MMU write request
  // ============================================================================

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
      val emsPerLine      = LineBytesNum / (GCElementWidth / 8)
      val offsetInLine    = queue_bottom(log2Up(emsPerLine) - 1 downto 0)
      val remainingInLine = emsPerLine - offsetInLine
      val wantNum         = U(4)
      val reqNum          = Mux(wantNum >= remainingInLine, remainingInLine, wantNum)

      val addr = elemAddr(queue_bottom)

      val spillPtrs =
        Vec((0 until 4).map(i => stkInc(stack_bottom, U(i + 1, stackPtrWidth bits))))

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
        issueReq(
          io.Mreq,
          addrBuf,
          True,
          ((reqNumBuf.resize(LineBytesNumBitSize)) << 3).resize(LineBytesNumBitSize),
          packDataBuf,
          False,
          False,
          issued
        ) { _ => }
      }

      when(issued) {
        issued    := False
        dataValid := False

        val newQueueBottom =
          queInc(queueBottomBuf, reqNumBuf.resize(queuePtrWidth))

        stack_bottom := stkInc(stackBottomBuf, reqNumBuf.resized)
        queue_bottom := newQueueBottom

        dbg(Seq(
          "SpillOut, moveNum=", reqNumBuf,
          " old queue_bottom=", queueBottomBuf,
          " new queue_bottom=", newQueueBottom
        ))
      }
    }

    def clear(): Unit = {
      issued      := False
      readPending := False
      dataValid   := False
    }
  }

  // ============================================================================
  // ReadBack Area
  //
  // 当片上 stack_data 太空，并且 JVM queue 中还有任务时，
  // 从 JVM queue 搬一批任务回 stack_data 的 bottom 端。
  // ============================================================================

  val readBackArea = new Area {
    val issued = RegInit(False)

    def run(): Unit = {
      val wantNum = U(4, queue_bottom.getWidth bits)
      val queueAvail = queue_bottom

      val queueBottomElements =
        elemAddr(queue_bottom)(4 downto 0) >> 3

      val reqNumTemp =
        Mux(wantNum >= queueAvail, queue_bottom, wantNum)

      val reqNum =
        Mux(
          reqNumTemp >= queueBottomElements && queueBottomElements =/= 0,
          queueBottomElements,
          reqNumTemp
        )

      // 保留 1 个空位，避免环形栈 full / empty 无法区分。
      val freeForReadback =
        Mux(
          task_free > U(1, task_free.getWidth bits),
          task_free - U(1, task_free.getWidth bits),
          U(0, task_free.getWidth bits)
        )

      val canReceive =
        Mux(
          freeForReadback >= reqNum.resize(task_free.getWidth),
          reqNum.resize(task_free.getWidth),
          freeForReadback
        )

      val readIndex = queDec(queue_bottom, reqNum)
      val readAddr  = elemAddr(readIndex)

      issueReq(io.Mreq, readAddr, False, reqNum * U(8), U(0), True, False, issued) { rd =>
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
  val task_exhausted = task_empty && queue_bottom === U(0, queuePtrWidth bits) && topCacheCount === U(0, topCacheCountWidth bits) &&
      !refillRespValid && push_count === U(0, 32 bits) && pushPrePopRem === U(0, 32 bits)

  // FSM
  val fsm = new StateMachine {
    val IDLE: State         = new State with EntryPoint
    val WORK: State         = new State

    inWork := isActive(WORK)

    always {
      when(isEntering(WORK) && isExiting(IDLE)) {
        queue_bottom     := io.ConfigIO.TaskQueue_Bottom
        queue_elems_base := io.ConfigIO.TaskQueue_ElemsBase.resize(MMUAddrWidth)

        dbg(Seq(
          "Config JVM Queue, Bottom=", io.ConfigIO.TaskQueue_Bottom,
          " ElemsBase=", io.ConfigIO.TaskQueue_ElemsBase
        ))
      }

      when(isEntering(IDLE) && isExiting(WORK)) {
        io.ConfigIO.TaskStackDone := True
      }
    }

    IDLE.whenIsActive {
      io.ConfigIO.TaskReady := True

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

      when(io.ConfigIO.TaskValid && io.ConfigIO.TaskReady) {
        goto(WORK)
      }
    }

    WORK.whenIsActive {
      when(!task_exhausted || !io.toFetch.Pop.ready) {
        handleFastPath()

        // 慢路径优先级：
        //   1. spillOut 已经开始时必须继续跑完
        //   2. 栈太满时 spillOut
        //   3. 栈太空时 readBack
        //
        // TopCache refill 是独立的后台同步读逻辑。
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