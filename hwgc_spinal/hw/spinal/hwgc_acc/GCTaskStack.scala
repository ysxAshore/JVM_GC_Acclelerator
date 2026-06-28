package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/* GCTaskStack
 * @description: 加速器内部的任务栈缓存,支持和JVM hotspot侧任务队列交换数据
 *               存在两层存储: 片上stack_data, 是加速器内部的Push和Pop的操作对象
 *                             JVM HotSpot TaskQueue, 位于内存中,通过MMU访问. 在stack_data满或空阈值时通过SpillOut和ReadBack协同stack_data和TaskQueue
 *               
 *               stack_top: push and pop operator pointer, Push时先加再写 Pop时先读再减
 *               stack_bottom: spillout and readback operator pointer
 *               queue_bottom: from Config Parameter for spillout and readback
 *                  spillout: queue_bottom 计算写地址 写write_num个(stack_bottom ~ stack_bottom + write_num - 1)
 *                  readback: queue_bottom - read_num 去计算读地址 读出read_num个 写到stack_bottom - i 的位置上(i 0->until read_num)
 * @parameters:  needs Config Parameter--bottomAddr and Base
 * @notice:      stack_data 使用readSync 有一拍的读延迟.
 *               为了不降低Pop和PrePop性能, 引入了recentPushBuf 保存最近Push数据
 *               Pop是会真正消费的任务 PrePop是预取 只会标记prefetched 不改变stack_top
 *
 * States: IDLE → WORK → UPDATE_CACHE → IDLE
 */
class GCTaskStack extends Module with GCTopParameters with GCParameters with HWParameters {
  val io = new Bundle {
    val toFetch = master(new GCToFetch)           //pop, prepop and precount
    val toStack = slave(new GCToStack)
    val gcUpdatedAop = slave(new GCUpdatedAop)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCTaskStackConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // --- MMU port defaults ---
  io.Mreq.Request.valid         := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready        := True

  io.ConfigIO.Done      := False
  io.ConfigIO.TaskReady := False

  //  HWGC Queue – Sacrificing one space indicates fullness
  //  push/pop operate on stack_top; readBack/spillOut operate on stack_bottom
  // ============================================================================
  val stackPtrWidth = log2Up(GCTaskStack_Entry)
  val queuePtrWidth = 32

  val stack_top    = RegInit(U(0, stackPtrWidth bits))
  val stack_bottom = RegInit(U(0, stackPtrWidth bits))

  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom = RegInit(U(0, queuePtrWidth bits))

  // --- Stack data memory, synchronous read ---
  val stack_data = Mem(UInt(GCElementWidth bits), GCTaskStack_Entry)
  val prefetched = Vec.fill(GCTaskStack_Entry)(RegInit(False))

  // ============================================================================
  // Recent Push Buffer
  // @function: 保存最近push进来的元素
  // Normal Push writes stack_data and also inserts here.
  // Pop / PrePop first checks this buffer. If hit, it bypasses stack_data.readSync.
  //
  // LastPush is still handled by popHold bypass directly and is not inserted here,
  // because it is intentionally not written into stack_data.
  // ============================================================================
  val recentPushValid = Vec.fill(PreFetchBufferNum)(RegInit(False))
  val recentPushIdx   = Vec.fill(PreFetchBufferNum)(Reg(UInt(stackPtrWidth bits)))
  val recentPushData  = Vec.fill(PreFetchBufferNum)(Reg(UInt(GCElementWidth bits)))

  def recentLookup(idx: UInt): (Bool, UInt) = {
    val hits = Vec(Bool(), PreFetchBufferNum)

    for (i <- 0 until PreFetchBufferNum) {
      hits(i) := recentPushValid(i) && recentPushIdx(i) === idx
    }

    val hit  = hits.asBits.orR
    val data = MuxOH(hits.asBits, recentPushData)

    (hit, data)
  }

  def recentInsert(idx: UInt, data: UInt): Unit = {
    for (i <- PreFetchBufferNum - 1 downto 1) {
      recentPushValid(i) := recentPushValid(i - 1) && recentPushIdx(i - 1) =/= idx
      recentPushIdx(i)   := recentPushIdx(i - 1)
      recentPushData(i)  := recentPushData(i - 1)
    }

    recentPushValid(0) := True
    recentPushIdx(0)   := idx
    recentPushData(0)  := data
  }

  def recentInvalidate(idx: UInt): Unit = {
    for (i <- 0 until PreFetchBufferNum) {
      when(recentPushValid(i) && recentPushIdx(i) === idx) {
        recentPushValid(i) := False
      }
    }
  }

  def recentClear(): Unit = {
    for (i <- 0 until PreFetchBufferNum) {
      recentPushValid(i) := False
    }
  }

  // --- Helper functions ---
  def stkInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskStack_Entry, step)
  def stkDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskStack_Entry, step)
  def queInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def queDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def elemAddr(idx: UInt): UInt = (queue_elems_base + (idx.resize(MMUAddrWidth) << 3)).resize(MMUAddrWidth)

  val stk_nextTop = stkInc(stack_top, U(1))
  val stk_prevTop = stkDec(stack_top, U(1))

  val task_empty = stack_top === stack_bottom
  val task_usage = (stack_top - stack_bottom).resize(stackPtrWidth + 1)
  val task_free = U(GCTaskStack_Entry - 1, stackPtrWidth + 1 bits) - task_usage
  val task_exhausted_raw = task_empty && queue_bottom === U(0)

  val need_spillOut = task_usage >= U(GCTaskStack_SpillNeed + 4, task_usage.getWidth bits)
  val need_readback = (task_usage <= U(GCTaskStack_ReadNeed - 4, task_usage.getWidth bits)) && (queue_bottom =/= U(0))

  // --- Push counter for prePop inhibition ---
  val push_count = RegInit(U(0, 32 bits))
  val not_prefetch = RegInit(False)

  // After the first push-follow PrePop, GCFetch keeps pushFollowRem internally.
  // GCTaskStack also needs to remember how many pushed entries should still be
  // supplied through PrePop.
  val pushPrePopRem    = RegInit(U(0, 32 bits))
  val pushPrePopOffset = RegInit(U(1, stackPtrWidth bits))

  // --- Prefetch candidate logic ---
  val candidates    = Vec(Bool(), PreFetchScanWindow)
  val candidateIdxs = Vec(UInt(stackPtrWidth bits), PreFetchScanWindow)

  for (i <- 1 until PreFetchScanWindow + 1) {
    val idx = stkDec(stack_top, U(i, stackPtrWidth bits))
    candidates(i - 1) := (U(i + 1, task_usage.getWidth bits) <= task_usage) && !prefetched(idx)
    candidateIdxs(i - 1) := idx
  }

  val firstValidOH = OHMasking.first(candidates.asBits)

  // --- I/O gating: only valid/ready when in WORK state ---
  val inWork = Bool()

  // ============================================================================
  // Pop sync-read / recent-bypass pipeline
  //
  // If stack_top hits recentPushBuf, Pop can return immediately from registers.
  // Otherwise it falls back to stack_data.readSync.
  // ============================================================================
  val popReadPending   = RegInit(False)
  val popHoldValid     = RegInit(False)
  val popHoldFromSpec  = RegInit(False)
  val popHoldFromRecent = RegInit(False)
  val popHoldIdx       = Reg(UInt(stackPtrWidth bits))
  val popHoldData      = Reg(UInt(GCElementWidth bits))
  val popReadIdxReg    = Reg(UInt(stackPtrWidth bits))

  val popBlockedByPushFollow =
    (push_count =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))

  val (popRecentHit, popRecentData) = recentLookup(stack_top)

  val popCanStart =
    inWork &&
      !popBlockedByPushFollow &&
      !io.toStack.Push.valid &&
      !task_empty &&
      !popReadPending &&
      !popHoldValid

  val popBypassReq = popCanStart && popRecentHit
  val popReadReq   = popCanStart && !popRecentHit

  val popReadData = stack_data.readSync(stack_top, popReadReq)

  val popReturnValid = popReadPending
  val popBypassValid = popBypassReq

  val popOutValid =
    popHoldValid || popReturnValid || popBypassValid

  val popOutData = Mux(
    popHoldValid,
    popHoldData,
    Mux(popReturnValid, popReadData, popRecentData)
  )

  // Speculative Pop exists when:
  // - sync read is pending,
  // - hold contains memory/recent speculative Pop data.
  //
  // LastPush bypass data is not speculative old stack data.
  val popSpecActive =
    popReadPending || (popHoldValid && popHoldFromSpec)

  val taskFreeForPush = Mux(
    popSpecActive && task_free =/= U(0, task_free.getWidth bits),
    task_free - U(1, task_free.getWidth bits),
    task_free
  )

  val bypassHoldActive = popHoldValid && !popHoldFromSpec

  val pushCanAccept =
    inWork &&
      taskFreeForPush =/= U(0, taskFreeForPush.getWidth bits) &&
      !bypassHoldActive

  val popSquashByPush =
    pushCanAccept &&
      io.toStack.Push.valid &&
      popSpecActive

  when(popReadReq) {
    popReadPending := True
    popReadIdxReg  := stack_top
  }

  when(popBypassReq) {
    popReadIdxReg := stack_top

    when(!io.toFetch.Pop.ready) {
      popHoldValid      := True
      popHoldFromSpec   := True
      popHoldFromRecent := True
      popHoldIdx        := stack_top
      popHoldData       := popRecentData
    }
  }

  when(popReturnValid) {
    popReadPending := False

    when(!io.toFetch.Pop.ready && !popSquashByPush) {
      popHoldValid      := True
      popHoldFromSpec   := True
      popHoldFromRecent := False
      popHoldIdx        := popReadIdxReg
      popHoldData       := popReadData
    }
  }

  // Consume held Pop data.
  when(io.toFetch.Pop.fire && popHoldValid) {
    when(popHoldFromRecent) {
      recentInvalidate(popHoldIdx)
    }

    popHoldValid      := False
    popHoldFromSpec   := False
    popHoldFromRecent := False
  }

  // Consume recent-bypass Pop directly.
  when(io.toFetch.Pop.fire && popBypassValid) {
    recentInvalidate(stack_top)
  }

  // Push has priority over stale speculative Pop.
  // If recent entry was speculatively held, do not invalidate it.
  when(popSquashByPush) {
    popReadPending    := False
    popHoldValid      := False
    popHoldFromSpec   := False
    popHoldFromRecent := False
  }

  when(!inWork) {
    popReadPending    := False
    popHoldValid      := False
    popHoldFromSpec   := False
    popHoldFromRecent := False
  }

  io.toFetch.Pop.valid   := inWork && popOutValid && !popSquashByPush
  io.toFetch.Pop.payload := popOutData

  io.toStack.Push.ready := pushCanAccept

  // PushCount seen by GCFetch should exclude the task consumed by Pop in the same cycle.
  val pushCountForFetch = Mux(
    io.toFetch.Pop.fire && push_count =/= U(0, 32 bits),
    push_count - U(1, 32 bits),
    push_count
  )

  io.toFetch.PushCount := pushCountForFetch

  // PrePop 使用 pushCountForFetch，而不是原始 push_count。
  val pushFollowPrePopMode =
    (pushCountForFetch =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))

  val pushFollowPrePopIdx = Mux(
    pushCountForFetch =/= U(0, 32 bits),
    stack_top,
    stkDec(stack_top, pushPrePopOffset)
  )

  val prefetchHit = Mux(
    pushFollowPrePopMode,
    True,
    firstValidOH.orR
  )

  val prefetchIdx = Mux(
    pushFollowPrePopMode,
    pushFollowPrePopIdx,
    MuxOH(firstValidOH, candidateIdxs)
  )

  val task_exhausted =
    task_exhausted_raw &&
      !popReadPending &&
      !popHoldValid &&
      pushPrePopRem === U(0, 32 bits)

  // ============================================================================
  // PrePop sync-read / recent-bypass pipeline
  //
  // If prefetchIdx hits recentPushBuf, PrePop can return immediately from registers.
  // Otherwise it falls back to stack_data.readSync.
  // ============================================================================
  val prefetchReadPending = RegInit(False)
  val prefetchHoldValid   = RegInit(False)

  val prefetchIdxReg   = Reg(UInt(stackPtrWidth bits))
  val prefetchHoldIdx  = Reg(UInt(stackPtrWidth bits))
  val prefetchHoldData = Reg(UInt(GCElementWidth bits))

  val prefetchBlockedByPop = popReadReq || popBypassReq

  val (prefetchRecentHit, prefetchRecentData) = recentLookup(prefetchIdx)

  val prefetchCanStart =
    inWork &&
      prefetchHit &&
      !not_prefetch &&
      !prefetchBlockedByPop &&
      !io.toStack.Push.valid &&
      !prefetchReadPending &&
      !prefetchHoldValid

  val prefetchBypassReq = prefetchCanStart && prefetchRecentHit
  val prefetchReadReq   = prefetchCanStart && !prefetchRecentHit

  val prefetchReadData = stack_data.readSync(prefetchIdx, prefetchReadReq)

  val prefetchReturnValid = prefetchReadPending
  val prefetchBypassValid = prefetchBypassReq

  val prefetchOutValid =
    prefetchHoldValid || prefetchReturnValid || prefetchBypassValid

  val prefetchOutIdx = Mux(
    prefetchHoldValid,
    prefetchHoldIdx,
    Mux(prefetchReturnValid, prefetchIdxReg, prefetchIdx)
  )

  val prefetchOutData = Mux(
    prefetchHoldValid,
    prefetchHoldData,
    Mux(prefetchReturnValid, prefetchReadData, prefetchRecentData)
  )

  when(prefetchReadReq) {
    prefetchReadPending := True
    prefetchIdxReg      := prefetchIdx
  }

  when(prefetchBypassReq && !io.toFetch.PrePop.ready) {
    prefetchHoldValid := True
    prefetchHoldIdx   := prefetchIdx
    prefetchHoldData  := prefetchRecentData
  }

  when(prefetchReturnValid) {
    prefetchReadPending := False

    when(!io.toFetch.PrePop.ready || not_prefetch) {
      prefetchHoldValid := True
      prefetchHoldIdx   := prefetchIdxReg
      prefetchHoldData  := prefetchReadData
    }
  }

  when(io.toFetch.PrePop.fire && prefetchHoldValid) {
    prefetchHoldValid := False
  }

  // Any Push may change top/LIFO order, so squash stale PrePop result.
  when(io.toStack.Push.fire) {
    prefetchReadPending := False
    prefetchHoldValid   := False
  }

  when(!inWork) {
    prefetchReadPending := False
    prefetchHoldValid   := False
  }

  io.toFetch.PrePop.valid   := inWork && prefetchOutValid && !not_prefetch
  io.toFetch.PrePop.payload := prefetchOutData

  // ============================================================================
  // push_count / pushPrePopRem update
  // ============================================================================
  val pushCountAfterPush = (push_count + io.toStack.Push.fire.asUInt.resize(32)).resized

  val pushCountAfterPop = Mux(
    io.toFetch.Pop.fire && pushCountAfterPush =/= U(0, 32 bits),
    pushCountAfterPush - U(1, 32 bits),
    pushCountAfterPush
  )

  push_count := pushCountAfterPop

  when(io.toStack.Push.fire) {
    not_prefetch := True
  }

  when(io.toStack.LastPush) {
    not_prefetch := False
  }

  when(io.toFetch.PrePop.fire) {
    when(pushCountForFetch =/= U(0, 32 bits)) {
      val takeNum = Mux(
        pushCountForFetch > U(PreFetchBufferNum, 32 bits),
        U(PreFetchBufferNum, 32 bits),
        pushCountForFetch
      )

      push_count       := U(0, 32 bits)
      pushPrePopRem    := takeNum - U(1, 32 bits)
      pushPrePopOffset := U(1, stackPtrWidth bits)

    }.elsewhen(pushPrePopRem =/= U(0, 32 bits)) {
      pushPrePopRem    := pushPrePopRem - U(1, 32 bits)
      pushPrePopOffset := pushPrePopOffset + U(1, stackPtrWidth bits)
    }
  }

  // --- Debug helper ---
  def dbg(msg: Seq[Any]): Unit = {
    if (DebugEnable) report(Seq("[GCTaskStack<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
  }

  //  Push / Pop handling (called from WORK state)
  def handlePushAndPop(): Unit = {
    when(io.toFetch.PrePop.fire) {
      prefetched(prefetchOutIdx) := True
      dbg(Seq("PreFetch, index=", prefetchOutIdx, " data=", prefetchOutData))
    }

    // Speculative Pop read from memory.
    when(popReadReq) {
      stack_top := stk_prevTop
      dbg(Seq("PopReadReq, index=", stack_top))
    }

    // Speculative Pop from recentPushBuf.
    when(popBypassReq) {
      stack_top := stk_prevTop
      dbg(Seq("PopRecentBypass, index=", stack_top, " data=", popRecentData))
    }

    when(io.toStack.Push.fire) {
      val restoreTop = Mux(popSpecActive, popReadIdxReg, stack_top)
      val pushIndex  = stkInc(restoreTop, U(1))

      when(io.toStack.LastPush) {
        // LastPush bypass:
        // Do not write stack_data.
        // Keep stack_top at restoreTop and expose this pushed data via popHold.
        stack_top          := restoreTop
        popHoldValid       := True
        popHoldFromSpec    := False
        popHoldFromRecent  := False
        popHoldIdx         := restoreTop
        popHoldData        := io.toStack.Push.payload

        dbg(Seq(
          "LastPush bypass to PopHold, data=", io.toStack.Push.payload,
          " restoreTop=", restoreTop
        ))

      } otherwise {
        // Normal Push.
        // If speculative Pop existed, restore top first, then push above it.
        stack_top := pushIndex
        prefetched(pushIndex) := False
        stack_data.write(pushIndex, io.toStack.Push.payload)

        // Insert normal pushed entry into recentPushBuf for Pop/PrePop bypass.
        recentInsert(pushIndex, io.toStack.Push.payload)

        dbg(Seq(
          "Push, index=", pushIndex,
          " data=", io.toStack.Push.payload,
          " restoreTop=", restoreTop
        ))
      }
    }
  }

  //  SpillOut Area – write elements from stack_bottom to queue (cache-line aligned)
  //  Max 4 elements per line (LineBytesNum=32B / GCElementWidth=8B = 4 elems/line)
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

      val spillPtrs = Vec((0 until 4).map(i => stkInc(stack_bottom, U(i + 1))))

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

        val newQueueBottom = queInc(queueBottomBuf, reqNumBuf.resize(queuePtrWidth))

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

  //  ReadBack Area – read elements from queue, write to stack_bottom (cache-line aligned)
  //  Max 4 elements per line (LineBytesNum=32B / GCElementWidth=8B = 4 elems/line)
  val readBackArea = new Area {
    val issued = RegInit(False)

    def run(): Unit = {
      val wantNum = U(4, queue_bottom.getWidth bits)
      val queueAvail = queue_bottom
      val queueBottomElements = elemAddr(queue_bottom)(4 downto 0) >> 3
      val reqNum_temp = Mux(wantNum >= queueAvail, queue_bottom, wantNum)
      val reqNum = Mux(reqNum_temp >= queueBottomElements && queueBottomElements =/= 0, queueBottomElements, reqNum_temp)

      val freeForReadback = Mux(task_free > U(1, task_free.getWidth bits), task_free - 1, U(0, task_free.getWidth bits))
      val canReceive      = Mux(freeForReadback >= reqNum.resize(task_free.getWidth), reqNum.resize(task_free.getWidth), freeForReadback)
      val readIndex       = queDec(queue_bottom, reqNum)
      val readAddr        = elemAddr(readIndex)

      issueReq(io.Mreq, readAddr, False, reqNum * U(8), U(0), True, False, issued) { rd =>
        val elems = rd.subdivideIn(GCElementWidth bits)
        val newQueueBottom = queDec(queue_bottom, canReceive)

        for (i <- 0 until 4) {
          when(i < canReceive) {
            val writeElement = elems((reqNum - 1 - i).resized)
            val wrPtr = stkDec(stack_bottom, U(i))

            stack_data.write(wrPtr, writeElement)
            prefetched(wrPtr) := False

            // ReadBack writes old queued entries into stack_data.
            // Invalidate recentPushBuf entry with the same physical index to avoid stale hit.
            recentInvalidate(wrPtr)
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

  //  MMU Update Area – 3 sequential write requests (UPDATE_CACHE state)
  val mmuUpdate = new Area {
    val reqIdx = RegInit(U(0, 2 bits))
    val sendIdx = RegInit(U(0, 2 bits))
    val respIdx = RegInit(U(0, 2 bits))

    def generateUpdateMMU(idx: UInt): (Bool, UInt, UInt, UInt) = {
      val valid = Bool()
      val addr  = UInt(GCElementWidth bits)
      val size  = UInt(LineBytesNumBitSize bits)
      val data  = UInt(MMUDataWidth bits)

      switch(idx) {
        is(U(0)) {
          addr  := io.gcUpdatedAop.Addr0
          size  := U(8)
          data  := io.gcUpdatedAop.Data0.resize(MMUDataWidth)
          valid := io.gcUpdatedAop.Valid0
        }

        is(U(1)) {
          addr  := io.gcUpdatedAop.Addr1
          size  := U(24)
          data  := io.gcUpdatedAop.Data1.resize(MMUDataWidth)
          valid := io.gcUpdatedAop.Valid1
        }

        is(U(2)) {
          addr  := io.gcUpdatedAop.Addr2
          size  := U(16)
          data  := io.gcUpdatedAop.Data2.resize(MMUDataWidth)
          valid := io.gcUpdatedAop.Valid2
        }

        default {
          addr  := U(0)
          size  := U(0)
          data  := U(0)
          valid := False
        }
      }

      (valid, addr, size, data)
    }

    def run(): Unit = {
      val (valid, addr, size, data) = generateUpdateMMU(reqIdx)

      io.Mreq.Request.valid := valid && reqIdx < 3
      io.Mreq.Request.payload.RequestType_isWrite := True
      io.Mreq.Request.payload.RequestVirtualAddr := addr
      io.Mreq.Request.payload.RequestSourceID := io.Mreq.ConherentRequsetSourceID.payload
      io.Mreq.Request.payload.NeedDoCmpxChg := False
      io.Mreq.Request.payload.RequestWStrb := getWstrb(size.resize(LineBytesNumBitSize))
      io.Mreq.Request.payload.NeedResponse := True
      io.Mreq.Request.payload.RequestData := data
      io.Mreq.Request.payload.RequestSize := size.resize(LineBytesNumBitSize)
      io.Mreq.Response.ready := True

      when(!valid && reqIdx < 3) {
        reqIdx := reqIdx + 1
      }

      when(io.Mreq.Request.fire) {
        reqIdx := reqIdx + 1
        sendIdx := sendIdx + 1
      }

      when(io.Mreq.Response.fire) {
        respIdx := respIdx + 1
      }
    }

    def clear(): Unit = {
      reqIdx := U(0)
      respIdx := U(0)
    }

    def updateFinished: Bool = reqIdx === 3 && respIdx === sendIdx
  }

  //  StateMachine: IDLE → WORK → UPDATE_CACHE → IDLE
  val fsm = new StateMachine {
    val IDLE: State         = new State with EntryPoint
    val WORK: State         = new State
    val UPDATE_CACHE: State = new State

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

      when(isEntering(UPDATE_CACHE) && isExiting(WORK)) {
        mmuUpdate.clear()
      }

      when(isEntering(IDLE) && isExiting(UPDATE_CACHE)) {
        io.ConfigIO.Done := True
      }
    }

    IDLE.whenIsActive {
      io.ConfigIO.TaskReady := True

      for (i <- 0 until GCTaskStack_Entry) {
        prefetched(i) := False
      }

      spillOutArea.clear()
      readBackArea.clear()
      recentClear()

      stack_top    := U(0)
      stack_bottom := U(0)
      queue_bottom := U(0)

      push_count       := U(0, 32 bits)
      pushPrePopRem    := U(0, 32 bits)
      pushPrePopOffset := U(1, stackPtrWidth bits)
      not_prefetch     := False

      popReadPending     := False
      popHoldValid       := False
      popHoldFromSpec    := False
      popHoldFromRecent  := False

      prefetchReadPending := False
      prefetchHoldValid   := False

      when(io.ConfigIO.TaskValid && io.ConfigIO.TaskReady) {
        goto(WORK)
      }
    }

    WORK.whenIsActive {
      when(!task_exhausted || !io.toFetch.Pop.ready) {
        handlePushAndPop()

        when(need_spillOut || spillOutArea.busy) {
          spillOutArea.run()
        }.elsewhen(need_readback) {
          readBackArea.run()
        }
      }

      when(task_exhausted && io.toFetch.Pop.ready) {
        goto(UPDATE_CACHE)
      }
    }

    UPDATE_CACHE.whenIsActive {
      mmuUpdate.run()

      when(mmuUpdate.updateFinished) {
        goto(IDLE)
      }
    }
  }
}

object GCTaskStackVerilog extends App {
  Config.spinal.generateVerilog(new GCTaskStack())
}

/*
package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

/* GCTaskStack
 * @description: As a hardware buffer for the task queue in the JVM Hotspot
 *               in HWGC pop and push operate on stack_top
 *               in JVM spillout and readback operate on stack_bottom and queue_bottom
 * @parameters: needs Config Parameter--bottomAddr and Base
 *
 * TopCache version:
 *   - Pop / PrePop use register TopCache fast path
 *   - stack_data.readAsync is only used for background cache refill and spillOut
 *   - no readSync hold/squash pipeline
 */
class GCTaskStack extends Module with GCTopParameters with GCParameters with HWParameters {
  val io = new Bundle {
    val toFetch = master(new GCToFetch)
    val toStack = slave(new GCToStack)
    val gcUpdatedAop = slave(new GCUpdatedAop)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCTaskStackConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // --- MMU port defaults ---
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ConfigIO.Done      := False
  io.ConfigIO.TaskReady := False

  // ============================================================================
  // Stack / Queue pointers
  // ============================================================================
  val stackPtrWidth = log2Up(GCTaskStack_Entry)
  val queuePtrWidth = 32

  val stack_top    = RegInit(U(0, stackPtrWidth bits))
  val stack_bottom = RegInit(U(0, stackPtrWidth bits))

  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom     = RegInit(U(0, queuePtrWidth bits))

  val stack_data = Mem(UInt(GCElementWidth bits), GCTaskStack_Entry)

  // Keep this array for compatibility/debug style, but fast prefetch uses TopCache flags.
  val prefetched = Vec.fill(GCTaskStack_Entry)(RegInit(False))

  // ============================================================================
  // Helper functions
  // ============================================================================
  def stkInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskStack_Entry, step)
  def stkDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskStack_Entry, step)
  def queInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def queDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def elemAddr(idx: UInt): UInt = (queue_elems_base + (idx.resize(MMUAddrWidth) << 3)).resize(MMUAddrWidth)

  val stk_nextTop = stkInc(stack_top, U(1))
  val stk_prevTop = stkDec(stack_top, U(1))

  val task_empty = stack_top === stack_bottom
  val task_usage = (stack_top - stack_bottom).resize(stackPtrWidth + 1)
  val task_free  = U(GCTaskStack_Entry - 1, stackPtrWidth + 1 bits) - task_usage

  val need_spillOut = task_usage >= U(GCTaskStack_SpillNeed + 4, task_usage.getWidth bits)
  val need_readback = (task_usage <= U(GCTaskStack_ReadNeed - 4, task_usage.getWidth bits)) && (queue_bottom =/= U(0))

  // ============================================================================
  // TopCache
  //
  // topCacheData(0) = logical stack top
  // topCacheData(1) = one below stack top
  // ...
  //
  // Pop / PrePop are served from here.
  // stack_data is write-through on Push and background refill source.
  // ============================================================================
  val TopCacheDepth = scala.math.min(
    GCTaskStack_Entry,
    scala.math.max(PreFetchScanWindow + 4, PreFetchBufferNum + 2)
  )

  val topCacheCountWidth  = log2Up(TopCacheDepth + 1)
  val topCacheOffsetWidth = log2Up(TopCacheDepth)

  val topCacheData       = Vec.fill(TopCacheDepth)(Reg(UInt(GCElementWidth bits)))
  val topCachePrefetched = Vec.fill(TopCacheDepth)(RegInit(False))
  val topCacheCount      = RegInit(U(0, topCacheCountWidth bits))

  def cacheOffsetValid(offset: UInt): Bool =
    offset.resize(topCacheCountWidth) < topCacheCount

  def cacheCanRefill: Bool =
    topCacheCount =/= U(TopCacheDepth, topCacheCountWidth bits) &&
      task_usage > topCacheCount.resize(task_usage.getWidth)

  val cacheRefillIdx  = stkDec(stack_top, topCacheCount.resize(stackPtrWidth))
  val cacheRefillData = stack_data.readAsync(cacheRefillIdx)

  // ============================================================================
  // Push-follow bookkeeping for GCFetch preFsm
  // ============================================================================
  val push_count  = RegInit(U(0, 32 bits))
  val not_prefetch = RegInit(False)

  val pushPrePopRem    = RegInit(U(0, 32 bits))
  val pushPrePopOffset = RegInit(U(1, topCacheOffsetWidth bits))

  // --- I/O gating: only valid/ready when in WORK state ---
  val inWork = Bool()

  // ============================================================================
  // Pop fast path from TopCache
  // ============================================================================
  val popAvailable = topCacheCount =/= U(0, topCacheCountWidth bits)

  io.toFetch.Pop.valid   := inWork && popAvailable
  io.toFetch.Pop.payload := topCacheData(0)

  io.toStack.Push.ready := inWork && task_free =/= U(0, task_free.getWidth bits)

  // PushCount seen by GCFetch excludes the pushed item consumed by Pop in the same cycle.
  val pushCountForFetch = Mux(
    io.toFetch.Pop.fire && push_count =/= U(0, 32 bits),
    push_count - U(1, 32 bits),
    push_count
  )

  io.toFetch.PushCount := pushCountForFetch

  // ============================================================================
  // PrePop fast path from TopCache
  // ============================================================================
  val normalCandidates    = Vec(Bool(), PreFetchScanWindow)
  val normalCandidateOffs = Vec(UInt(topCacheOffsetWidth bits), PreFetchScanWindow)

  for (i <- 1 until PreFetchScanWindow + 1) {
    val off = U(i, topCacheOffsetWidth bits)
    normalCandidates(i - 1)    := cacheOffsetValid(off) && !topCachePrefetched(off)
    normalCandidateOffs(i - 1) := off
  }

  val normalFirstOH = OHMasking.first(normalCandidates.asBits)

  // Push-follow mode:
  // - first push-follow PrePop uses offset 0
  // - remaining push-follow PrePop uses pushPrePopOffset
  //
  // PrePop is intentionally not allowed in the same cycle as Pop/Push,
  // so offset 0 is stable and simple.
  val pushFollowPrePopMode =
    (pushCountForFetch =/= U(0, 32 bits)) || (pushPrePopRem =/= U(0, 32 bits))

  val pushFollowOffset = Mux(
    pushCountForFetch =/= U(0, 32 bits),
    U(0, topCacheOffsetWidth bits),
    pushPrePopOffset
  )

  val normalPrePopOffset = MuxOH(normalFirstOH, normalCandidateOffs)

  val prefetchOffset = Mux(
    pushFollowPrePopMode,
    pushFollowOffset,
    normalPrePopOffset
  )

  val prefetchHit = Mux(
    pushFollowPrePopMode,
    cacheOffsetValid(pushFollowOffset),
    normalFirstOH.orR
  )

  // Avoid same-cycle Pop/Push with PrePop so TopCache offset bookkeeping stays simple.
  val prefetchBlockedByStackMove =
    io.toFetch.Pop.fire || io.toStack.Push.fire

  io.toFetch.PrePop.valid :=
    inWork &&
      prefetchHit &&
      !not_prefetch &&
      !prefetchBlockedByStackMove

  io.toFetch.PrePop.payload := topCacheData(prefetchOffset)

  // ============================================================================
  // Task exhausted
  // ============================================================================
  val task_exhausted =
    task_empty &&
      queue_bottom === U(0, queuePtrWidth bits) &&
      topCacheCount === U(0, topCacheCountWidth bits) &&
      pushPrePopRem === U(0, 32 bits)

  // ============================================================================
  // Debug helper
  // ============================================================================
  def dbg(msg: Seq[Any]): Unit = {
    if (DebugEnable) {
      report(Seq("[GCTaskStack<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  }

  // ============================================================================
  // TopCache / Push / Pop / PrePop handling
  // ============================================================================
  def handleFastPath(): Unit = {
    val pushFire = io.toStack.Push.fire
    val popFire  = io.toFetch.Pop.fire
    val preFire  = io.toFetch.PrePop.fire

    // --------------------------------------------------------------------------
    // PrePop marks cache entry as prefetched.
    // PrePop is blocked during Push/Pop, so no shift conflict here.
    // --------------------------------------------------------------------------
    when(preFire) {
      topCachePrefetched(prefetchOffset) := True

      dbg(Seq(
        "PrePop from TopCache, offset=", prefetchOffset,
        " data=", topCacheData(prefetchOffset)
      ))
    }

    // --------------------------------------------------------------------------
    // PushCount / push-follow bookkeeping
    // --------------------------------------------------------------------------
    val pushCountAfterPush = (push_count + pushFire.asUInt.resize(32)).resized

    val pushCountAfterPop = Mux(
      popFire && pushCountAfterPush =/= U(0, 32 bits),
      pushCountAfterPush - U(1, 32 bits),
      pushCountAfterPush
    )

    push_count := pushCountAfterPop

    when(pushFire) {
      not_prefetch := True
    }

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

    // --------------------------------------------------------------------------
    // TopCache and stack_top update
    //
    // Cases:
    //   Push + Pop : replace top, stack_top unchanged
    //   Push only  : push new top, shift cache right
    //   Pop only   : pop top, shift cache left
    //   None       : background refill one entry from stack_data
    // --------------------------------------------------------------------------
    when(pushFire && popFire) {
      // Net stack depth unchanged.
      // Pop returns old topCacheData(0), Push becomes new top.
      stack_data.write(stack_top, io.toStack.Push.payload)

      topCacheData(0)       := io.toStack.Push.payload
      topCachePrefetched(0) := False

      // Count unchanged.
      dbg(Seq(
        "Push+Pop TopCache replace, index=", stack_top,
        " push=", io.toStack.Push.payload,
        " pop=", topCacheData(0)
      ))

    }.elsewhen(pushFire) {
      // Write-through to backing RAM.
      stack_data.write(stk_nextTop, io.toStack.Push.payload)

      // Shift cache down and insert at top.
      for (i <- TopCacheDepth - 1 downto 1) {
        topCacheData(i)       := topCacheData(i - 1)
        topCachePrefetched(i) := topCachePrefetched(i - 1)
      }

      topCacheData(0)       := io.toStack.Push.payload
      topCachePrefetched(0) := False

      when(topCacheCount =/= U(TopCacheDepth, topCacheCountWidth bits)) {
        topCacheCount := topCacheCount + 1
      }

      stack_top := stk_nextTop

      dbg(Seq(
        "Push TopCache, index=", stk_nextTop,
        " data=", io.toStack.Push.payload
      ))

    }.elsewhen(popFire) {
      // Shift cache up.
      for (i <- 0 until TopCacheDepth - 1) {
        topCacheData(i)       := topCacheData(i + 1)
        topCachePrefetched(i) := topCachePrefetched(i + 1)
      }

      topCachePrefetched(TopCacheDepth - 1) := False

      when(topCacheCount =/= U(0, topCacheCountWidth bits)) {
        topCacheCount := topCacheCount - 1
      }

      stack_top := stk_prevTop

      dbg(Seq(
        "Pop TopCache, index=", stack_top,
        " data=", topCacheData(0)
      ))

    }.otherwise {
      // Background refill.
      // This is the only fast-path use of stack_data.readAsync.
      // Its output is captured into a register, not driven directly to Fetch.
      when(
        inWork &&
          cacheCanRefill &&
          !need_spillOut &&
          !need_readback
      ) {
        topCacheData(topCacheCount)       := cacheRefillData
        topCachePrefetched(topCacheCount) := False
        topCacheCount                     := topCacheCount + 1

        dbg(Seq(
          "TopCache refill, offset=", topCacheCount,
          " index=", cacheRefillIdx,
          " data=", cacheRefillData
        ))
      }
    }
  }

  // ============================================================================
  // SpillOut Area – write elements from stack_bottom to queue
  // Uses readAsync only in slow/background path.
  // ============================================================================
  val spillOutArea = new Area {
    val issued = RegInit(False)

    def busy: Bool = issued

    def run(): Unit = {
      val emsPerLine      = LineBytesNum / (GCElementWidth / 8)
      val offsetInLine    = queue_bottom(log2Up(emsPerLine) - 1 downto 0)
      val remainingInLine = emsPerLine - offsetInLine
      val wantNum         = U(4)
      val reqNum          = Mux(wantNum >= remainingInLine, remainingInLine, wantNum)

      val addr      = elemAddr(queue_bottom)
      val spillPtrs = Vec((0 until 4).map(i => stkInc(stack_bottom, U(i + 1))))
      val spillData = Vec((0 until 4).map(i => stack_data.readAsync(spillPtrs(i))))
      val packData  = Cat(spillData.reverse).asUInt

      issueReq(io.Mreq, addr, True, reqNum * U(8), packData, False, False, issued) { _ => }

      when(issued) {
        issued := False

        val newQueueBottom = queInc(queue_bottom, reqNum)

        stack_bottom := stkInc(stack_bottom, reqNum)
        queue_bottom := newQueueBottom

        dbg(Seq(
          "SpillOut, moveNum=", reqNum,
          " old queue_bottom=", queue_bottom,
          " new queue_bottom=", newQueueBottom
        ))
      }
    }

    def clear(): Unit = {
      issued := False
    }
  }

  // ============================================================================
  // ReadBack Area – read elements from queue, write to stack_bottom
  // ============================================================================
  val readBackArea = new Area {
    val issued = RegInit(False)

    def run(): Unit = {
      val wantNum = U(4, queue_bottom.getWidth bits)
      val queueAvail = queue_bottom
      val queueBottomElements = elemAddr(queue_bottom)(4 downto 0) >> 3
      val reqNum_temp = Mux(wantNum >= queueAvail, queue_bottom, wantNum)
      val reqNum = Mux(
        reqNum_temp >= queueBottomElements && queueBottomElements =/= 0,
        queueBottomElements,
        reqNum_temp
      )

      val freeForReadback = Mux(
        task_free > U(1, task_free.getWidth bits),
        task_free - 1,
        U(0, task_free.getWidth bits)
      )

      val canReceive = Mux(
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
            val wrPtr = stkDec(stack_bottom, U(i))

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

  // ============================================================================
  // MMU Update Area – 3 sequential write requests
  // ============================================================================
  val mmuUpdate = new Area {
    val reqIdx  = RegInit(U(0, 2 bits))
    val sendIdx = RegInit(U(0, 2 bits))
    val respIdx = RegInit(U(0, 2 bits))

    def generateUpdateMMU(idx: UInt): (Bool, UInt, UInt, UInt) = {
      val valid = Bool()
      val addr  = UInt(GCElementWidth bits)
      val size  = UInt(LineBytesNumBitSize bits)
      val data  = UInt(MMUDataWidth bits)

      switch(idx) {
        is(U(0)) {
          addr  := io.gcUpdatedAop.Addr0
          size  := U(8)
          data  := io.gcUpdatedAop.Data0.resize(MMUDataWidth)
          valid := io.gcUpdatedAop.Valid0
        }

        is(U(1)) {
          addr  := io.gcUpdatedAop.Addr1
          size  := U(24)
          data  := io.gcUpdatedAop.Data1.resize(MMUDataWidth)
          valid := io.gcUpdatedAop.Valid1
        }

        is(U(2)) {
          addr  := io.gcUpdatedAop.Addr2
          size  := U(16)
          data  := io.gcUpdatedAop.Data2.resize(MMUDataWidth)
          valid := io.gcUpdatedAop.Valid2
        }

        default {
          addr  := U(0)
          size  := U(0)
          data  := U(0)
          valid := False
        }
      }

      (valid, addr, size, data)
    }

    def run(): Unit = {
      val (valid, addr, size, data) = generateUpdateMMU(reqIdx)

      io.Mreq.Request.valid := valid && reqIdx < 3
      io.Mreq.Request.payload.RequestType_isWrite := True
      io.Mreq.Request.payload.RequestVirtualAddr := addr
      io.Mreq.Request.payload.RequestSourceID := io.Mreq.ConherentRequsetSourceID.payload
      io.Mreq.Request.payload.NeedDoCmpxChg := False
      io.Mreq.Request.payload.RequestWStrb := getWstrb(size.resize(LineBytesNumBitSize))
      io.Mreq.Request.payload.NeedResponse := True
      io.Mreq.Request.payload.RequestData := data
      io.Mreq.Request.payload.RequestSize := size.resize(LineBytesNumBitSize)
      io.Mreq.Response.ready := True

      when(!valid && reqIdx < 3) {
        reqIdx := reqIdx + 1
      }

      when(io.Mreq.Request.fire) {
        reqIdx  := reqIdx + 1
        sendIdx := sendIdx + 1
      }

      when(io.Mreq.Response.fire) {
        respIdx := respIdx + 1
      }
    }

    def clear(): Unit = {
      reqIdx  := U(0)
      sendIdx := U(0)
      respIdx := U(0)
    }

    def updateFinished: Bool = reqIdx === 3 && respIdx === sendIdx
  }

  // ============================================================================
  // FSM
  // ============================================================================
  val fsm = new StateMachine {
    val IDLE: State         = new State with EntryPoint
    val WORK: State         = new State
    val UPDATE_CACHE: State = new State

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

      when(isEntering(UPDATE_CACHE) && isExiting(WORK)) {
        mmuUpdate.clear()
      }

      when(isEntering(IDLE) && isExiting(UPDATE_CACHE)) {
        io.ConfigIO.Done := True
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

      spillOutArea.clear()
      readBackArea.clear()

      stack_top    := U(0)
      stack_bottom := U(0)
      queue_bottom := U(0)

      topCacheCount := U(0, topCacheCountWidth bits)

      push_count       := U(0, 32 bits)
      pushPrePopRem    := U(0, 32 bits)
      pushPrePopOffset := U(1, topCacheOffsetWidth bits)
      not_prefetch     := False

      when(io.ConfigIO.TaskValid && io.ConfigIO.TaskReady) {
        goto(WORK)
      }
    }

    WORK.whenIsActive {
      when(!task_exhausted || !io.toFetch.Pop.ready) {
        handleFastPath()

        when(need_spillOut) {
          spillOutArea.run()
        }.elsewhen(need_readback) {
          readBackArea.run()
        }
      }

      when(task_exhausted && io.toFetch.Pop.ready) {
        goto(UPDATE_CACHE)
      }
    }

    UPDATE_CACHE.whenIsActive {
      mmuUpdate.run()

      when(mmuUpdate.updateFinished) {
        goto(IDLE)
      }
    }
  }
}

object GCTaskStackVerilog extends App {
  Config.spinal.generateVerilog(new GCTaskStack())
}
 */
