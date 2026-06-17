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
 * Refactored using StateMachine DSL for cleaner state transitions & reduced timing.
 * States: IDLE → WORK → UPDATE_CACHE → IDLE
 */
class GCTaskStack extends Module with GCTopParameters with GCParameters with HWParameters {
  val io = new Bundle {
    val toFetch = master(new GCToFetch)
    val toStack = slave(new GCToStack)
    val gcUpdatedRegion = slave(new GCUpdatedRegion)
    val gcUpdatedAop = slave(new GCUpdatedAop)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCTaskStackConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // --- MMU port defaults ---
  io.Mreq.Request.valid         := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid     := False
  io.Mreq.RequestSize.payload.clearAll()
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

  // --- Stack data memory (asynchronous read for higher frequency) ---
  val stack_data = Mem(UInt(GCElementWidth bits), GCTaskStack_Entry)
  val prefetched = Vec.fill(GCTaskStack_Entry)(RegInit(False))

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
  val task_exhausted = task_empty && queue_bottom === U(0)

  val need_spillOut = task_usage >= U(GCTaskStack_SpillNeed + 4, task_usage.getWidth bits)
  val need_readback = (task_usage <= U(GCTaskStack_ReadNeed - 4, task_usage.getWidth bits)) && (queue_bottom =/= U(0))

  // --- Push counter for prePop inhibition ---
  val push_count = RegInit(U(0, 32 bits))
  val not_prefetch = RegInit(False)
  when(io.toStack.Push.fire){
    push_count := push_count + 1
    not_prefetch := True
  }
  when(io.toStack.LastPush){
    not_prefetch := False
  }
  when(io.toFetch.PrePop.fire) { push_count := U(0) }
  io.toFetch.PushCount := push_count

  // --- Prefetch logic ---
  val candidates    = Vec(Bool(), PreFetchScanWindow)
  val candidateIdxs = Vec(UInt(stackPtrWidth bits), PreFetchScanWindow)

  for (i <- 1 until PreFetchScanWindow + 1) {
    val idx = stkDec(stack_top, U(i, stackPtrWidth bits))
    candidates(i - 1) := (U(i + 1, task_usage.getWidth bits) <= task_usage) && !prefetched(idx)
    candidateIdxs(i - 1) := idx
  }

  val firstValidOH   = OHMasking.first(candidates.asBits)
  val prefetchHit    = Mux(push_count === U(0), firstValidOH.orR, True)
  val prefetchIdx    = Mux(push_count === U(0), MuxOH(firstValidOH, candidateIdxs), stack_top)
  val prefetchData   = stack_data.readAsync(prefetchIdx)

  // --- I/O gating: only valid/ready when in WORK state ---
  val inWork = Bool()

  io.toFetch.Pop.valid        := inWork && !task_empty
  io.toFetch.Pop.payload      := stack_data.readAsync(stack_top)
  when(io.toFetch.Pop.fire && push_count =/= 0) { push_count := push_count - 1 }

  // --- PrePop arbitration: disable PrePop when prefetchIdx == stack_top to avoid dual-pop conflict ---
  // When push_count == 0, prefetchIdx is selected from candidates and may equal stack_top.
  // Only suppress PrePop when there's an actual address conflict, allowing concurrent
  // Pop + PrePop when they target different elements for higher throughput.
  io.toFetch.PrePop.valid     := inWork && prefetchHit && !not_prefetch
  io.toFetch.PrePop.payload   := prefetchData
  io.toStack.Push.ready       := inWork && task_free =/= U(0)

  // --- Debug helper ---
  def dbg(msg: Seq[Any]): Unit = {
    if (DebugEnable) report(Seq("[GCTaskStack<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
  }

  //  Push / Pop handling (called from WORK state)
  def handlePushAndPop(): Unit = {
    when(io.toFetch.PrePop.fire) {
      prefetched(prefetchIdx) := True
      dbg(Seq("PreFetch, index=", prefetchIdx, " data=", prefetchData))
    }
    when(io.toStack.Push.fire && !io.toFetch.Pop.fire) {
      stack_top := stk_nextTop
      prefetched(stk_nextTop) := False
      stack_data.write(stk_nextTop, io.toStack.Push.payload)
      dbg(Seq("Push, index=", stk_nextTop, " data=", io.toStack.Push.payload))
    }.elsewhen(io.toFetch.Pop.fire && !io.toStack.Push.fire) {
      stack_top := stk_prevTop
      dbg(Seq("Pop, index=", stack_top))
    }.elsewhen(io.toStack.Push.fire && io.toFetch.Pop.fire) {
      prefetched(stack_top) := False
      stack_data.write(stack_top, io.toStack.Push.payload)
      dbg(Seq("Push+Pop simultaneously, index=", stack_top, " push=", io.toStack.Push.payload))
    }
  }

  //  SpillOut Area – write elements from stack_bottom to queue (cache-line aligned)
  //  Max 4 elements per line (LineBytesNum=32B / GCElementWidth=8B = 4 elems/line)
  val spillOutArea = new Area {
    val issued = RegInit(False)

    def run(): Unit = {
      val emsPerLine      = LineBytesNum / (GCElementWidth / 8)              // 4 elems/line
      val offsetInLine    = queue_bottom(log2Up(emsPerLine) - 1 downto 0)    // queue element offset within current line
      val remainingInLine = emsPerLine - offsetInLine                        // space left in current cache line
      val wantNum         = U(4)                                             // ideal batch size
      val reqNum          = Mux(wantNum >= remainingInLine, remainingInLine, wantNum)  // don't cross line boundary

      val addr      = elemAddr(queue_bottom)
      val spillPtrs = Vec((0 until 4).map(i => stkInc(stack_bottom, U(i + 1))))
      val spillData = Vec((0 until 4).map(i => stack_data.readAsync(spillPtrs(i))))
      val packData  = Cat(spillData.reverse).asUInt

      issueReq(io.Mreq, addr, True, reqNum * U(8), packData, issued) { _ =>
        val newQueueBottom = queInc(queue_bottom, reqNum)
        stack_bottom := stkInc(stack_bottom, reqNum)
        queue_bottom := newQueueBottom
        dbg(Seq("SpillOut, moveNum=", reqNum, " old queue_bottom=", queue_bottom, " new queue_bottom=", newQueueBottom))
      }
    }

    def clear(): Unit = { issued := False }
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

      // Reserve 1 slot for front-end push to avoid tight timing
      val freeForReadback = Mux(task_free > U(1, task_free.getWidth bits), task_free - 1, U(0, task_free.getWidth bits))
      val canReceive      = Mux(freeForReadback >= reqNum.resize(task_free.getWidth), reqNum.resize(task_free.getWidth), freeForReadback)
      val readIndex       = queDec(queue_bottom, reqNum)
      val readAddr        = elemAddr(readIndex)

      issueReq(io.Mreq, readAddr, False, reqNum * U(8), U(0), issued) { rd =>
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
        dbg(Seq("ReadBack, reqNum=", reqNum, " receive=", canReceive, " old queue_bottom=", queue_bottom, " new queue_bottom=", newQueueBottom))
      }
    }

    def clear(): Unit = { issued := False }
  }

  //  MMU Update Area – 6 sequential write requests (UPDATE_CACHE state)
  val mmuUpdate = new Area {
    val idx = RegInit(U(0, 3 bits))

    def generateUpdateMMU(idx: UInt): (Bool, UInt, UInt, UInt) = {
      val valid = Bool()
      val addr  = UInt(GCElementWidth bits)
      val size  = UInt(LineBytesNumBitSize bits)
      val data  = UInt(MMUDataWidth bits)
      switch (idx) {
        is(U(0)) { addr := io.gcUpdatedRegion.Buffer0;  size := U(8);  data := io.gcUpdatedRegion.RegionTop0.resize(MMUDataWidth); valid := io.gcUpdatedRegion.Valid0 }
        is(U(1)) { addr := io.gcUpdatedRegion.Buffer1;  size := U(8);  data := io.gcUpdatedRegion.RegionTop1.resize(MMUDataWidth); valid := io.gcUpdatedRegion.Valid1 }
        is(U(2)) { addr := io.gcUpdatedAop.Addr0;       size := U(8);  data := io.gcUpdatedAop.Data0.resize(MMUDataWidth);      valid := io.gcUpdatedAop.Valid0 }
        is(U(3)) { addr := io.gcUpdatedAop.Addr1;       size := U(24); data := io.gcUpdatedAop.Data1.resize(MMUDataWidth);      valid := io.gcUpdatedAop.Valid1 }
        is(U(4)) { addr := io.gcUpdatedAop.Addr2;       size := U(16); data := io.gcUpdatedAop.Data2.resize(MMUDataWidth);      valid := io.gcUpdatedAop.Valid2 }
        is(U(5)) { addr := io.gcUpdatedAop.Addr3;       size := U(32) - io.gcUpdatedAop.Addr3(4 downto 0); data := io.gcUpdatedAop.Data3.resize(MMUDataWidth); valid := io.gcUpdatedAop.Valid3 }
        default  { addr := U(0); size := U(0); data := U(0); valid := False }
      }
      (valid, addr, size, data)
    }

    def run(): Unit = {
      val (valid, addr, size, data) = generateUpdateMMU(idx)
      io.Mreq.Request.valid := valid
      io.Mreq.Request.payload.RequestType_isWrite := True
      io.Mreq.Request.payload.RequestVirtualAddr := addr
      io.Mreq.Request.payload.RequestSourceID := io.Mreq.ConherentRequsetSourceID.payload
      io.Mreq.Request.payload.RequestWStrb := getWstrb(size.resize(LineBytesNumBitSize))
      io.Mreq.Request.payload.RequestData := data
      io.Mreq.RequestSize.valid := valid
      io.Mreq.RequestSize.payload := size
      io.Mreq.Response.ready := True

      when(io.Mreq.Request.fire || !valid) {
        when(idx < 6) { idx := idx + 1 }
      }
    }

    def clear(): Unit = { idx := U(0) }
    def updateFinished: Bool = idx === 6
  }

  //  StateMachine: IDLE → WORK → UPDATE_CACHE → IDLE
  val fsm = new StateMachine {
    val IDLE: State         = new State with EntryPoint
    val WORK: State         = new State
    val UPDATE_CACHE: State = new State

    // I/O gating: inWork is true when actively in WORK state
    inWork := isActive(WORK)

    // Global: detect transitions for edge-triggered actions
    always {
      // Capture config on transition edge (IDLE → WORK)
      when(isEntering(WORK) && isExiting(IDLE)) {
        queue_bottom     := io.ConfigIO.TaskQueue_Bottom
        queue_elems_base := io.ConfigIO.TaskQueue_ElemsBase.resize(MMUAddrWidth)
        dbg(Seq("Config JVM Queue, Bottom=", io.ConfigIO.TaskQueue_Bottom, " ElemsBase=", io.ConfigIO.TaskQueue_ElemsBase))
      }
      // Reset mmuUpdate index when entering UPDATE_CACHE
      when(isEntering(UPDATE_CACHE) && isExiting(WORK)) {
        mmuUpdate.clear()
      }
      // Done signal when transitioning back to IDLE from UPDATE_CACHE
      when(isEntering(IDLE) && isExiting(UPDATE_CACHE)) {
        io.ConfigIO.Done := True
      }
    }

    // --- IDLE: reset pointers, accept config ---
    IDLE.whenIsActive {
      io.ConfigIO.TaskReady := True
      for (i <- 0 until GCTaskStack_Entry) prefetched(i) := False
      spillOutArea.clear()
      readBackArea.clear()
      stack_top    := U(0)
      stack_bottom := U(0)
      queue_bottom := U(0)
      when(io.ConfigIO.TaskValid && io.ConfigIO.TaskReady) {
        goto(WORK)
      }
    }

    // --- WORK: push/pop, spillOut, readBack ---
    WORK.whenIsActive {
      when(!task_exhausted || !io.toFetch.Pop.ready) {
        handlePushAndPop()
        when(need_spillOut)  { spillOutArea.run() }
          .elsewhen(need_readback) { readBackArea.run() }
      }
      when(task_exhausted && io.toFetch.Pop.ready) {
        goto(UPDATE_CACHE)
      }
    }

    // --- UPDATE_CACHE: drive MMU writes ---
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