package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCTaskStack
 * @description: As a hardware buffer for the task queue in the JVM Hotspot
 *               in HWGC pop and push operate on stack_top
 *               in JVM spillout and readback operate on stack_bottom and queue_bottom
 * @parameters: needs Config Parameter--bottomAddr and Base
 */
class GCTaskStack extends Module with GCParameters with HWParameters {
  val io = new Bundle {
    val Pop = master Stream UInt(GCElementWidth bits)
    val Push = slave Stream UInt(GCElementWidth bits)
    val PreFetch = master Stream UInt(GCElementWidth bits)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCTaskStackConfigIO)
    val LastPush = in Bool()
    val PushCount = out UInt(32 bits)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ConfigIO.Done := False
  io.ConfigIO.TaskReady := False

  // HWGC Queue, Sacrificing one space indicates fullness
  // push and pop operate on stack_top, meanwhile -> not inc or dec stack_top, only store new data to stack_top
  //    push: first inc then store
  //     pop: first load then dec
  // readBack and spillOut operate on stack_bottom
  //    readBack: first store then dec
  //    spillout: first inc then load
  val stackPtrWidth = log2Up(GCTaskStack_Entry)
  val queuePtrWidth = 32

  val stack_top = RegInit(U(0, stackPtrWidth bits))
  val stack_bottom = RegInit(U(0, stackPtrWidth bits))

  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom = RegInit(U(0, queuePtrWidth bits))

  val task_count = RegInit(U(0, stackPtrWidth + 1 bits))
  val task_free = U(GCTaskStack_Entry - 1, stackPtrWidth + 1 bits) - task_count

  // @todo: 如果需要提高频率, 可以改为同步读
  val stack_data = Mem(UInt(GCElementWidth bits), GCTaskStack_Entry)
  val prefetched = Vec.fill(GCTaskStack_Entry)(RegInit(False))

  object overall_state extends SpinalEnum {
    val s_idle, s_work = newElement()
  }
  val state = RegInit(overall_state.s_idle)

  def stkInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskStack_Entry, step)
  def stkDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskStack_Entry, step)

  def queInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)
  def queDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, GCTaskQueue_Size, step).resize(queuePtrWidth)

  def elemAddr(idx: UInt): UInt = {
    (queue_elems_base + (idx.resize(MMUAddrWidth) << 3)).resize(MMUAddrWidth)
  }

  val stk_nextTop = stkInc(stack_top, U(1))
  val stk_prevTop = stkDec(stack_top, U(1))

  val task_empty  = stack_top === stack_bottom
  val task_exhausted = task_empty && queue_bottom === U(0)

  val need_spillOut = task_count >= U(GCTaskStack_SpillNeed + 4, task_count.getWidth bits)
  val need_readback = (task_count <= U(GCTaskStack_ReadNeed - 4, task_count.getWidth bits)) && (queue_bottom =/= U(0))

  val push_count = RegInit(U(0, 32 bits))
  val not_prefetch = RegInit(False)
  when(io.Push.fire){
    push_count := push_count + 1
    not_prefetch := True
  }
  when(io.LastPush){
    not_prefetch := False
  }
  when(io.PreFetch.fire){
    push_count := U(0)
  }
  io.PushCount := push_count

  // 从 stack_top 往前找尚未预取的任务 生成所有候选位置
  val prefetchHit = Bool()
  val prefetchIdx = UInt(stackPtrWidth bits)
  val prefetchData = cloneOf(stack_data.readAsync(stack_top))

  val candidates = Vec(Bool(), GCTaskStack_Entry-1)
  val candidateIdxs = Vec(UInt(stackPtrWidth bits), GCTaskStack_Entry-1)

  for(i <- 1 until GCTaskStack_Entry) {
    val idx = stkDec(stack_top, U(i, stackPtrWidth bits))
    candidates(i-1) := (U(i + 1, task_count.getWidth bits) < task_count) && !prefetched(idx)
    candidateIdxs(i-1) := idx
  }

  val firstValidOH = OHMasking.first(candidates.asBits)

  prefetchHit := Mux(push_count === U(0), firstValidOH.orR, True)
  prefetchIdx := Mux(push_count === U(0), MuxOH(firstValidOH, candidateIdxs), stack_top)
  prefetchData := stack_data.readAsync(prefetchIdx)

  io.Pop.valid := state === overall_state.s_work && !task_empty
  io.Pop.payload := stack_data.readAsync(stack_top)

  io.PreFetch.valid := state === overall_state.s_work && prefetchHit && !not_prefetch
  io.PreFetch.payload := prefetchData

  io.Push.ready := state === overall_state.s_work && task_free =/= U(0)

  def dbg(msg: Seq[Any]): Unit = {
    if(DebugEnable){
      report(Seq("[GCTaskStack<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  }

  def handlePushAndPop(): Unit = {
    when(io.PreFetch.fire) {
      prefetched(prefetchIdx) := True
      dbg(Seq("PreFetch, index=", prefetchIdx, " data=", prefetchData))
    }
    when(io.Push.fire && !io.Pop.fire) {
      stack_top := stk_nextTop
      task_count := task_count + U(1)
      stack_data.write(stk_nextTop, io.Push.payload)
      dbg(Seq("Push, index=", stk_nextTop, " data=", io.Push.payload))
    }.elsewhen(io.Pop.fire && !io.Push.fire) {
      stack_top := stk_prevTop
      task_count := task_count - U(1)
      prefetched(stack_top) := False
      dbg(Seq("Pop, index=", stack_top, " data=", stack_data.readAsync(stack_top)))
    }.elsewhen(io.Push.fire && io.Pop.fire) {
      stack_data.write(stack_top, io.Push.payload)
      prefetched(stack_top) := False
      dbg(Seq("Push+Pop simultaneously, index=", stack_top, " pop=", stack_data.readAsync(stack_top), " push=", io.Push.payload))
    }
  }

  val spillOutArea = new Area {
    val issued = RegInit(False)

    val sb1 = stkInc(stack_bottom, U(1))
    val sb2 = stkInc(stack_bottom, U(2))
    val sb3 = stkInc(stack_bottom, U(3))
    val sb4 = stkInc(stack_bottom, U(4))

    val e0 = stack_data.readAsync(sb1)
    val e1 = stack_data.readAsync(sb2)
    val e2 = stack_data.readAsync(sb3)
    val e3 = stack_data.readAsync(sb4)

    def run(): Unit = {
      val reqNum = U(4, 4 bits)
      val addr = elemAddr(queue_bottom)
      val packData = Cat(e3, e2, e1, e0).resize(MMUDataWidth bits).asUInt
      issueReq(io.Mreq, addr, True, reqNum * U(8), packData, issued) { _ =>
        val newQueueBottom = queInc(queue_bottom, reqNum)

        task_count := task_count - reqNum.resize(task_count.getWidth)
        stack_bottom := stkInc(stack_bottom, reqNum)
        queue_bottom := newQueueBottom

        dbg(Seq("SpillOut, moveNum=", reqNum, " old queue_bottom=", queue_bottom, " new queue_bottom=", newQueueBottom))
      }
    }
  }

  val readBackArea = new Area{
    val issued    = RegInit(False)

    def run(): Unit = {
      val wantNum = U(4, queue_bottom.getWidth bits)
      val queueAvail = queue_bottom
      val reqNum = Mux(wantNum >= queueAvail, queue_bottom, wantNum)

      // 给前台 push 留 1 个槽位，避免返回拍和 push 太紧
      val freeForReadback = Mux(task_free > U(1, task_free.getWidth bits), task_free - 1, U(0, task_free.getWidth bits))
      val canReceive = Mux(freeForReadback >= reqNum.resize(task_free.getWidth), reqNum.resize(task_free.getWidth), freeForReadback)
      val readIndex = queDec(queue_bottom, reqNum)
      val readAddr = elemAddr(readIndex)

      issueReq(io.Mreq, readAddr, False, reqNum * U(8), U(0), issued) { rd =>
        val elems = rd.subdivideIn(GCElementWidth bits)
        val newQueueBottom = queDec(queue_bottom, canReceive)

        for(i <- 0 until 4){
          when(i < canReceive){
            val writeElement = elems((reqNum - 1 - i).resized)
            val wrPtr  = stkDec(stack_bottom, U(i))
            stack_data.write(wrPtr, writeElement)
          }
        }
        stack_bottom := stkDec(stack_bottom, canReceive)
        queue_bottom := newQueueBottom
        task_count := task_count + canReceive.resize(task_count.getWidth)

        dbg(Seq("ReadBack, reqNum=", reqNum, " receive=", canReceive, " old queue_bottom=", queue_bottom, " new queue_bottom=", newQueueBottom))
      }
    }
  }

  val read_issued = RegInit(False)
  switch(state){
    is(overall_state.s_idle){
      for(i <- 0 until GCTaskStack_Entry) {
        prefetched(i) := False
      }

      read_issued            := False
      spillOutArea.issued    := False
      readBackArea.issued    := False

      stack_top    := U(0)
      stack_bottom := U(0)
      task_count   := U(0)
      queue_bottom := U(0)

      io.ConfigIO.TaskReady := True

      when(io.ConfigIO.TaskValid && io.ConfigIO.TaskReady) {
        state             := overall_state.s_work
        queue_bottom := io.ConfigIO.TaskQueue_Bottom
        queue_elems_base  := io.ConfigIO.TaskQueue_ElemsBase.resize(MMUAddrWidth)
        dbg(Seq("Config JVM Queue, Bottom=", io.ConfigIO.TaskQueue_Bottom, " ElemsBase=", io.ConfigIO.TaskQueue_ElemsBase))
      }
    }

    is(overall_state.s_work){
      when(task_exhausted && io.Pop.ready){
        io.ConfigIO.Done := True
        state := overall_state.s_idle
      }.otherwise{
        handlePushAndPop()
        when(need_spillOut){
          spillOutArea.run()
        }.elsewhen(need_readback){
          readBackArea.run()
        }
      }
    }
  }
}

object GCTaskStackVerilog extends App {
  Config.spinal.generateVerilog(new GCTaskStack())
}