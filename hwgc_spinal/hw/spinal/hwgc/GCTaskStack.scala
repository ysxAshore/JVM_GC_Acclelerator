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
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCTaskStackConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
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
  val stackMaskWidth = log2Up(GCTaskStack_Entry)
  val stack_mask = U(GCTaskStack_Entry - 1, stackMaskWidth bits)
  val stack_top = RegInit(U(0, stackMaskWidth bits))
  val stack_bottom = RegInit(U(0, stackMaskWidth bits))
  // @notice: 根据时钟频率，如果时序违例（Timing Violation）发生在 stack_data 的 MUX 路径上 需要换到Mem类型
  // val stack_data = Vec.fill(GCTaskStack_Entry)(RegInit(U(0, GCElementWidth bits)))
  val stack_data = Mem(UInt(GCElementWidth bits), GCTaskStack_Entry)

  val queue_bottom_addr = RegInit(U(0, MMUAddrWidth bits))
  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom = RegInit(U(0, 32 bits))

  object overall_state extends SpinalEnum {
    val s_idle, s_read, s_work = newElement()
  }

  // @todo: readback和spillout的子状态机 需要第二次状态更新queue_bottom_addr数据 -> 看一下到底需不需要这个更新
  object sub_state extends SpinalEnum {
    val s0, s1 = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val stk_nextTop    = WrapInc(stack_top,    GCTaskStack_Entry)
  val stk_prevTop    = WrapDec(stack_top,    GCTaskStack_Entry)
  val stk_nextBottom = WrapInc(stack_bottom, GCTaskStack_Entry)
  val stk_prevBottom = WrapDec(stack_bottom, GCTaskStack_Entry)

  val que_nextBottom = WrapInc(queue_bottom, GCTaskQueue_Size).resize(32)
  val que_prevBottom = WrapDec(queue_bottom, GCTaskQueue_Size).resize(32)

  val task_usage = (stack_top - stack_bottom) & stack_mask
  val task_free = U(GCTaskStack_Entry - 1, stackMaskWidth bits) - task_usage
  val task_exhausted = stack_top === stack_bottom && queue_bottom === U(0)

  val need_spillOut = task_usage >= U(GCTaskStack_SpillNeed)
  val need_readback = task_usage <= U(GCTaskStack_ReadNeed) && queue_bottom =/= U(0)

  io.Pop.valid := task_usage =/= U(0)
  io.Pop.payload := stack_data.readAsync(stack_top)
  io.Push.ready := state === overall_state.s_work && task_free =/= U(0)

  def dbg(msg: Seq[Any]): Unit = {
    if(DebugEnable){
      report(Seq("[GCTaskStack<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  }

  def handlePushAndPop(): Unit = {
    when(io.Push.fire && !io.Pop.fire) {
      stack_data.write(stk_nextTop, io.Push.payload)
      stack_top := stk_nextTop
      dbg(Seq("Push, index=", stk_nextTop, " data=", io.Push.payload))
    }.elsewhen(io.Pop.fire && !io.Push.fire) {
      stack_top := stk_prevTop
      dbg(Seq("Pop, index=", stack_top, " data=", stack_data.readAsync(stack_top)))
    }.elsewhen(io.Push.fire && io.Pop.fire) {
      stack_data.write(stack_top, io.Push.payload)
      dbg(Seq("Push+Pop simultaneously, index=", stack_top, " pop=", stack_data.readAsync(stack_top), " push=", io.Push.payload))
    }
  }

  val spillOutArea = new Area {
    val spillOut_state = RegInit(sub_state.s0)
    val issued = RegInit(False)

    def run(): Unit = {
      switch(spillOut_state){
        is(sub_state.s0){
          val offset = (queue_bottom * U(GCScannerTask_Size)).resize(MMUAddrWidth)
          val addr = queue_elems_base + offset
          issueReq(io.Mreq, addr, True, getWstrb(8), stack_data.readAsync(stk_nextBottom), issued) { _ =>
            spillOut_state := sub_state.s1
            stack_bottom := stk_nextBottom
            dbg(Seq("SpillOut stack index=", stk_nextBottom, " data=", stack_data.readAsync(stk_nextBottom), " -> queue index=", queue_bottom))
          }
        }
        is(sub_state.s1){
          issueReq(io.Mreq, queue_bottom_addr, True, getWstrb(4), que_nextBottom, issued) { _ =>
            queue_bottom := que_nextBottom
            spillOut_state := sub_state.s0
          }
        }
      }
    }
  }

  val readBackArea = new Area{
    val readBack_state = RegInit(sub_state.s0)
    val issued    = RegInit(False)

    def run(): Unit = {
      switch(readBack_state) {
        is(sub_state.s0) {
          val offset   = (que_prevBottom * U(GCScannerTask_Size)).resize(MMUAddrWidth)
          val readAddr = queue_elems_base + offset

          issueReq(io.Mreq, readAddr, False, U(0), U(0), issued) { rd =>
            when(!io.Push.fire || task_free >= U(2)) { // 要么不存在Push 要么和Push同时执行 但是task mem中至少有两个空位
              stack_data.write(stack_bottom, rd(GCElementWidth - 1 downto 0))
            }
            readBack_state := sub_state.s1
            stack_bottom := stk_prevBottom
            dbg(Seq("ReadBack queue index=", que_prevBottom, " data=", rd, " -> stack index=", stack_bottom, " writeEnable=", !io.Push.fire || task_free >= U(2)))
          }
        }
        is(sub_state.s1) {
          issueReq(io.Mreq, queue_bottom_addr, True, getWstrb(4), que_prevBottom, issued) { _ =>
            queue_bottom := que_prevBottom
            readBack_state := sub_state.s0
          }
        }
      }
    }
  }

  val read_issued = RegInit(False)
  switch(state){
    is(overall_state.s_idle){
      read_issued            := False
      spillOutArea.issued    := False
      spillOutArea.spillOut_state := sub_state.s0
      readBackArea.issued    := False
      readBackArea.readBack_state := sub_state.s0
      stack_top              := U(0)
      stack_bottom           := U(0)

      io.ConfigIO.TaskReady := True

      when(io.ConfigIO.TaskValid && io.ConfigIO.TaskReady) {
        state             := overall_state.s_read
        queue_bottom_addr := io.ConfigIO.TaskQueue_BottomAddr.resize(MMUAddrWidth)
        queue_elems_base  := io.ConfigIO.TaskQueue_ElemsBase.resize(MMUAddrWidth)
        dbg(Seq("Config JVM Queue, BottomAddr=", io.ConfigIO.TaskQueue_BottomAddr, " ElemsBase=", io.ConfigIO.TaskQueue_ElemsBase))
      }
    }

    is(overall_state.s_read){
      issueReq(io.Mreq, queue_bottom_addr, False, U(0), U(0), read_issued) { rd =>
        state        := overall_state.s_work
        queue_bottom := rd(31 downto 0)
        dbg(Seq("Fetched JVM Queue Bottom=", rd(31 downto 0)))
      }
    }

    is(overall_state.s_work){
      when(task_exhausted){
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