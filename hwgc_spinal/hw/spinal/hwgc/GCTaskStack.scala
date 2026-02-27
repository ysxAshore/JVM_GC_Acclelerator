package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/* GCTaskStack
 * @description: As a hardware buffer for the task queue in the JVM Hotspot
 *               in HWGC pop and push operate on stack_top
 *               in JVM spillout and readback operate on stack_bottom and queue_bottom
 * @parameters: needs Config Parameter--ageTopAddr bottomAddr and Base
 */
class GCTaskStack extends Module with GCParameters with HWParameters {
  val io = new Bundle {
    val Pop = master Stream UInt(GCElementWidth bits)
    val Push = slave Stream UInt(GCElementWidth bits)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCTaskStackConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // HWGC Queue, Sacrificing one space indicates fullness
  // push and pop operate on stack_top
  //    push: first inc then store
  //     pop: first load then dec
  // readBack and spillOut operate on stack_bottom
  //    readBack: first store then dec
  //    spillout: first inc then load
  val stackMaskWidth = log2Up(GCTaskStack_Entry)
  val stack_mask = U(GCTaskStack_Entry - 1, stackMaskWidth bits)
  val stack_top = RegInit(U(0, stackMaskWidth bits))
  val stack_bottom = RegInit(U(0, stackMaskWidth bits))
  val stack_data = Vec.fill(GCTaskStack_Entry)(RegInit(U(0, GCElementWidth bits)))

  // JVM HotSpot Queue Config
  val queue_bottom_addr = RegInit(U(0, MMUAddrWidth bits))
  val queue_elems_base = RegInit(U(0, MMUAddrWidth bits))
  val queue_bottom = RegInit(U(0, 32 bits))

  // State Machine
  // 0: Idle 1: Read 2: Work 3: End
  object overall_state extends SpinalEnum {
    val s_idle, s_read, s_work, s_end = newElement()
  }

  // 0: s0 1: s1
  object sub_state extends SpinalEnum {
    val s0, s1 = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  // these all need access memory twice
  val spillOut_sub_state = RegInit(sub_state.s0)
  val readBack_sub_state = RegInit(sub_state.s0)

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False
  io.ConfigIO.Done := False
  io.ConfigIO.TaskReady := False

  val issued_memReq = RegInit(False)

  // hwgc stack and jvm queue both are empty meanwhile task all done(fetch module ready can receive new task)
  val dispatch_done = stack_top === stack_bottom && queue_bottom === U(0) && io.Pop.ready
  // size is a power of 2 and the usage can be calculated using (top - bottom) & (size - 1)
  val task_usage = (stack_top - stack_bottom) & stack_mask
  val task_free = U(GCTaskStack_Entry - 1, stackMaskWidth bits) - task_usage

  // spillout: not need judge queue_bottom and queue_ageTop, becuase in these benchmark 1 << 17 capacity is enough
  val need_spillOut = task_usage >= U(GCTaskStack_SpillNeed)
  // readback: need judge queue_bottom and queue_ageTop, because readback can result in jvm queue empty
  val need_readback = task_usage <= U(GCTaskStack_ReadNeed) && queue_bottom =/= U(0)

  val nextTop = WrapInc(stack_top, GCTaskStack_Entry)
  val nextBottom = WrapInc(stack_bottom, GCTaskStack_Entry)
  val prevTop = WrapDec(stack_top, GCTaskStack_Entry)
  val prevBottom = WrapDec(stack_bottom, GCTaskStack_Entry)
  val nextQueueBottom = WrapInc(queue_bottom, GCTaskQueue_Size).resize(32 bits)
  val prevQueueBottom = WrapDec(queue_bottom, GCTaskQueue_Size).resize(32 bits)

  io.Pop.valid := task_usage =/= U(0)
  io.Pop.payload := stack_data(stack_top)
  io.Push.ready := state === overall_state.s_work && task_free =/= U(0) // not full

  when(state === overall_state.s_idle){
    //reset issued
    issued_memReq := False

    stack_top := U(0)
    stack_bottom := U(0)

    io.ConfigIO.TaskReady := True

    when(io.ConfigIO.TaskValid && io.ConfigIO.TaskReady){
      state := overall_state.s_read
      queue_bottom_addr := io.ConfigIO.TaskQueue_BottomAddr.resize(MMUAddrWidth bits)
      queue_elems_base := io.ConfigIO.TaskQueue_ElemsBase.resize(MMUAddrWidth bits)

      if(DebugEnable){
        report(Seq(
          "[GCTaskStack<", io.DebugTimeStamp,
          ">]Config JVM HotSpot Queue, Bottom Addr is ", io.ConfigIO.TaskQueue_BottomAddr,
          ", Elems Base is", io.ConfigIO.TaskQueue_ElemsBase,
          "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_read){
    issueReq(io.Mreq, queue_bottom_addr, False, U(0), U(0), issued_memReq) { rd =>
      state := overall_state.s_work
      queue_bottom := rd(31 downto 0)
      if(DebugEnable){
        report(Seq(
          "[GCTaskStack<", io.DebugTimeStamp,
          ">]Getched JVM HotSpot Queue, Bottom is ", queue_bottom,
          "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_work){
    // push to stack or pop from stack --- operate on stack_top
    when(dispatch_done){
      state := overall_state.s_end
    }.otherwise{
      when(io.Push.fire && !io.Pop.fire){
        stack_data(nextTop) := io.Push.payload
        stack_top := nextTop
        if(DebugEnable){
          report(Seq(
            "[GCTaskStack<", io.DebugTimeStamp,
            ">]Push to TaskStack, index is ", nextTop,
            ",data is ", io.Push.payload,
            "\n"
          ))
        }
      }.elsewhen(io.Pop.fire && !io.Push.fire){
        stack_top := prevTop
        if(DebugEnable){
          report(Seq(
            "[GCTaskStack<", io.DebugTimeStamp,
            ">]Pop from TaskStack, index is ", stack_top,
            ",data is ", stack_data(stack_top),
            "\n"
          ))
        }
      }.elsewhen(io.Pop.fire && io.Push.fire){
        stack_data(stack_top) := io.Push.payload
        if(DebugEnable){
          report(Seq(
            "[GCTaskStack<", io.DebugTimeStamp,
            ">]Push and Pop simultaneously, index is", stack_top,
            ", pop data is ", stack_data(stack_top),
            ", push data is ", io.Push.payload,
            "\n"
          ))
        }
      }

      // spill or readBack from stack --- operate on queue_bottom and stack_bottom
      // spill > readBack
      // @todo: test delete write queue_bottom
      when(need_spillOut){
        switch(spillOut_sub_state){
          is(sub_state.s0){
            issueReq(io.Mreq, (queue_elems_base + queue_bottom * GCScannerTask_Size).resize(MMUAddrWidth bits), True,  getWstrb(8), stack_data(nextBottom), issued_memReq){ rd =>
              spillOut_sub_state := sub_state.s1
              stack_bottom := nextBottom

              if(DebugEnable){
                report(Seq(
                  "[GCTaskStack<", io.DebugTimeStamp,
                  ">]SpillOut index ", nextBottom,
                  ", data ", stack_data(nextBottom),
                  ", to elems index ", queue_bottom,
                  "\n"
                ))
              }
            }
          }
          is(sub_state.s1){
            // @notice: 应该是不需要再这里填回去
            issueReq(io.Mreq, queue_bottom_addr, True, getWstrb(4), nextQueueBottom, issued_memReq) { rd =>
              queue_bottom := nextQueueBottom
              spillOut_sub_state := sub_state.s0
            }
          }
        }
      }.elsewhen(need_readback){
        switch(readBack_sub_state){
          is(sub_state.s0){
            issueReq(io.Mreq, (queue_elems_base + prevQueueBottom * GCScannerTask_Size).resize(MMUAddrWidth bits), False, U(0), U(0), issued_memReq) { rd =>
              // when readback, not push or capacity >= 2(can support one push and readback)
              when(!io.Push.fire || task_free  >= U(2)){
                stack_data(stack_bottom) := rd(GCElementWidth -1 downto 0)
              }
              readBack_sub_state := sub_state.s1
              stack_bottom := prevBottom

              if(DebugEnable){
                report(Seq(
                  "[GCTaskStack<", io.DebugTimeStamp,
                  ">]ReadBack elems index ", stack_bottom,
                  ", data ", rd,
                  ", to index ", stack_bottom,
                  ", flag ", !io.Push.fire || task_free >= U(2),
                  "\n"
                ))
              }
            }
          }
          is(sub_state.s1){
            issueReq(io.Mreq, queue_bottom_addr, True, getWstrb(4), prevQueueBottom, issued_memReq){ rd =>
              queue_bottom := prevQueueBottom
              stack_bottom := prevBottom
              readBack_sub_state := sub_state.s0
            }
          }
        }
      }
    }
  }

  when(state === overall_state.s_end){
    state := overall_state.s_idle
    io.ConfigIO.Done := True
  }
}

object GCTaskStackVerilog extends App {
  Config.spinal.generateVerilog(new GCTaskStack())
}