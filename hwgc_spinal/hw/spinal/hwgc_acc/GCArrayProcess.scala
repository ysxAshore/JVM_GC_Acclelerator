package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCArrayProcess extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val Mreq          = master(new LocalMMUIO)
    val Fetch2Process = slave(new GCToProcessUnit)
    val Process2Trace = master(new GCToTrace)
    val ConfigIO      = slave(new GCArrayProcessConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // defaults
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()

  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()

  io.Mreq.Response.ready := True

  io.Fetch2Process.clearOut()
  io.Process2Trace.clearIn()

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCArrayProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  // request state
  val issued = RegInit(False)

  // task context registers
  val oopType    = RegInit(U(0, GCOopTypeWidth bits))
  val srcOopPtr  = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord   = RegInit(U(0, GCElementWidth bits))
  val srcLength  = RegInit(U(0, 32 bits))

  val step_index  = RegInit(U(0, 32 bits))
  val dest_length = RegInit(U(0, 32 bits))
  val step_ncreate = RegInit(U(0, 32 bits))
  val heap_region = RegInit(U(0, GCElementWidth bits))

  val task_limit  = io.ConfigIO.StepperOffset(31 downto 0)
  val task_fanout = io.ConfigIO.StepperOffset(63 downto 32)

  // heap region cache
  val heapRegionCacheEntries = 4
  val heapRegionCache = Vec.fill(heapRegionCacheEntries)(RegInit(False))
  val heapRegionCacheTag = Vec.fill(heapRegionCacheEntries)(RegInit(U(0, MMUAddrWidth bits)))
  val heapRegionCacheValid = Vec.fill(heapRegionCacheEntries)(RegInit(False))
  val heapRegionCacheReplacePtr = RegInit(U(0, log2Up(heapRegionCacheEntries) bits))

  val heapRegionAddrLookup = (io.ConfigIO.HeapRegionBiasedBase + (destOopPtr >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth)

  val heapRegionHitVec = Vec.fill(heapRegionCacheEntries)(Bool())

  for (i <- 0 until heapRegionCacheEntries) {
    heapRegionHitVec(i) := heapRegionCacheValid(i) && heapRegionCacheTag(i) === heapRegionAddrLookup
  }

  val heapRegionHit = heapRegionHitVec.orR
  val heapRegionHitIndex = OHToUInt(heapRegionHitVec.asBits)

  // Main StateMachine
  val fsm = new StateMachine {
    val IDLE            = new State with EntryPoint
    val READ_DEST_LEN   = new State
    val CALC_STEP       = new State
    val LOOKUP_HEAP_REG = new State
    val READ_HUMONGOUS  = new State
    val SEND_TRACE      = new State
    val WAIT_TRACE_DONE = new State

    IDLE.whenIsActive {
      io.Fetch2Process.Ready := True

      when(io.Fetch2Process.Valid && io.Fetch2Process.Ready) {
        oopType    := io.Fetch2Process.OopType
        srcOopPtr  := io.Fetch2Process.SrcOopPtr
        markWord   := io.Fetch2Process.MarkWord
        destOopPtr := io.Fetch2Process.MarkWord & ~U(3, GCElementWidth bits)
        srcLength  := io.Fetch2Process.SrcLength

        issued := False

        goto(READ_DEST_LEN)

        dbg(Seq("Receive task from Fetch Module, the srcOopPtr is ", io.Fetch2Process.SrcOopPtr, ", the markWord is ", io.Fetch2Process.MarkWord))
      }
    }

    READ_DEST_LEN.whenIsActive {
      val addr = destOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16))

      issueReq(io.Mreq, addr.resize(MMUAddrWidth), False, U(4), U(0), issued) { rd =>
        dest_length := rd(31 downto 0)
        goto(CALC_STEP)
      }
    }

    CALC_STEP.whenIsActive {
      val task_num = (dest_length / io.ConfigIO.ChunkSize).resize(32)
      val remaining_tasks = ((srcLength - dest_length) / io.ConfigIO.ChunkSize).resize(32)
      val max_pending = ((task_fanout - U(1)) * task_num + U(1)).resize(32)
      val pending = max_pending.min(remaining_tasks).min(task_limit)
      step_ncreate := task_fanout.min(remaining_tasks.min(task_fanout + U(1)) - pending).resize(32)
      step_index := dest_length + io.ConfigIO.ChunkSize

      goto(LOOKUP_HEAP_REG)
    }

    LOOKUP_HEAP_REG.whenIsActive {
      when(heapRegionHit) {
        goto(SEND_TRACE)

      } otherwise {
        issueReq(io.Mreq, heapRegionAddrLookup, False, U(8), U(0), issued) { rd =>
          heap_region := rd(GCElementWidth - 1 downto 0)
          goto(READ_HUMONGOUS)
        }
      }
    }

    READ_HUMONGOUS.whenIsActive {
      val addr = (heap_region.resize(MMUAddrWidth) + U"xbc").resize(MMUAddrWidth)

      issueReq(io.Mreq, addr, False, U(4), U(0),issued) { rd =>
        heapRegionCacheValid(heapRegionCacheReplacePtr) := True
        heapRegionCacheTag(heapRegionCacheReplacePtr)   := heapRegionAddrLookup
        heapRegionCache(heapRegionCacheReplacePtr)      := (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)
        heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + 1

        goto(SEND_TRACE)
      }
    }

    SEND_TRACE.whenIsActive {
      io.Process2Trace.Valid := True

      io.Process2Trace.OopType := oopType
      io.Process2Trace.SrcOopPtr := srcOopPtr
      io.Process2Trace.DestOopPtr := destOopPtr
      io.Process2Trace.ScanningInYoung := heapRegionCache(heapRegionHitIndex)
      io.Process2Trace.StepIndex := step_index
      io.Process2Trace.StepNCreate := step_ncreate
      io.Process2Trace.ArrayLength := dest_length + io.ConfigIO.ChunkSize
      io.Process2Trace.PartialArrayStart := dest_length

      when(io.Process2Trace.Valid && io.Process2Trace.Ready) {
        goto(WAIT_TRACE_DONE)

        dbg(Seq("This task has sent to Trace Module"))
      }
    }

    WAIT_TRACE_DONE.whenIsActive {
      // 这里不需要缓存 Process2Trace.Done 信号
      // 因为在当前设计中，Process2Trace.Done 信号只会在 Trace Module 完成当前任务后发出一次，并且在 FSM 中，我们已经确保只有在发送了有效的 Process2Trace 信号后才会进入 WAIT_TRACE_DONE 状态。因此，在 WAIT_TRACE_DONE 状态中，我们可以直接监测 Process2Trace.Done 信号，而不需要担心它会被重复触发或者丢失
      when(io.Process2Trace.Done) {
        io.Fetch2Process.Done := True

        goto(IDLE)

        dbg(Seq("This task done"))
      }
    }
  }
}

object GCArrayProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCArrayProcess())
}