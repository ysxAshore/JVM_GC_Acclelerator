package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCArrayProcess extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Fetch2Process = slave(new GCFetch2ProcessUnit)
    val Process2Trace = master(new GCProcess2Trace)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCArrayProcessConfigIO)
  }

  // default value
  io.Fetch2Process.Ready := False
  io.Fetch2Process.Done := False
  io.Fetch2Process.DestOopPtr := U(0)

  io.Process2Trace.Valid := False
  io.Process2Trace.OopType := U(0)
  io.Process2Trace.KlassPtr := U(0)
  io.Process2Trace.SrcOopPtr := U(0)
  io.Process2Trace.DestOopPtr := U(0)
  io.Process2Trace.Kid := U(0)
  io.Process2Trace.ArrayLength := U(0)
  io.Process2Trace.PartialArrayStart := U(0)
  io.Process2Trace.StepIndex := U(0)
  io.Process2Trace.StepNCreate := U(0)
  io.Process2Trace.ScanningInYoung := False

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  object overall_state extends SpinalEnum {
    val s_idle, s_readSrcLen, s_readDestLen, s_readHeapRegionPtr, s_readHeapRegionType, s_doTrace, s_waitDone = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val pss = RegInit(U(0, MMUAddrWidth bits))
  val chunk_size = RegInit(U(0, 32 bits))
  val task_limit = RegInit(U(0, 32 bits))
  val task_fanout = RegInit(U(0, 32 bits))
  val heapRegionBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val heapRegionShiftBy = RegInit(U(0, 32 bits))
  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val srcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val destOopPtr = RegInit(U(0,MMUAddrWidth bits))
  val markWord = RegInit(U(0, MMUDataWidth bits))

  when(state === overall_state.s_idle){
    io.Fetch2Process.Ready := True
    when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
      pss := io.ConfigIO.ParScanThreadStatePtr
      chunk_size := io.ConfigIO.ChunkSize
      task_limit := io.ConfigIO.STEPPER_OFFSET(31 downto 0)
      task_fanout := io.ConfigIO.STEPPER_OFFSET(63 downto 32)
      heapRegionBiasedBase := io.ConfigIO.HeapRegionBiasedBase
      heapRegionShiftBy := io.ConfigIO.HeapRegionShiftBy

      oopType := io.Fetch2Process.OopType
      srcOopPtr := io.Fetch2Process.SrcOopPtr
      markWord := io.Fetch2Process.MarkWord
      destOopPtr := io.Fetch2Process.MarkWord & ~U(LOCK_MASK_IN_PLACE, MMUDataWidth bits)

      state := overall_state.s_readSrcLen
    }
  }

  val srcLength = RegInit(U(0, 32 bits))
  val reqIssued = RegInit(False)
  when(state === overall_state.s_readSrcLen){
    issueReq(io.Mreq, srcOopPtr + ArrayLenOff, False, U(0), U(0), reqIssued) { rd =>
      srcLength := rd(31 downto 0)
      state := overall_state.s_readDestLen
    }
  }

  val destLength = RegInit(U(0, 32 bits))
  val stepIndex = RegInit(U(0, 32 bits))
  val stepNCreate = RegInit(U(0, 32 bits))
  val destLenWriteDone = RegInit(False)
  when(state === overall_state.s_readDestLen){
    issueReq(io.Mreq, destOopPtr + ArrayLenOff, False, U(0), U(0), reqIssued) { rd =>
      destLength := rd(31 downto 0)
      val task_num = rd(31 downto 0) / chunk_size
      val remaining_tasks = (srcLength - rd(31 downto 0)) / chunk_size
      val max_pending = (task_fanout - U(1)) * task_num + U(1)
      val pending = max_pending.min(remaining_tasks).min(task_limit)
      stepNCreate := task_fanout.min(remaining_tasks.min(task_limit + U(1)) - pending).resized
      stepIndex := destLength + chunk_size
      destLenWriteDone := False
      state := overall_state.s_readHeapRegionPtr
    }
  }

  val heap_region = RegInit(U(0,MMUAddrWidth bits))
  when(state === overall_state.s_readHeapRegionPtr){
    issueReq(io.Mreq, (heapRegionBiasedBase + (destOopPtr >> heapRegionShiftBy) * GCObjectPtr_Size).resized, False, U(0), U(0), reqIssued){ rd =>
      heap_region := rd
      state := overall_state.s_readHeapRegionType
    }
  }

  val scanning_in_young = RegInit(False)
  when(state === overall_state.s_readHeapRegionType){
    issueReq(io.Mreq, heap_region + HEAP_REGION_TYPE_OFFSET, False, U(0), U(0), reqIssued){ rd =>
      scanning_in_young := (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)
      state := overall_state.s_doTrace
    }
  }

  when(state === overall_state.s_doTrace){
    io.Process2Trace.Valid := True
    io.Process2Trace.OopType := oopType
    io.Process2Trace.SrcOopPtr := srcOopPtr
    io.Process2Trace.DestOopPtr := destOopPtr
    io.Process2Trace.ArrayLength := destLength + chunk_size
    io.Process2Trace.PartialArrayStart := destLength
    io.Process2Trace.StepIndex := stepIndex
    io.Process2Trace.StepNCreate := stepNCreate
    io.Process2Trace.ScanningInYoung := scanning_in_young

    when(io.Process2Trace.Valid && io.Process2Trace.Ready){
      state := overall_state.s_waitDone
    }
  }

  when(state === overall_state.s_doTrace || state === overall_state.s_waitDone){
    when(!destLenWriteDone){
      issueReq(io.Mreq, destOopPtr + ArrayLenOff, True, halfBytesOnes, (destLength + chunk_size).resized, reqIssued) { rd =>
        destLenWriteDone := True
      }
    }
  }

  when(state === overall_state.s_waitDone){
    when(io.Process2Trace.Done && destLenWriteDone){
      io.Fetch2Process.Done := True
      state := overall_state.s_idle
    }
  }
}

object GCArrayProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCArrayProcess())
}
