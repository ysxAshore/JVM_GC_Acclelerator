package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCArrayProcess extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val Fetch2Process = slave(new GCFetch2ProcessUnit)
    val Process2Trace = master(new GCToTrace)
    val ConfigIO = slave(new GCArrayProcessConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  io.Fetch2Process.Ready := False
  io.Fetch2Process.Done := False

  io.Process2Trace.Valid := False
  io.Process2Trace.Kid := U(0)
  io.Process2Trace.OopType := U(0)
  io.Process2Trace.KlassPtr := U(0)
  io.Process2Trace.SrcOopPtr := U(0)
  io.Process2Trace.DestOopPtr := U(0)
  io.Process2Trace.ScanningInYoung := False
  io.Process2Trace.StepIndex := U(0)
  io.Process2Trace.StepNCreate := U(0)
  io.Process2Trace.ArrayLength := U(0)
  io.Process2Trace.PartialArrayStart := U(0)

  object overall_state extends SpinalEnum {
    val s_idle, s_readSrcLen, s_readDestLen, s_readHeapRegionPtr, s_readHeapRegionType, s_doTrace, s_waitDone = newElement()
  }

  val state = RegInit(overall_state.s_idle)

  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord = RegInit(U(0, GCElementWidth bits))

  when(state === overall_state.s_idle){
    io.Fetch2Process.Ready := True
    when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
      oopType := io.Fetch2Process.OopType
      srcOopPtr := io.Fetch2Process.SrcOopPtr
      markWord := io.Fetch2Process.MarkWord
      destOopPtr := io.Fetch2Process.MarkWord & ~U(x"3", GCElementWidth bits)

      state := overall_state.s_readSrcLen

      if(DebugEnable){
        report(Seq(
          "[GCArrayProcess<", io.DebugTimeStamp,
          ">]Receive task from Fetch Module",
          ", the srcOopPtr is ", io.Fetch2Process.SrcOopPtr,
          ", the markWord is ", io.Fetch2Process.MarkWord,
          ", the destOopPtr is ", io.Fetch2Process.MarkWord & ~U"x3".resize(GCElementWidth bits),
          "\n"
        ))
      }
    }
  }

  val srcLength = RegInit(U(0, 32 bits))
  val reqIssued = RegInit(False)
  when(state === overall_state.s_readSrcLen){
    val addr = (srcOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16))).resize(MMUAddrWidth bits)
    issueReq(io.Mreq, addr, False, U(0), U(0), reqIssued) { rd =>
      srcLength := rd(31 downto 0)
      state := overall_state.s_readDestLen
    }
  }

  val stepIndex = RegInit(U(0, 32 bits))
  val destLength = RegInit(U(0, 32 bits))
  val stepNCreate = RegInit(U(0, 32 bits))
  val destLenWriteDone = RegInit(False)
  when(state === overall_state.s_readDestLen){
    val addr = (destOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16))).resize(MMUAddrWidth bits)
    issueReq(io.Mreq, addr, False, U(0), U(0), reqIssued) { rd =>
      destLength := rd(31 downto 0)
      val task_num = (rd(31 downto 0) / io.ConfigIO.ChunkSize).resize(32 bits)
      val remaining_tasks = ((srcLength - rd(31 downto 0)) / io.ConfigIO.ChunkSize).resize(32 bits)
      val max_pending = ((io.ConfigIO.StepperOffset(63 downto 32) - U(1)) * task_num + U(1)).resize(32 bits)
      val pending = max_pending.min(remaining_tasks).min(io.ConfigIO.StepperOffset(31 downto 0))
      stepNCreate := io.ConfigIO.StepperOffset(63 downto 32).min(remaining_tasks.min(io.ConfigIO.StepperOffset(31 downto 0) + U(1)) - pending).resize(32 bits)
      stepIndex := destLength + io.ConfigIO.ChunkSize
      destLenWriteDone := False
      state := overall_state.s_readHeapRegionPtr
    }
  }

  val heap_region = RegInit(U(0, GCElementWidth bits))
  when(state === overall_state.s_readHeapRegionPtr){
    val addr = (io.ConfigIO.HeapRegionBiasedBase + (destOopPtr >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth bits)
    issueReq(io.Mreq, addr, False, U(0), U(0), reqIssued){ rd =>
      heap_region := rd(GCElementWidth - 1 downto 0)
      state := overall_state.s_readHeapRegionType
    }
  }

  val scanning_in_young = RegInit(False)
  when(state === overall_state.s_readHeapRegionType){
    val addr = (heap_region + U"xbc").resize(MMUAddrWidth bits)
    issueReq(io.Mreq, addr, False, U(0), U(0), reqIssued){ rd =>
      scanning_in_young := (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)
      state := overall_state.s_doTrace
    }
  }

  when(state === overall_state.s_doTrace){
    io.Process2Trace.Valid := True
    io.Process2Trace.OopType := oopType
    io.Process2Trace.SrcOopPtr := srcOopPtr
    io.Process2Trace.DestOopPtr := destOopPtr
    io.Process2Trace.ScanningInYoung := scanning_in_young
    io.Process2Trace.StepIndex := stepIndex
    io.Process2Trace.StepNCreate := stepNCreate
    io.Process2Trace.ArrayLength := destLength + io.ConfigIO.ChunkSize
    io.Process2Trace.PartialArrayStart := destLength

    when(io.Process2Trace.Valid && io.Process2Trace.Ready){
      state := overall_state.s_waitDone

      if(DebugEnable){
        report(Seq(
          "[GCArrayProcess<", io.DebugTimeStamp,
          ">]This task has sent to Trace Module",
          "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_waitDone){
    when(io.Process2Trace.Done){
      io.Fetch2Process.Done := True

      state := overall_state.s_idle

      if(DebugEnable){
        report(Seq(
          "[GCArrayProcess<", io.DebugTimeStamp,
          ">]This task done",
          "\n"
        ))
      }
    }
  }
}

object GCArrayProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCArrayProcess())
}
