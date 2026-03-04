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

  io.Fetch2Process.clearOut()
  io.Process2Trace.clearIn()

  object overall_state extends SpinalEnum {
    val s_idle, s_readSrcLen, s_readDestLen, s_readHeapRegionPtr, s_readHeapRegionType, s_doTrace, s_waitDone = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val issued = RegInit(False)

  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord = RegInit(U(0, GCElementWidth bits))

  val src_length = RegInit(U(0, 32 bits))
  val heap_region = RegInit(U(0, GCElementWidth bits))
  val scanning_in_young = RegInit(False)
  val step_index = RegInit(U(0, 32 bits))
  val dest_length = RegInit(U(0, 32 bits))
  val step_ncreate = RegInit(U(0, 32 bits))

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCArrayProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  switch(state){
    is(overall_state.s_idle){
      io.Fetch2Process.Ready := True
      when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
        oopType := io.Fetch2Process.OopType
        srcOopPtr := io.Fetch2Process.SrcOopPtr
        markWord := io.Fetch2Process.MarkWord
        destOopPtr := io.Fetch2Process.MarkWord & ~U(3, GCElementWidth bits)

        state := overall_state.s_readSrcLen

        dbg(Seq("Receive task from Fetch Module, the srcOopPtr is ", io.Fetch2Process.SrcOopPtr, ", the markWord is ", io.Fetch2Process.MarkWord))
      }
    }

    is(overall_state.s_readSrcLen){
      val addr = srcOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16))
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        src_length := rd(31 downto 0)
        state := overall_state.s_readDestLen
      }
    }

    is(overall_state.s_readDestLen){
      val addr = destOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16))
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        dest_length := rd(31 downto 0)
        state := overall_state.s_readHeapRegionPtr
      }
    }

    is(overall_state.s_readHeapRegionPtr){
      val task_num = (dest_length / io.ConfigIO.ChunkSize).resize(32)
      val remaining_tasks = ((src_length - dest_length) / io.ConfigIO.ChunkSize).resize(32)
      val max_pending = ((io.ConfigIO.StepperOffset(63 downto 32) - U(1)) * task_num + U(1)).resize(32)
      val pending = max_pending.min(remaining_tasks).min(io.ConfigIO.StepperOffset(31 downto 0))
      step_ncreate := io.ConfigIO.StepperOffset(63 downto 32).min(remaining_tasks.min(io.ConfigIO.StepperOffset(31 downto 0) + U(1)) - pending).resize(32)
      step_index := dest_length + io.ConfigIO.ChunkSize

      val addr = (io.ConfigIO.HeapRegionBiasedBase + (destOopPtr >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
        heap_region := rd(GCElementWidth - 1 downto 0)
        state := overall_state.s_readHeapRegionType
      }
    }

    is(overall_state.s_readHeapRegionType){
      val addr = heap_region + U"xbc"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
        scanning_in_young := (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)
        state := overall_state.s_doTrace
      }
    }

    is(overall_state.s_doTrace){
      io.Process2Trace.Valid := True
      io.Process2Trace.OopType := oopType
      io.Process2Trace.SrcOopPtr := srcOopPtr
      io.Process2Trace.DestOopPtr := destOopPtr
      io.Process2Trace.ScanningInYoung := scanning_in_young
      io.Process2Trace.StepIndex := step_index
      io.Process2Trace.StepNCreate := step_ncreate
      io.Process2Trace.ArrayLength := dest_length + io.ConfigIO.ChunkSize
      io.Process2Trace.PartialArrayStart := dest_length

      when(io.Process2Trace.Valid && io.Process2Trace.Ready){
        state := overall_state.s_waitDone
        dbg(Seq("This task has sent to Trace Module"))
      }
    }

    is(overall_state.s_waitDone){
      when(io.Process2Trace.Done){
        io.Fetch2Process.Done := True
        state := overall_state.s_idle

        dbg(Seq("This task done"))
      }
    }
  }
}

object GCArrayProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCArrayProcess())
}