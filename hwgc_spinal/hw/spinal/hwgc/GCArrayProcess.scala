package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCArrayProcess extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val Fetch2Process = slave(new GCToProcessUnit)
    val Process2Trace = master(new GCToTrace)
    val ConfigIO = slave(new GCArrayProcessConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := True

  io.Fetch2Process.clearOut()
  io.Process2Trace.clearIn()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(7)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val oopType = RegInit(U(0, GCOopTypeWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord = RegInit(U(0, GCElementWidth bits))
  val srcLength = RegInit(U(0, 32 bits))

  val step_index = RegInit(U(0, 32 bits))
  val dest_length = RegInit(U(0, 32 bits))
  val step_ncreate = RegInit(U(0, 32 bits))
  val heap_region = RegInit(U(0, GCElementWidth bits))

  val task_limit = io.ConfigIO.StepperOffset(31 downto 0)
  val task_fanout = io.ConfigIO.StepperOffset(63 downto 32)

  val trace_done = RegInit(False)

  val heapRegionCacheEntries = 4
  val heapRegionCacheValid = Vec(RegInit(False), heapRegionCacheEntries)
  val heapRegionCacheTag = Vec(RegInit(U(0, GCElementWidth bits)), heapRegionCacheEntries)
  val heapRegionCache = Vec(RegInit(False), heapRegionCacheEntries)
  val heapRegionAddrLookup = (io.ConfigIO.HeapRegionBiasedBase + (destOopPtr >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth)

  val heapRegionHitVec = Vec(Bool(), heapRegionCacheEntries)
  for(i <- 0 until heapRegionCacheEntries){
    heapRegionHitVec(i) := heapRegionCacheValid(i) && (heapRegionCacheTag(i) === heapRegionAddrLookup)
  }
  val heapRegionHit = heapRegionHitVec.orR
  val heapRegionHitIndex = OHToUInt(heapRegionHitVec.asBits)
  val heapRegionCacheReplacePtr = RegInit(U(0, log2Up(heapRegionCacheEntries) bits))

  when(io.Process2Trace.Done){
    trace_done := True
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCArrayProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  switch(state){
    is(overall_state.states(0)){
      io.Fetch2Process.Ready := True
      when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
        oopType := io.Fetch2Process.OopType
        srcOopPtr := io.Fetch2Process.SrcOopPtr
        markWord := io.Fetch2Process.MarkWord
        destOopPtr := io.Fetch2Process.MarkWord & ~U(3, GCElementWidth bits)
        srcLength := io.Fetch2Process.SrcLength

        state := overall_state.states(1)

        dbg(Seq("Receive task from Fetch Module, the srcOopPtr is ", io.Fetch2Process.SrcOopPtr, ", the markWord is ", io.Fetch2Process.MarkWord))
      }
    }

    is(overall_state.states(1)){
      val addr = destOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16))
      issueReq(io.Mreq, addr, False, U(4), U(0), issued) { rd =>
        dest_length := rd(31 downto 0)
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      val task_num = (dest_length / io.ConfigIO.ChunkSize).resize(32)
      val remaining_tasks = ((srcLength - dest_length) / io.ConfigIO.ChunkSize).resize(32)
      val max_pending = ((task_fanout - U(1)) * task_num + U(1)).resize(32)
      val pending = max_pending.min(remaining_tasks).min(task_limit)
      step_ncreate := task_fanout.min(remaining_tasks.min(task_fanout + U(1)) - pending).resize(32)
      step_index := dest_length + io.ConfigIO.ChunkSize

      state := overall_state.states(3)
    }

    is(overall_state.states(3)){
      when(heapRegionHit){
        state := overall_state.states(5)
      }.otherwise {
        issueReq(io.Mreq, heapRegionAddrLookup, False, U(8), U(0), issued) { rd =>
          heap_region := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(4)
        }
      }
    }

    is(overall_state.states(4)){
      val addr = heap_region + U"xbc"
      issueReq(io.Mreq, addr, False, U(4), U(0), issued){ rd =>

        heapRegionCacheValid(heapRegionCacheReplacePtr) := True
        heapRegionCacheTag(heapRegionCacheReplacePtr) := heapRegionAddrLookup
        heapRegionCache(heapRegionCacheReplacePtr) := (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)
        heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + 1
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      io.Process2Trace.Valid := True
      io.Process2Trace.OopType := oopType
      io.Process2Trace.SrcOopPtr := srcOopPtr
      io.Process2Trace.DestOopPtr := destOopPtr
      io.Process2Trace.ScanningInYoung := heapRegionCache(heapRegionHitIndex)
      io.Process2Trace.StepIndex := step_index
      io.Process2Trace.StepNCreate := step_ncreate
      io.Process2Trace.ArrayLength := dest_length + io.ConfigIO.ChunkSize
      io.Process2Trace.PartialArrayStart := dest_length

      when(io.Process2Trace.Valid && io.Process2Trace.Ready){
        state := overall_state.states(6)
        dbg(Seq("This task has sent to Trace Module"))
      }
    }

    is(overall_state.states(6)){
      when(io.Process2Trace.Done || trace_done){
        trace_done := False
        io.Fetch2Process.Done := True
        state := overall_state.states(0)

        dbg(Seq("This task done"))
      }
    }
  }
}

object GCArrayProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCArrayProcess())
}