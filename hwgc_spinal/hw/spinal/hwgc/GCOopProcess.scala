package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCOopProcess extends Module with HWParameters with GCParameters{
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val Process2Aop = master(new GCToAop)
    val Fetch2Process = slave(new GCToProcessUnit)
    val Process2CopySurvivor = master(new GCToSurvivor)
    val ConfigIO = slave(new GCOopProcessConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := False

  io.Fetch2Process.clearOut()
  io.Process2CopySurvivor.clearIn()
  io.Process2Aop.clearIn()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(9)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val task = RegInit(U(0, GCElementWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord = RegInit(U(0, GCElementWidth bits))
  val klassPtr = RegInit(U(0, GCElementWidth bits))
  val srcLength = RegInit(U(0, 32 bits))

  val heap_region = RegInit(U(0, MMUAddrWidth bits))
  val access_regionAttr = RegInit(False)
  val dest_region_attr = RegInit(U(0, 16 bits))
  val fromMarkWord = RegInit(False)

  val aop_done = RegInit(False)
  val copy2survivor_done = RegInit(False)

  val regionAttrCacheEntries = 8
  val regionAttrCacheValid = Vec(RegInit(False), regionAttrCacheEntries)
  val regionAttrCacheTag = Vec(RegInit(U(0, GCElementWidth bits)), regionAttrCacheEntries)
  val regionAttrCache = Vec(RegInit(U(0, 16 bits)), regionAttrCacheEntries)
  val regionAttrAddrLookup = (io.ConfigIO.RegionAttrBiasedBase + (srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth)

  val regionAttrHitVec = Vec(Bool(), regionAttrCacheEntries)
  for(i <- 0 until regionAttrCacheEntries){
    regionAttrHitVec(i) := regionAttrCacheValid(i) && (regionAttrCacheTag(i) === regionAttrAddrLookup)
  }
  val regionAttrHit = regionAttrHitVec.orR
  val regionAttrHitIndex = OHToUInt(regionAttrHitVec.asBits)
  val regionAttrCacheReplacePtr = RegInit(U(0, log2Up(regionAttrCacheEntries) bits))

  val heapRegionCacheEntries = 4
  val heapRegionCacheValid = Vec(RegInit(False), heapRegionCacheEntries)
  val heapRegionCacheTag = Vec(RegInit(U(0, GCElementWidth bits)), heapRegionCacheEntries)
  val heapRegionCache = Vec(RegInit(False), heapRegionCacheEntries)
  val heapRegionAddrLookup = (io.ConfigIO.HeapRegionBiasedBase + (task >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth)

  val heapRegionHitVec = Vec(Bool(), heapRegionCacheEntries)
  for(i <- 0 until heapRegionCacheEntries){
    heapRegionHitVec(i) := heapRegionCacheValid(i) && (heapRegionCacheTag(i) === heapRegionAddrLookup)
  }
  val heapRegionHit = heapRegionHitVec.orR
  val heapRegionHitIndex = OHToUInt(heapRegionHitVec.asBits)
  val heapRegionCacheReplacePtr = RegInit(U(0, log2Up(heapRegionCacheEntries) bits))

  when(io.Process2Aop.Done){
    aop_done := True
  }
  when(io.Process2CopySurvivor.Done){
    copy2survivor_done := True
    destOopPtr := io.Process2CopySurvivor.DestOopPtr
  }

  def resetState(): Unit = {
    state := overall_state.states(0)
    io.Fetch2Process.Done := True
  }

  def sendAop(): Unit = {
    when(heapRegionHit && heapRegionCache(heapRegionHitIndex)){
      resetState()
    }.otherwise{
      state := overall_state.states(8)
    }
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCOopProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  switch(state){
    is(overall_state.states(0)){
      io.Fetch2Process.Ready := True
      when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
        task := io.Fetch2Process.Task
        markWord := io.Fetch2Process.MarkWord
        klassPtr := io.Fetch2Process.KlassPtr
        srcOopPtr := io.Fetch2Process.SrcOopPtr
        srcLength := io.Fetch2Process.SrcLength

        state := overall_state.states(1)

        dbg(Seq("Receive task from Fetch Module, the srcOopPtr is ", io.Fetch2Process.SrcOopPtr, ", the markWord is ", io.Fetch2Process.MarkWord, ", the klassPtr is ", io.Fetch2Process.KlassPtr))
      }
    }

    is(overall_state.states(1)){
      when(regionAttrHit){
        state := overall_state.states(2)
      }.otherwise{
        issueReq(io.Mreq, regionAttrAddrLookup, False, U(2), U(0), issued) { rd =>
          state := overall_state.states(2)

          regionAttrCacheValid(regionAttrCacheReplacePtr) := True
          regionAttrCacheTag(regionAttrCacheReplacePtr) := regionAttrAddrLookup
          regionAttrCache(regionAttrCacheReplacePtr) := rd(15 downto 0)
          regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + 1
        }
      }
    }

    is(overall_state.states(2)){
      val src_region_attr_type = regionAttrCache(regionAttrHitIndex).asSInt
      when(src_region_attr_type < 0){
        resetState()
      }.otherwise{
        val doCopy2Survivor = (markWord & U(3, GCElementWidth bits)) =/= U(3, GCElementWidth bits)
        when(!doCopy2Survivor){
          destOopPtr := markWord & ~U(3, GCElementWidth bits)
          fromMarkWord := True
          state := overall_state.states(7)
        }.otherwise{
          fromMarkWord := False
          state := overall_state.states(3)
        }
      }
    }

    is(overall_state.states(3)){
      io.Process2CopySurvivor.Valid := True
      io.Process2CopySurvivor.MarkWord := markWord
      io.Process2CopySurvivor.KlassPtr := klassPtr
      io.Process2CopySurvivor.SrcOopPtr := srcOopPtr
      io.Process2CopySurvivor.SrcLength := srcLength
      io.Process2CopySurvivor.SrcRegionAttr := regionAttrCache(regionAttrHitIndex)
      io.Process2CopySurvivor.RegionAttrPtr :=  (io.ConfigIO.RegionAttrBiasedBase + (srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(GCElementWidth)

      when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
        state := overall_state.states(4)
        dbg(Seq("Send the task to Copy2Survivor"))
      }
    }

    is(overall_state.states(4)){
      when(heapRegionHit){
        state := overall_state.states(6)
      }.otherwise {
        issueReq(io.Mreq, heapRegionAddrLookup, False, U(8), U(0), issued) { rd =>
          heap_region := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(5)
        }
      }
    }

    is(overall_state.states(5)){
      val addr = heap_region + U"xbc"
      issueReq(io.Mreq, addr, False, U(4), U(0), issued){ rd =>
        state := overall_state.states(6)

        heapRegionCacheValid(heapRegionCacheReplacePtr) := True
        heapRegionCacheTag(heapRegionCacheReplacePtr) := heapRegionAddrLookup
        heapRegionCache(heapRegionCacheReplacePtr) := (rd(31 downto 0) & U(2, 32 bits)) =/= U(0)
        heapRegionCacheReplacePtr := heapRegionCacheReplacePtr + 1
      }
    }

    is(overall_state.states(6)){
      when(fromMarkWord){
        fromMarkWord := False
        sendAop()
      }.elsewhen(io.Process2CopySurvivor.Done || copy2survivor_done){
        copy2survivor_done := False
        destOopPtr := Mux(copy2survivor_done, destOopPtr, io.Process2CopySurvivor.DestOopPtr)
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      val writeObj = Mux(io.ConfigIO.UseCompressedOop, ((destOopPtr - io.ConfigIO.CompressedOopBase) >> io.ConfigIO.CompressedOopShift).resize(GCElementWidth), destOopPtr)
      val writeSize = Mux(io.ConfigIO.UseCompressedOop, U(4), U(8))
      issueReq(io.Mreq, task.resize(MMUAddrWidth), True, writeSize, writeObj, issued) { _ =>
        val cond = (task ^ destOopPtr) >> io.ConfigIO.LogOfHRGrainBytes === U(0)
        when(cond){
          resetState()
        }.otherwise{
          sendAop()
        }
      }
    }

    is(overall_state.states(8)){
      when(!access_regionAttr){
        val addr = (io.ConfigIO.RegionAttrBiasedBase + (destOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth)
        issueReq(io.Mreq, addr, False, U(2), U(0), issued){ rd =>
          access_regionAttr := True
          dest_region_attr := rd(15 downto 0)
        }
      }

      io.Process2Aop.Valid := access_regionAttr
      io.Process2Aop.Task := task
      io.Process2Aop.RegionAttr := dest_region_attr

      when(io.Process2Aop.Valid && io.Process2Aop.Ready){
        resetState()
      }
    }

    //is(overall_state.states(9)){
    //  when(io.Process2Aop.Done || aop_done){
    //    aop_done := False
    //    resetState()
    //  }
    //}
  }
}

object GCOopProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCOopProcess())
}