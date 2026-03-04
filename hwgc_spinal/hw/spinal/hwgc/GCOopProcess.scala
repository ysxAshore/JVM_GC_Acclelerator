package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCOopProcess extends Module with HWParameters with GCParameters{
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val Process2Aop = master(new GCToAopParameters)
    val Fetch2Process = slave(new GCFetch2ProcessUnit)
    val Process2CopySurvivor = master(new GCProcess2Survivor)
    val ConfigIO = slave(new GCOopProcessConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  io.Fetch2Process.clearOut()
  io.Process2CopySurvivor.clearIn()
  io.Process2Aop.clearIn()

  object overall_state extends SpinalEnum {
    val s_idle, s_sendCopy2Survivor, s_waitDone1, s_writeTask, s_readHR, s_readHRType, s_sendAop, s_waitDone2 = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val issued = RegInit(False)

  val task = RegInit(U(0, GCElementWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord = RegInit(U(0, GCElementWidth bits))
  val klassPtr = RegInit(U(0, GCElementWidth bits))

  val heap_region = RegInit(U(0, MMUAddrWidth bits))
  val access_regionAttr = RegInit(False)
  val region_attr = RegInit(U(0, 16 bits))

  def resetState(): Unit = {
    state := overall_state.s_idle
    io.Fetch2Process.Done := True
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCOopProcess<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  switch(state){
    is(overall_state.s_idle){
      io.Fetch2Process.Ready := True
      when(io.Fetch2Process.Valid && io.Fetch2Process.Ready){
        task := io.Fetch2Process.Task
        markWord := io.Fetch2Process.MarkWord
        klassPtr := io.Fetch2Process.KlassPtr
        srcOopPtr := io.Fetch2Process.SrcOopPtr

        val doCopy2Survivor = (io.Fetch2Process.MarkWord & U(3, GCElementWidth bits)) =/= U(3, GCElementWidth bits)
        when(!doCopy2Survivor){
          destOopPtr := io.Fetch2Process.MarkWord & ~U(3, GCElementWidth bits)
          state := overall_state.s_writeTask
        }.otherwise{
          state := overall_state.s_sendCopy2Survivor
        }

        dbg(Seq("Receive task from Fetch Module, the srcOopPtr is ", io.Fetch2Process.SrcOopPtr, ", the markWord is ", io.Fetch2Process.MarkWord, ", the klassPtr is ", io.Fetch2Process.KlassPtr))
      }
    }

    is(overall_state.s_sendCopy2Survivor){
      io.Process2CopySurvivor.Valid := True
      io.Process2CopySurvivor.MarkWord := markWord
      io.Process2CopySurvivor.KlassPtr := klassPtr
      io.Process2CopySurvivor.SrcOopPtr := srcOopPtr
      io.Process2CopySurvivor.RegionAttrPtr := (io.ConfigIO.RegionAttrBiasedBase + (srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) * GCHeapRegionAttr_Size).resize(GCElementWidth)

      when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
        state := overall_state.s_waitDone1
        dbg(Seq("Send the task to Copy2Survivor"))
      }
    }

    is(overall_state.s_waitDone1){
      when(io.Process2CopySurvivor.Done){
        destOopPtr := io.Process2CopySurvivor.DestOopPtr
        state := overall_state.s_writeTask

        dbg(Seq("The Copy2Survivor Module has done, and the destOopPtr is ", io.Process2CopySurvivor.DestOopPtr))
      }
    }

    is(overall_state.s_writeTask){
      val writeObj = Mux(io.ConfigIO.UseCompressedOop, ((destOopPtr - io.ConfigIO.CompressedOopBase) >> io.ConfigIO.CompressedOopShift).resize(GCElementWidth), destOopPtr)
      val writeMask = Mux(io.ConfigIO.UseCompressedOop, getWstrb(4), getWstrb(8))
      issueReq(io.Mreq, task.resize(MMUAddrWidth), True, writeMask, writeObj, issued) { _ =>
        when((task ^ destOopPtr) >> io.ConfigIO.LogOfHRGrainBytes === U(0)){
          resetState()
        }.otherwise{
          state := overall_state.s_readHR
        }
      }
    }

    is(overall_state.s_readHR){
      val addr = (io.ConfigIO.HeapRegionBiasedBase + (task >> io.ConfigIO.HeapRegionShiftBy) * U(GCObjectPtr_Size)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
        heap_region := rd(GCElementWidth -1 downto 0)
        state := overall_state.s_readHRType
      }
    }

    is(overall_state.s_readHRType){
      val addr = heap_region + U"xbc"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
        when((rd(31 downto 0) & U(2, 32 bits)) === U(0)){
          state := overall_state.s_sendAop
        }.otherwise{
          resetState()
          dbg(Seq("The task has done"))
        }
      }
    }

    is(overall_state.s_sendAop){
      when(!access_regionAttr){
        val addr = (io.ConfigIO.RegionAttrBiasedBase + (destOopPtr >> io.ConfigIO.RegionAttrShiftBy) * GCHeapRegionAttr_Size).resize(MMUAddrWidth)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
          access_regionAttr := True
          region_attr := rd(15 downto 0)
        }
      }

      io.Process2Aop.Valid := access_regionAttr
      io.Process2Aop.Task := task
      io.Process2Aop.RegionAttr := region_attr

      when(io.Process2Aop.Valid && io.Process2Aop.Ready){
        state := overall_state.s_waitDone2
        access_regionAttr := False
        dbg(Seq("Send the task to Aop"))
      }
    }

    is(overall_state.s_waitDone2){
      when(io.Process2Aop.Done){
        resetState()
        dbg(Seq("Received the aop done, the task has done"))
      }
    }
  }
}

object GCOopProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCOopProcess())
}