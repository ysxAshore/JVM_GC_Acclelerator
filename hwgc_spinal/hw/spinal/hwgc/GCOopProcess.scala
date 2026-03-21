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
    val s_idle, s_readSrcRegionAttr, s_sendCopy2Survivor, s_waitDone1, s_writeTask, s_readHR, s_readHRType, s_sendAop, s_waitDone2 = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val issued = RegInit(False)

  val task = RegInit(U(0, GCElementWidth bits))
  val srcOopPtr = RegInit(U(0, GCElementWidth bits))
  val destOopPtr = RegInit(U(0, GCElementWidth bits))
  val markWord = RegInit(U(0, GCElementWidth bits))
  val klassPtr = RegInit(U(0, GCElementWidth bits))
  val srcLength = RegInit(U(0, 32 bits))

  val src_region_attr = RegInit(U(0, 16 bits))
  val heap_region = RegInit(U(0, MMUAddrWidth bits))
  val access_regionAttr = RegInit(False)
  val dest_region_attr = RegInit(U(0, 16 bits))

  val aop_done = RegInit(False)
  val copy2survivor_done = RegInit(False)

  when(io.Process2Aop.Done && state =/= overall_state.s_waitDone2){
    aop_done := True
  }
  when(io.Process2CopySurvivor.Done && state =/= overall_state.s_waitDone1){
    copy2survivor_done := True
    destOopPtr := io.Process2CopySurvivor.DestOopPtr
  }

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
        srcLength := io.Fetch2Process.SrcLength

        state := overall_state.s_readSrcRegionAttr

        dbg(Seq("Receive task from Fetch Module, the srcOopPtr is ", io.Fetch2Process.SrcOopPtr, ", the markWord is ", io.Fetch2Process.MarkWord, ", the klassPtr is ", io.Fetch2Process.KlassPtr))
      }
    }

    is(overall_state.s_readSrcRegionAttr){
      val addr = (io.ConfigIO.RegionAttrBiasedBase + (srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(GCElementWidth)
      issueReq(io.Mreq, addr, False, U(2), U(0), issued){ rd =>
        val src_region_attr_type = rd(15 downto 8).asSInt
        src_region_attr := rd(15 downto 0)
        when(src_region_attr_type < 0){
          resetState()
        }.otherwise{
          val doCopy2Survivor = (markWord & U(3, GCElementWidth bits)) =/= U(3, GCElementWidth bits)
          when(!doCopy2Survivor){
            destOopPtr := markWord & ~U(3, GCElementWidth bits)
            state := overall_state.s_writeTask
          }.otherwise{
            state := overall_state.s_sendCopy2Survivor
          }
        }
      }
    }

    is(overall_state.s_sendCopy2Survivor){
      io.Process2CopySurvivor.Valid := True
      io.Process2CopySurvivor.MarkWord := markWord
      io.Process2CopySurvivor.KlassPtr := klassPtr
      io.Process2CopySurvivor.SrcOopPtr := srcOopPtr
      io.Process2CopySurvivor.SrcLength := srcLength
      io.Process2CopySurvivor.SrcRegionAttr := src_region_attr
      io.Process2CopySurvivor.RegionAttrPtr :=  (io.ConfigIO.RegionAttrBiasedBase + (srcOopPtr >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(GCElementWidth)

      when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
        state := overall_state.s_waitDone1
        dbg(Seq("Send the task to Copy2Survivor"))
      }
    }

    is(overall_state.s_waitDone1){
      when(io.Process2CopySurvivor.Done || copy2survivor_done){
        copy2survivor_done := False
        destOopPtr := Mux(copy2survivor_done, destOopPtr, io.Process2CopySurvivor.DestOopPtr)
        state := overall_state.s_writeTask

        dbg(Seq("The Copy2Survivor Module has done, and the destOopPtr is ", io.Process2CopySurvivor.DestOopPtr))
      }
    }

    is(overall_state.s_writeTask){
      val writeObj = Mux(io.ConfigIO.UseCompressedOop, ((destOopPtr - io.ConfigIO.CompressedOopBase) >> io.ConfigIO.CompressedOopShift).resize(GCElementWidth), destOopPtr)
      val writeSize = Mux(io.ConfigIO.UseCompressedOop, U(4), U(8))
      issueReq(io.Mreq, task.resize(MMUAddrWidth), True, writeSize, writeObj, issued) { _ =>
        when((task ^ destOopPtr) >> io.ConfigIO.LogOfHRGrainBytes === U(0)){
          resetState()
        }.otherwise{
          state := overall_state.s_readHR
        }
      }
    }

    is(overall_state.s_readHR){
      val addr = (io.ConfigIO.HeapRegionBiasedBase + (task >> io.ConfigIO.HeapRegionShiftBy) * U(8)).resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued){ rd =>
        heap_region := rd(GCElementWidth -1 downto 0)
        state := overall_state.s_readHRType
      }
    }

    is(overall_state.s_readHRType){
      val addr = heap_region + U"xbc"
      issueReq(io.Mreq, addr, False, U(4), U(0), issued){ rd =>
        when((rd(31 downto 0) & U(2, 32 bits)) === U(0)){
          state := overall_state.s_sendAop
        }.otherwise{
          resetState()
        }
      }
    }

    is(overall_state.s_sendAop){
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
        state := overall_state.s_waitDone2
        access_regionAttr := False
      }
    }

    is(overall_state.s_waitDone2){
      when(io.Process2Aop.Done || aop_done){
        aop_done := False
        resetState()
      }
    }
  }
}

object GCOopProcessVerilog extends App {
  Config.spinal.generateVerilog(new GCOopProcess())
}