package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
class GCAllocate extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToAllocate = slave(new GCToAllocate)
    val ToParAllocate = master(new GCToParAllocate)
    val ToAttempAllocate = master(new GCToAttemptAllocate)
    val ConfigIO = slave(new GCAllocateConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ToAllocate.Ready := False
  io.ToAllocate.Done := False
  io.ToAllocate.DestObjPtr := U(0)

  io.ToParAllocate.Valid := False
  io.ToParAllocate.excuteAll := False
  io.ToParAllocate.botUpdates := False
  io.ToParAllocate.allocRegion := U(0)
  io.ToParAllocate.minWordSize := U(0)
  io.ToParAllocate.desiredWordSize := U(0)

  io.ToAttempAllocate.Valid := False

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(25)(i => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val size = RegInit(U(0, 32 bits))
  val destAttrType = RegInit(U(0, 8 bits))

  val destObjPtr = RegInit(U(0, GCElementWidth bits))
  val actualPlabSize = RegInit(U(0, GCElementWidth bits))

  val tempValue = RegInit(U(0, GCElementWidth bits))
  val plabWordSize = RegInit(U(0, GCElementWidth bits))
  val requiredInPlab = RegInit(U(0, GCElementWidth bits))
  val allocatorPtr = RegInit(U(0, GCElementWidth bits))
  val offsetPlabAllocator10 = RegInit(U(0, GCElementWidth bits))
  val offsetPlabAllocator18 = RegInit(U(0, GCElementWidth bits))
  val buffer = RegInit(U(0, GCElementWidth bits))
  val region_top = RegInit(U(0, GCElementWidth bits))
  val region_end = RegInit(U(0, GCElementWidth bits))
  val offsetBuffer48 = RegInit(U(0, GCElementWidth bits))

  val duringGCSelect = RegInit(False)
  val minWordSize = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val regionPtr = RegInit(U(0, GCElementWidth bits))
  val allocRegion = RegInit(U(0, GCElementWidth bits))

  switch(state){
    is(overall_state.states(0)){
      io.ToAllocate.Ready := True
      when(io.ToAllocate.Valid && io.ToAllocate.Ready){
        size := io.ToAllocate.Size
        destAttrType := io.ToAllocate.DestAttrType
        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      val addr = (io.ConfigIO.G1h + Mux(destAttrType === U(0), U"x250", U"x2e0") + U"x30").resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        tempValue := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      plabWordSize := Min(Max(tempValue, U"x102"), U"x400")
      requiredInPlab := (size + U(2)).resize(GCElementWidth bits)

      val addr = (io.ConfigIO.PlabAllocatorPtr + U"x8").resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        allocatorPtr := rd(GCElementWidth - 1 downto 0)
        offsetPlabAllocator10 := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        offsetPlabAllocator18 := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)

        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      val cond = requiredInPlab * U(100) < plabWordSize * U(10)
      when(requiredInPlab <= plabWordSize && cond){
        val addr = Mux(destAttrType === U(0), offsetPlabAllocator10, offsetPlabAllocator18)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          buffer := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(4)
        }
      }.otherwise{
        duringGCSelect := True
        minWordSize := size.resize(GCElementWidth bits)
        desiredWordSize := size.resize(GCElementWidth bits)
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(4)){
      val addr = (buffer + U"x30").resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        region_top := rd(GCElementWidth - 1 downto 0)
        region_end := rd(GCElementWidth * 3 - 1  downto GCElementWidth * 2)
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      when(region_top < region_end){
        val words = (region_end - region_top) / U(8)
        val headSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(2), U(3))
        val temp = Mux(words >= headSize, io.ConfigIO.intArrayKlassObj, io.ConfigIO.objectKlassObj)
        val writeOff0 = U(1)
        val writeOff8 = Mux(io.ConfigIO.UseCompressedKlassPointers, ((temp - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64 bits), temp)
        val writeLen = (words - headSize) * U(2)
        val writeValue = Mux(io.ConfigIO.UseCompressedKlassPointers, Cat(writeLen.resize(32 bits), writeOff8.resize(32 bits), writeOff0.resize(64 bits)).resize(MMUDataWidth bits), Cat(writeLen.resize(32 bits), writeOff8.resize(64 bits), writeOff0.resize(64 bits)).resize(MMUDataWidth bits))
        val writeMask = Mux(words >= headSize && io.ConfigIO.UseCompressedKlassPointers, getWstrb(16),
                        Mux(words >= headSize, getWstrb(20),
                        Mux(io.ConfigIO.UseCompressedKlassPointers, getWstrb(12), getWstrb(16))))
        issueReq(io.Mreq, region_top, True, writeMask, writeValue.asUInt, issued) { rd =>
          state := overall_state.states(6)
        }
      }.otherwise{
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(6)){
      val addr = (buffer + U"x28").resize(MMUAddrWidth bits)
      val writeValue = Cat(region_end, region_end, region_end).asUInt
      issueReq(io.Mreq, addr, True, getWstrb(24), writeValue, issued) { rd =>
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      val addr = (buffer + U"x48").resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        offsetBuffer48 := rd(GCElementWidth - 1 downto 0)
        tempValue := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(8)){
      val addr = (buffer + U"x50").resize(MMUAddrWidth bits)
      val writeValue = (tempValue + (region_end - region_top) / U(8)).resize(GCElementWidth bits)
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { rd =>
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      var addr = (io.ConfigIO.PlabAllocatorPtr + U"x30" + destAttrType * U(8)).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        tempValue := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      var addr = (io.ConfigIO.PlabAllocatorPtr + U"x30" + destAttrType * U(8)).resize(MMUAddrWidth bits)
      val writeValue = tempValue + U(1)
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { rd =>
        duringGCSelect := False
        minWordSize := requiredInPlab
        desiredWordSize := plabWordSize
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(11)){
      when(destObjPtr =/= U(0)){
        val off20 = actualPlabSize
        val off28 = destObjPtr
        val off30 = Mux(actualPlabSize - U(2) >= size, (destObjPtr + size * U(8)).resize(GCElementWidth bits), destObjPtr)
        val off38 = (destObjPtr + (actualPlabSize - U(2)) * U(8)).resize(GCElementWidth bits)
        val writeValue = Cat(off38, off30, off28, off20).asUInt
        val addr = (buffer + U"x20").resize(MMUAddrWidth bits)
        issueReq(io.Mreq, addr, True, getWstrb(32), writeValue, issued) { rd =>
          state := overall_state.states(12)
        }
      }.otherwise{
        duringGCSelect := True
        minWordSize := size.resize(GCElementWidth bits)
        desiredWordSize := size.resize(GCElementWidth bits)
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(12)){
      val off40 = (destObjPtr + actualPlabSize * U(8)).resize(GCElementWidth bits)
      val off48 = offsetBuffer48 + actualPlabSize
      val writeValue = Cat(off48, off40).asUInt
      val addr = (buffer + U"x40").resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, True, getWstrb(16), writeValue, issued) { rd =>
        io.ToAllocate.Done := True
        io.ToAllocate.DestObjPtr := Mux(actualPlabSize - U(2) < size, U(0), destObjPtr)
        state := overall_state.states(0)
      }
    }

    is(overall_state.states(13)){
      when(destAttrType === U(0)){
        val addr = allocatorPtr + U"x28"
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          regionPtr := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(14)
        }
      }.otherwise{
        regionPtr := allocatorPtr + U"x30"
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      val addr = regionPtr + U"x8"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        allocRegion := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(15)){
      io.ToParAllocate.Valid := True
      io.ToParAllocate.excuteAll := destAttrType =/= U(0)
      io.ToParAllocate.botUpdates := True
      io.ToParAllocate.allocRegion := allocRegion
      io.ToParAllocate.minWordSize := minWordSize
      io.ToParAllocate.desiredWordSize := desiredWordSize

      when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      when(io.ToParAllocate.Done){
        destObjPtr := io.ToParAllocate.DestObjPtr
        actualPlabSize := io.ToParAllocate.ActualPlabSize
        state := overall_state.states(17)
      }
    }

    is(overall_state.states(17)){
      when(destObjPtr === U(0)){
        val addr = allocatorPtr + U"x10"
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          tempValue := rd(7 downto 0).resize(GCElementWidth bits)
          state := overall_state.states(18)
        }
      }.otherwise{
        when(duringGCSelect){
          state := overall_state.states(0)
          io.ToAllocate.Done := True
          io.ToAllocate.DestObjPtr := destObjPtr
        }.otherwise{
          state := overall_state.states(11)
        }
      }
    }

    is(overall_state.states(18)){
      val is_full = Mux(destAttrType === U(0), tempValue(0), tempValue(1))
      when(is_full){
        when(duringGCSelect){
          state := overall_state.states(0)
          io.ToAllocate.Done := True
          io.ToAllocate.DestObjPtr := destObjPtr
        }.otherwise{
          state := overall_state.states(11)
        }
      }.otherwise{
        issueReq(io.Mreq, io.ConfigIO.LockPtr, True, getWstrb(8), io.ConfigIO.Thread, issued) { rd =>
          state := overall_state.states(19)
        }
      }
    }

    is(overall_state.states(19)){
      io.ToParAllocate.Valid := True
      io.ToParAllocate.excuteAll := destAttrType =/= U(0)
      io.ToParAllocate.botUpdates := True
      io.ToParAllocate.allocRegion := allocRegion
      io.ToParAllocate.minWordSize := minWordSize
      io.ToParAllocate.desiredWordSize := desiredWordSize

      when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(20)){
      when(io.ToParAllocate.Done){
        destObjPtr := io.ToParAllocate.DestObjPtr
        actualPlabSize := io.ToParAllocate.ActualPlabSize
        state := overall_state.states(21)
      }
    }

    is(overall_state.states(21)){
      when(destObjPtr === U(0)){
        //send attempt alloc
      }.otherwise{
        state := overall_state.states(24)
      }
    }

    is(overall_state.states(22)){
      when(io.ToAttempAllocate.Done){
        destObjPtr := io.ToAttempAllocate.DestObjPtr
        actualPlabSize := io.ToAttempAllocate.ActualPlabSize
        state := overall_state.states(23)
      }
    }

    is(overall_state.states(23)){
      val addr = Mux(destAttrType === U(0), allocatorPtr + U"x10", allocatorPtr + U"x11")
      issueReq(io.Mreq, addr, True, getWstrb(1), U(1), issued) { rd =>
        state := overall_state.states(24)
      }
    }

    is(overall_state.states(24)){
      issueReq(io.Mreq, io.ConfigIO.LockPtr, True, getWstrb(8), U(0), issued) { rd =>
        when(duringGCSelect){
          state := overall_state.states(0)
          io.ToAllocate.Done := True
          io.ToAllocate.DestObjPtr := destObjPtr
        }.otherwise{
          state := overall_state.states(11)
        }
      }
    }
  }
}

object GCAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCAllocate())
}
