package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCAttemptAlloc extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToNewGCAlloc = master(new GCToNewGCAlloc)
    val ToParAllocate = master(new GCToParAllocate)
    val ToAttemptAllocate = slave(new GCToAttemptAllocate)
    val ConfigIO = slave(new GCAttemptAllocConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ToNewGCAlloc.Valid := False
  io.ToNewGCAlloc.regionPtr := U(0)

  io.ToParAllocate.Valid := False
  io.ToParAllocate.excuteAll := False
  io.ToParAllocate.botUpdates := False
  io.ToParAllocate.allocRegion := U(0)
  io.ToParAllocate.minWordSize := U(0)
  io.ToParAllocate.desiredWordSize := U(0)

  io.ToAttemptAllocate.ActualPlabSize := U(0)
  io.ToAttemptAllocate.DestObjPtr := U(0)
  io.ToAttemptAllocate.Done := False
  io.ToAttemptAllocate.Ready := False

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(25)(i => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val allocRegion = RegInit(U(0, GCElementWidth bits))
  val regionPtr = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val alloc_top = RegInit(U(0, GCElementWidth bits))
  val alloc_end = RegInit(U(0, GCElementWidth bits))
  val allocated_bytes = RegInit(U(0, GCElementWidth bits))
  val new_alloc_region = RegInit(U(0, GCElementWidth bits))
  val cm = RegInit(U(0, GCElementWidth bits))
  val root_regions_array = RegInit(U(0, GCElementWidth bits))
  val next_top = RegInit(U(0, GCElementWidth bits))
  val offset_regionPtr_10 = RegInit(U(0, GCElementWidth bits))
  val offset_regionPtr_18 = RegInit(U(0, GCElementWidth bits))
  val offset_regionPtr_20 = RegInit(U(0, GCElementWidth bits))
  val offset_regionPtr_30 = RegInit(U(0, GCElementWidth bits))
  val tempValue = RegInit(U(0, GCElementWidth bits))
  val thisType = RegInit(U(0, 8 bits))

  switch(state){
    is(overall_state.states(0)){
      io.ToAttemptAllocate.Ready := True
      when(io.ToAttemptAllocate.Valid && io.ToAttemptAllocate.Ready){
        regionPtr := io.ToAttemptAllocate.regionPtr
        allocRegion := io.ToAttemptAllocate.allocRegion
        desiredWordSize := io.ToAttemptAllocate.desiredWordSize

        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      when(allocRegion =/= io.ConfigIO.DummyRegion){
        issueReq(io.Mreq, allocRegion, False, U(0), U(0), issued) { rd =>
          alloc_end := rd(GCElementWidth - 1 downto 0)
          alloc_top := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          state := overall_state.states(2)
        }
      }.otherwise{
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(2)){
      val addr = regionPtr + U"x10"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        offset_regionPtr_10 := rd(GCElementWidth - 1 downto 0)
        offset_regionPtr_18 := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        offset_regionPtr_20 := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      allocated_bytes := alloc_top - alloc_end - offset_regionPtr_18
      val addr = io.ConfigIO.G1h + U"x240"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        tempValue := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      val addr = io.ConfigIO.G1h + U"x240"
      val writeValue = tempValue + allocated_bytes
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { rd =>
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      val addr = regionPtr + U"x30"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        offset_regionPtr_30 := rd(GCElementWidth - 1 downto 0)
        thisType := rd(GCElementWidth * 2 + 7 downto GCElementWidth * 2)
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(6)){
      val addr = io.ConfigIO.G1h + Mux(thisType === U(1), U"xa0", U"x3f8") + U"x10"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        tempValue := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      val addr = io.ConfigIO.G1h + Mux(thisType === U(1), U"xa0", U"x3f8") + U"x10"
      val writeValue = tempValue + Mux(thisType === U(1), U(1), allocated_bytes)
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { rd =>
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(8)){
      val addr = io.ConfigIO.G1h + U"x3c1"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        when(rd(7 downto 0) =/= U(0) && allocated_bytes >= U(0)){
          state := overall_state.states(9)
        }.otherwise{
          state := overall_state.states(14)
        }
      }
    }

    is(overall_state.states(9)){
      val addr = io.ConfigIO.G1h + U"x4e8"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        cm := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      val addr = allocRegion + U"xe8"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        next_top := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(11)
      }
    }

    is(overall_state.states(11)){
      val addr = cm + U"xb0"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        root_regions_array := rd(GCElementWidth - 1 downto 0)
        tempValue := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(12)){
      val addr = (root_regions_array + tempValue * U(10)).resize(MMUAddrWidth bits)
      val writeOff0 = next_top
      val writeOff8 = ((alloc_top - next_top) / U(8)).resize(GCElementWidth bits)
      val writeValue = Cat(writeOff8, writeOff0).asUInt
      issueReq(io.Mreq, addr, True, getWstrb(16), writeValue, issued) { rd =>
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(13)){
      val addr = cm + U"xb0" + U"x10"
      val writeValue = tempValue + U(1)
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { rd =>
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      val addr = regionPtr + U"x8"
      val writeOff0 = io.ConfigIO.DummyRegion
      val writeOff10 = U(0)
      val writeValue = Cat(writeOff10, offset_regionPtr_10, writeOff0).asUInt
      issueReq(io.Mreq, addr, True, getWstrb(24), writeValue, issued) { rd =>
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(15)){
      io.ToNewGCAlloc.Valid := True
      io.ToNewGCAlloc.regionPtr := regionPtr
      when(io.ToNewGCAlloc.Valid && io.ToNewGCAlloc.Ready){
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      when(io.ToNewGCAlloc.Done){
        new_alloc_region := io.ToNewGCAlloc.newAllocRegion
        state := overall_state.states(17)
      }
    }

    is(overall_state.states(17)){
      when(new_alloc_region =/= U(0)){
        val addr = new_alloc_region + U"xa8"
        issueReq(io.Mreq, addr, True, getWstrb(8), U(0), issued) { rd =>
          state := overall_state.states(18)
        }
      }.otherwise{
        io.ToAttemptAllocate.Done  := True
        io.ToAttemptAllocate.DestObjPtr := U(0)
        state := overall_state.states(0)
      }
    }

    is(overall_state.states(18)){
      val off0 = alloc_end
      val off10 = alloc_top
      val writeValue = Cat(alloc_top, U(0, 64 bits), alloc_end).asUInt
      // @todo not write off8
      issueReq(io.Mreq, new_alloc_region, True, getWstrb(24), writeValue, issued) { rd =>
        state := overall_state.states(19)
      }
    }

    is(overall_state.states(19)){
      val off8 = new_alloc_region
      val off10 = Cat(offset_regionPtr_10(63 downto 32), offset_regionPtr_10(31 downto 0) + U(1)).asUInt
      val off18 = alloc_top - alloc_end
      val addr = regionPtr + U"x8"
      val writeValue = Cat(off18, off10, off8).asUInt
      issueReq(io.Mreq, addr, True, getWstrb(24), writeValue, issued) { rd =>
        state := overall_state.states(20)
      }
    }

    is(overall_state.states(20)){
      io.ToParAllocate.Valid := True
      io.ToParAllocate.excuteAll := True
      io.ToParAllocate.botUpdates := offset_regionPtr_20(0)
      io.ToParAllocate.allocRegion := allocRegion
      io.ToParAllocate.minWordSize := desiredWordSize
      io.ToParAllocate.desiredWordSize := desiredWordSize

      when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
        state := overall_state.states(21)
      }
    }

    is(overall_state.states(21)){
      // wait done -> set done
      when(io.ToParAllocate.Done){
        when(new_alloc_region =/= U(0) && io.ToParAllocate.DestObjPtr =/= U(0)){
          io.ToAttemptAllocate.ActualPlabSize := desiredWordSize
        }
        io.ToAttemptAllocate.Done  := True
        io.ToAttemptAllocate.DestObjPtr := io.ToParAllocate.DestObjPtr
        state := overall_state.states(0)
      }
    }
  }
}

object GCAttemptAllocVerilog extends App{
  Config.spinal.generateVerilog(new GCAttemptAlloc())
}