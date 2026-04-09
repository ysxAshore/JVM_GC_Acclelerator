package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCAop extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Aop = slave (new GCToAop)
    val Mreq = master(new LocalMMUIO)
    val ToStack = master(new GCUpdatedAop)
    val ConfigIO = slave(new GCAopConfigIO)
    val TaskDone = in(Bool())
    val DebugTimeStamp = in(UInt(64 bits))
  }

  io.Aop.clearOut()

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := True

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(18)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val dest = RegInit(U(0, GCElementWidth bits))

  val res = RegInit(U(0, GCElementWidth bits))
  val card_index = RegInit(U(0, GCElementWidth bits))
  val old_node = RegInit(U(0, GCElementWidth bits))
  val new_top = RegInit(U(0, GCElementWidth bits))
  val node = RegInit(U(0, GCElementWidth bits))
  val previous_state = RegInit(overall_state.states(0))

  val byte_about_valid = RegInit(False)
  val byte_map_cache = RegInit(U(0, GCElementWidth bits))
  val byte_map_base_cache = RegInit(U(0, GCElementWidth bits))
  val last_index_valid = RegInit(False)
  val last_index_cache = RegInit(U(0, GCElementWidth bits))
  val parScanOff40_valid = RegInit(False)
  val offset40_cache = RegInit(U(0, GCElementWidth bits))
  val index_cache = RegInit(U(0, GCElementWidth bits))
  val buffer_cache = RegInit(U(0, GCElementWidth bits))
  val parScanOff20_valid = RegInit(False)
  val node_allocator_ptr_cache = RegInit(U(0, GCElementWidth bits))
  val offset30_cache = RegInit(U(0, GCElementWidth bits))
  val offset38_cache = RegInit(U(0, GCElementWidth bits))

  val wrcnt = RegInit(U(0, 3 bits))
  val wraddr = RegInit(U(0, GCElementWidth bits))
  val waitWR = RegInit(False)
  val writeBufferData = RegInit(U(0, MMUDataWidth bits))

  io.ToStack.Valid0 := last_index_valid
  io.ToStack.Addr0 := io.ConfigIO.ParScanThreadStatePtr + U"x1b0"
  io.ToStack.Data0 := last_index_cache
  io.ToStack.Valid1 := parScanOff40_valid
  io.ToStack.Addr1 := io.ConfigIO.ParScanThreadStatePtr + U"x40"
  io.ToStack.Data1 := Cat(buffer_cache, (index_cache * U(8)).resize(GCElementWidth), offset40_cache).asUInt
  io.ToStack.Valid2 := parScanOff20_valid
  io.ToStack.Addr2 := io.ConfigIO.ParScanThreadStatePtr + U"x30"
  io.ToStack.Data2 := Cat(offset38_cache, offset30_cache).asUInt
  io.ToStack.Valid3 := wrcnt =/= 0
  io.ToStack.Addr3 := wraddr
  io.ToStack.Data3 := writeBufferData

  def resetState(): Unit = {
    io.Aop.Done := True
    state := overall_state.states(0)
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCAop<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  when(io.TaskDone){
    byte_about_valid := False
    last_index_valid := False
    parScanOff40_valid := False
    parScanOff20_valid := False
    wrcnt := U(0)
  }

  switch(state) {
    is(overall_state.states(0)){
      io.Aop.Ready := True
      when(io.Aop.Ready && io.Aop.Valid){
        dest := io.Aop.Task
        when(io.Aop.RegionAttr(7 downto 0) === U(0, 8 bits)){
          resetState()
          dbg(Seq("RegionAttr=0, skip directly"))
        }.otherwise{
          when(!byte_about_valid){
            val addr = io.ConfigIO.CardTablePtr + U"x38"
            issueReq(io.Mreq, addr, False, U(16), U(0), issued) { rd =>
              byte_about_valid := True
              byte_map_cache := rd(GCElementWidth -1 downto 0)
              byte_map_base_cache := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
              state := overall_state.states(1)
            }
          }.otherwise{
            state := overall_state.states(1)
          }
        }
      }
    }

    is(overall_state.states(1)){
      val byte_map_entry = (byte_map_base_cache + (dest >> U(9))).resize(GCElementWidth)
      res := byte_map_entry
      card_index := byte_map_entry - byte_map_cache
      when(!last_index_valid) {
        val addr = io.ConfigIO.ParScanThreadStatePtr + U"x1b0"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          last_index_valid := True
          last_index_cache := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(2)
        }
      }.otherwise{
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      when(last_index_cache === card_index){
        resetState()
        dbg(Seq("last_index == card_index, done"))
      }.otherwise{
        when(!parScanOff40_valid) {
          val addr = io.ConfigIO.ParScanThreadStatePtr + U"x40"
          issueReq(io.Mreq, addr, False, U(32), U(0), issued) { rd =>
            offset40_cache := rd(GCElementWidth - 1 downto 0)
            index_cache := (rd(GCElementWidth * 2 - 1 downto GCElementWidth) / U(8)).resize(GCElementWidth)
            buffer_cache := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
            parScanOff40_valid := True
            state := overall_state.states(3)
          }
        }.otherwise{
          state := overall_state.states(3)
        }
      }
    }

    is(overall_state.states(3)){
      when(index_cache === U(0)){
        old_node := U(0)
        when(!parScanOff20_valid){
          val addr = io.ConfigIO.ParScanThreadStatePtr + U"x20"
          issueReq(io.Mreq, addr, False, U(32), U(0), issued) { rd =>
            node_allocator_ptr_cache := rd(GCElementWidth - 1 downto 0)
            offset30_cache := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
            offset38_cache := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
            parScanOff20_valid := True
            state := overall_state.states(4)
          }
        }.otherwise{
          state := overall_state.states(4)
        }
      }.otherwise{
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(4)){
      when(buffer_cache =/= U(0)){
        old_node := buffer_cache - U"x10"
        offset40_cache := offset40_cache + index_cache
        when(offset38_cache === U(0)){
          offset38_cache := buffer_cache - U"x10"
        }
        state := overall_state.states(5)
      }.otherwise{
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(5)){
      val addr = old_node
      val writeValue = Cat(offset30_cache, U(0, GCElementWidth bits)).asUInt
      issueReq(io.Mreq, addr, True, U(16), writeValue, issued){ _ =>
      }
      when(issued){
        issued := False
        offset30_cache := old_node
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(6)){
      new_top := U(0)
      val addr = node_allocator_ptr_cache + U"x80"
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        node := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      when(node =/= U(0)){
        val addr = node + U(8)
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          new_top := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(8)
        }
      }.otherwise{
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(8)){
      val addr = node + U(8)
      issueReq(io.Mreq, addr, True, U(8), U(0), issued) { _ =>
      }
      when(issued){
        issued := False
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      val addr = node_allocator_ptr_cache + U"x80"
      issueReq(io.Mreq, addr, True, U(8), new_top, issued) { _ =>
      }
      when(issued){
        issued := False
        state := Mux(node === U(0), overall_state.states(10), overall_state.states(11))
        previous_state := Mux(node === U(0), overall_state.states(10), overall_state.states(9))
      }
    }

    is(overall_state.states(10)){
      // to interrupt
      state := overall_state.states(11)
    }

    is(overall_state.states(11)){
      // @todo from interrupt get node
      when(previous_state === overall_state.states(10)){

      }.otherwise {
        buffer_cache := node + U"x10"
      }
      issueReq(io.Mreq, node_allocator_ptr_cache, False, U(8), U(0), issued) { rd =>
        index_cache := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(12)){
      last_index_cache := card_index

      val index = index_cache - U(1)
      val addr = (buffer_cache + index * U(8)).resize(MMUAddrWidth)

      val hitBoundary = addr(4 downto 0) === 0
      val isLast      = index_cache === 1
      val shouldFlush = hitBoundary || isLast

      val nextWrcnt      = wrcnt + 1
      val nextWriteValue = (writeBufferData << 64).resize(MMUDataWidth) + res
      val writeSize      = nextWrcnt * 8

      when(shouldFlush){
        issueReq(io.Mreq, addr, True, writeSize, nextWriteValue, issued) { _ =>
        }
        when(issued){
          issued           := False
          index_cache      := index
          wrcnt            := U(0)
          wraddr           := U(0)
          writeBufferData  := U(0)

          resetState()
          dbg(Seq("update last_index and sent write req, done"))
        }
      }.otherwise{
        index_cache := index
        wrcnt := nextWrcnt
        wraddr := addr
        writeBufferData := nextWriteValue

        resetState()
        dbg(Seq("update last_index, done"))
      }
    }
  }
}

object GCAopVerilog extends App{
  Config.spinal.generateVerilog(new GCAop())
}