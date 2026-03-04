package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCAop extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Aop = slave (new GCToAopParameters)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCAopConfigIO)
    val DebugTimeStamp = in(UInt(64 bits))
  }

  io.Aop.clearOut()

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(18)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val dest = RegInit(U(0, GCElementWidth bits))

  val byte_map = RegInit(U(0, GCElementWidth bits))
  val byte_map_base = RegInit(U(0, GCElementWidth bits))
  val res = RegInit(U(0, GCElementWidth bits))
  val card_index = RegInit(U(0, GCElementWidth bits))
  val last_index = RegInit(U(0, GCElementWidth bits))
  val index = RegInit(U(0, GCElementWidth bits))
  val buffer = RegInit(U(0, GCElementWidth bits))
  val old_node = RegInit(U(0, GCElementWidth bits))
  val node_allocator_ptr = RegInit(U(0, GCElementWidth bits))
  val new_top = RegInit(U(0, GCElementWidth bits))
  val node = RegInit(U(0, GCElementWidth bits))
  val par_scan_off30 = RegInit(U(0, GCElementWidth bits))
  val par_scan_off38 = RegInit(U(0, GCElementWidth bits))
  val par_scan_off40 = RegInit(U(0, GCElementWidth bits))

  def resetState(): Unit = {
    io.Aop.Done := True
    state := overall_state.states(0)
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCAop<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  switch(state) {
    is(overall_state.states(0)){
      io.Aop.Ready := True
      when(io.Aop.Ready && io.Aop.Valid){
        dest := io.Aop.Task
        when(io.Aop.RegionAttr(7 downto 0) === U(0, 8 bits)){
          resetState()
          dbg(Seq("RegionAttr=0, skip directly"))
        }.otherwise{
          val addr = io.ConfigIO.CardTablePtr + U"x38"
          issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
            byte_map := rd(GCElementWidth -1 downto 0)
            byte_map_base := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
            state := overall_state.states(1)
            dbg(Seq("Read CardTable, byte_map and byte_map_base"))
          }
        }
      }
    }

    is(overall_state.states(1)){
      val byte_map_entry = (byte_map_base + (dest >> U(9))).resize(GCElementWidth)
      res := byte_map_entry
      card_index := byte_map_entry - byte_map
      val addr = io.ConfigIO.ParScanThreadStatePtr + U"x1b0"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        last_index := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(2)
        dbg(Seq("Read last_index=", rd(GCElementWidth - 1 downto 0)))
      }
    }

    is(overall_state.states(2)){
      when(last_index === card_index){
        resetState()
        dbg(Seq("last_index == card_index, done"))
      }.otherwise{
        val addr = io.ConfigIO.ParScanThreadStatePtr + U"x40"
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          par_scan_off40 := rd(GCElementWidth - 1 downto 0)
          index := (rd(GCElementWidth * 2 - 1 downto GCElementWidth) / U(8)).resize(GCElementWidth)
          buffer := rd(GCElementWidth * 4 -1 downto GCElementWidth * 3)
          state := overall_state.states(3)
        }
      }
    }

    is(overall_state.states(3)){
      when(index === U(0)){
        old_node := U(0)
        when(buffer =/= U(0)){
          old_node := buffer - U(10)
          issueReq(io.Mreq, old_node, True, getWstrb(8), U(0), issued) { _ =>
            state := overall_state.states(6)
          }
        }.otherwise{
          state := overall_state.states(6)
        }
      }.otherwise{
        index := index - U(1)
        val addr = (buffer + (index - U(1)) * U(8)).resize(MMUAddrWidth)
        issueReq(io.Mreq, addr, True, getWstrb(8), res, issued) { _ =>
          state := overall_state.states(4)
        }
      }
    }

    is(overall_state.states(4)){
      val addr = io.ConfigIO.ParScanThreadStatePtr + U"x48"
      val writeData = (index * U(8)).resize(GCElementWidth)
      issueReq(io.Mreq, addr, True, getWstrb(8), writeData, issued) { _ =>
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      val addr = io.ConfigIO.ParScanThreadStatePtr + U"x1b0"
      issueReq(io.Mreq, addr, True, getWstrb(8), card_index, issued) { _ =>
        resetState()
      }
    }

    is(overall_state.states(6)){
      val addr = io.ConfigIO.ParScanThreadStatePtr + U"x20"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        node_allocator_ptr := rd(GCElementWidth - 1 downto 0)
        par_scan_off30 := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        par_scan_off38 := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      new_top := U(0)
      val addr = node_allocator_ptr + U"x80"
      issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
        node := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(8)){
      when(node =/= U(0)){
        val addr = node + U(8)
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          new_top := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(9)
        }
      }.otherwise{
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      val addr = node_allocator_ptr + U"x80"
      issueReq(io.Mreq, addr, True, getWstrb(8), new_top, issued) { _ =>
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      when(node =/= U(0)){
        val addr = node + U(8)
        issueReq(io.Mreq, addr, True, getWstrb(8), U(0), issued) { _ =>
          state := overall_state.states(11)
        }
      }.otherwise{
        // @todo interrupt
      }
    }

    is(overall_state.states(11)){
      // @todo from interrupt get node
      val writeValue = node + U(10)
      val addr = io.ConfigIO.ParScanThreadStatePtr + U"x58"
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { _ =>
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(12)){
      issueReq(io.Mreq, node_allocator_ptr, False, U(0), U(0), issued) { rd =>
        index := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(13)){
      val addr = io.ConfigIO.ParScanThreadStatePtr + U"x48"
      val writeValue = (index * U(8)).resize(GCElementWidth)
      issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued) { _ =>
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      when(old_node === U(0)){
        state := overall_state.states(3)
      }.otherwise{
        val addr = io.ConfigIO.ParScanThreadStatePtr + U"x40"
        val writeValue = (par_scan_off40 + index).resize(GCElementWidth)
        issueReq(io.Mreq, addr, True, getWstrb(8), writeValue, issued){ _ =>
          state := overall_state.states(15)
        }
      }
    }

    is(overall_state.states(15)){
      val addr = old_node + U(8)
      issueReq(io.Mreq, addr, True, getWstrb(8), par_scan_off30, issued){ _ =>
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      val addr = io.ConfigIO.ParScanThreadStatePtr + U"x30"
      issueReq(io.Mreq, addr, True, getWstrb(8), old_node, issued) { _ =>
        state := overall_state.states(17)
      }
    }

    is(overall_state.states(17)){
      when(par_scan_off38 === U(0)){
        val addr = io.ConfigIO.ParScanThreadStatePtr + U"x38"
        issueReq(io.Mreq, addr, True, getWstrb(8), old_node, issued) { _ =>
          state := overall_state.states(3)
        }
      }.otherwise{
        state := overall_state.states(3)
      }
    }
  }
}

object GCAopVerilog extends App{
  Config.spinal.generateVerilog(new GCAop())
}