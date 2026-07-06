package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCAop extends Module with GCTopParameters with HWParameters {
  val io = new Bundle {
    val Aop            = slave(new GCToAop)
    val Mreq           = master(new LocalMMUIO)
    val ConfigIO       = slave(new GCAopConfigIO)
    val NoAopSrc       = in(Bool())
    val DebugTimeStamp = in(UInt(64 bits))
  }

  // Defaults
  io.Aop.clearOut()

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True

  def dbg(msg: Seq[Any]): Unit = {
    if (DebugEnable) {
      report(Seq("[GCAop<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }
  }

  def pssAddr(offset: UInt): UInt = (io.ConfigIO.ParScanThreadStatePtr + offset).resize(MMUAddrWidth)

  // MMU Request state
  val issued = RegInit(False)

  // Task context
  val dest       = RegInit(U(0, GCElementWidth bits))
  val res        = RegInit(U(0, GCElementWidth bits))
  val card_index = RegInit(U(0, GCElementWidth bits))

  // Buffer-node allocation context
  val old_node = RegInit(U(0, GCElementWidth bits))
  val new_top  = RegInit(U(0, GCElementWidth bits))

  // Local caches
  val byte_about_valid    = RegInit(False)
  val byte_map_cache      = RegInit(U(0, GCElementWidth bits))
  val byte_map_base_cache = RegInit(U(0, GCElementWidth bits))

  val last_index_valid = RegInit(False)
  val last_index_cache = RegInit(U(0, GCElementWidth bits))

  val parScanOff40_valid = RegInit(False)
  val index_cache        = RegInit(U(0, GCElementWidth bits))
  val temp_cache         = RegInit(U(0, GCElementWidth bits))
  val buffer_cache       = RegInit(U(0, GCElementWidth bits))

  val parScanOff20_valid       = RegInit(False)
  val node_allocator_ptr_cache = RegInit(U(0, GCElementWidth bits))
  val offset30_cache           = RegInit(U(0, GCElementWidth bits))
  val offset38_cache           = RegInit(U(0, GCElementWidth bits))

  val wrcnt           = RegInit(U(0, 3 bits))
  val wraddr          = RegInit(U(0, GCElementWidth bits))
  val writeBufferData = RegInit(U(0, MMUDataWidth bits)) // 先得到高位数据 再得到低位数据

  // NoAopSrc handling
  // NoAopSrc 和 Aop.Ready 可能不同拍有效。 所以这里锁存 NoAopSrc。
  // 等 FSM 回到 IDLE，并且 io.Aop.Valid 也没有挂起时，再执行 writeback。
  val noAopSrcLast    = RegNext(io.NoAopSrc) init False
  val noAopSrcRise    = io.NoAopSrc && !noAopSrcLast
  val noAopSrcPending = RegInit(False)

  val clearNoAopSrcPending = Bool()
  clearNoAopSrcPending := False

  // Main FSM
  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint

    val READ_BYTE_MAP    = new State
    val CALC_CARD        = new State
    val CHECK_LAST_INDEX = new State
    val CHECK_INDEX     = new State

    val LINK_OLD_BUFFER = new State
    val WRITE_OLD_NODE  = new State

    val ALLOC_READ_FREE_LIST = new State
    val ALLOC_READ_NEW_TOP   = new State
    val ALLOC_WRITE_FREE_TOP = new State
    val ALLOC_CLEAR_NEXT     = new State
    val ALLOC_SLOW_RETRY     = new State
    val READ_NODE_CAPACITY   = new State

    val ENQUEUE_RES = new State

    val WB_FLUSH_BUFFER_WRITE = new State
    val WB_WRITE_INDEX_BUFFER = new State
    val WB_WRITE_OFFSET30_38  = new State
    val WB_WRITE_LAST_INDEX   = new State
    val WB_CLEAR_CACHE        = new State

    IDLE.whenIsActive {
      val noAopSeen = noAopSrcPending || noAopSrcRise

      io.Aop.cmd.ready := True

      // 如果 noAopSeen 和 io.Aop.Valid 同拍有效，优先接收这个已经挂在接口上的 Aop。
      when(io.Aop.cmd.valid) {
        dest   := io.Aop.cmd.payload.Task
        issued := False

        when(io.Aop.cmd.payload.RegionAttr(7 downto 0) === U(0, 8 bits)) {
          dbg(Seq("RegionAttr=0, skip directly"))
          io.Aop.Done := True
        } otherwise {
          when(!byte_about_valid) {
            goto(READ_BYTE_MAP)
          } otherwise {
            goto(CALC_CARD)
          }
        }

      } elsewhen noAopSeen {
        io.Aop.cmd.ready := False
        issued := False
        goto(WB_FLUSH_BUFFER_WRITE)
      }
    }

    READ_BYTE_MAP.whenIsActive {
      val addr = (io.ConfigIO.CardTablePtr + U"x38").resize(MMUAddrWidth)

      issueReq(io.Mreq, addr, False, U(16), U(0), True, False, issued) { rd =>
        byte_about_valid    := True
        byte_map_cache      := rd(GCElementWidth - 1 downto 0)
        byte_map_base_cache := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

        dbg(Seq("read byte map cache"))
        goto(CALC_CARD)
      }
    }

    CALC_CARD.whenIsActive {
      val byte_map_entry = (byte_map_base_cache + (dest >> U(9))).resize(GCElementWidth)
      res        := byte_map_entry
      card_index := byte_map_entry - byte_map_cache

      when(!last_index_valid) {
        val addr = pssAddr(U"x1b0")

        issueReq(io.Mreq, addr, False, U(8), U(0), True, False, issued) { rd =>
          last_index_valid := True
          last_index_cache := rd(GCElementWidth - 1 downto 0)

          dbg(Seq("read last_index_cache"))
          goto(CHECK_LAST_INDEX)
        }
      } otherwise {
        goto(CHECK_LAST_INDEX)
      }
    }

    CHECK_LAST_INDEX.whenIsActive {
      when(last_index_cache === card_index) {
        dbg(Seq("last_index == card_index, done"))
        io.Aop.Done := True
        goto(IDLE)
      } otherwise {
        when(!parScanOff40_valid) {
          val addr = pssAddr(U"x48")

          issueReq(io.Mreq, addr, False, U(24), U(0), True, False, issued) { rd =>
            index_cache := rd(GCElementWidth - 1 downto 0)
            temp_cache := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
            buffer_cache := rd(GCElementWidth * 3  - 1 downto GCElementWidth * 2)

            parScanOff40_valid := True

            dbg(Seq("read parScan offset40/index/buffer cache"))
            goto(CHECK_INDEX)
          }
        } otherwise {
          goto(CHECK_INDEX)
        }
      }
    }

    CHECK_INDEX.whenIsActive {
      when(index_cache === U(0)) {
        when(!parScanOff20_valid) {
          val addr = pssAddr(U"x20")
          issueReq(io.Mreq, addr, False, U(32), U(0), True, False, issued) { rd =>
            node_allocator_ptr_cache := rd(GCElementWidth - 1 downto 0)
            offset30_cache           := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
            offset38_cache           := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)

            parScanOff20_valid := True

            dbg(Seq("read parScan offset20/30/38 cache"))
            goto(LINK_OLD_BUFFER)
          }
        } otherwise {
          goto(LINK_OLD_BUFFER)
        }
      } otherwise {
        goto(ENQUEUE_RES)
      }
    }

    LINK_OLD_BUFFER.whenIsActive {
      when(buffer_cache =/= U(0)) {
        old_node := buffer_cache - U"x10"
        when(offset38_cache === U(0)){
          offset38_cache := buffer_cache - U"x10"
        }
        goto(WRITE_OLD_NODE)
      } otherwise {
        goto(ALLOC_READ_FREE_LIST)
      }
    }

    WRITE_OLD_NODE.whenIsActive {
      val addr       = old_node.resize(MMUAddrWidth)
      val writeValue = Cat(offset30_cache, U(0, GCElementWidth bits)).asUInt.resize(MMUDataWidth)

      issueReq(io.Mreq, addr, True, U(16), writeValue, False, False, issued) { _ => }

      when(issued) {
        issued := False
        offset30_cache := old_node

        dbg(Seq("write old buffer node link"))
        goto(ALLOC_READ_FREE_LIST)
      }
    }

    ALLOC_READ_FREE_LIST.whenIsActive {
      new_top := U(0)
      val addr = (node_allocator_ptr_cache + U"x80").resize(MMUAddrWidth)
      issueReq(io.Mreq, addr, False, U(8), U(0), True, False, issued) { rd =>
        old_node := rd(GCElementWidth - 1 downto 0)

        when(rd(GCElementWidth - 1 downto 0) =/= U(0)) {
          dbg(Seq("buffer_node_allocate: got node from free list"))
          goto(ALLOC_READ_NEW_TOP)
        } otherwise {
          dbg(Seq("buffer_node_allocate: free list empty, retry slow path"))
          goto(ALLOC_WRITE_FREE_TOP)
        }
      }
    }

    ALLOC_READ_NEW_TOP.whenIsActive {
      val addr = (old_node + U(8)).resize(MMUAddrWidth)

      issueReq(io.Mreq, addr, False, U(8), U(0), True, False, issued) { rd =>
        new_top := rd(GCElementWidth - 1 downto 0)

        dbg(Seq("buffer_node_allocate: read new_top"))
        goto(ALLOC_WRITE_FREE_TOP)
      }
    }

    ALLOC_WRITE_FREE_TOP.whenIsActive {
      val addr       = (node_allocator_ptr_cache + U"x80").resize(MMUAddrWidth)
      val writeValue = new_top.resize(MMUDataWidth)

      issueReq(io.Mreq, addr, True, U(8), writeValue, False, False, issued) { _ => }

      when(issued) {
        issued := False

        dbg(Seq("buffer_node_allocate: write free_list top"))
        when(old_node === U(0)) {
          goto(ALLOC_SLOW_RETRY)
        }.otherwise{
          goto(ALLOC_CLEAR_NEXT)
        }
      }
    }

    ALLOC_CLEAR_NEXT.whenIsActive {
      val addr = (old_node + U(8)).resize(MMUAddrWidth)

      // *(uintptr_t *)(node + 0x8) = 0
      issueReq(io.Mreq, addr, True, U(8), U(0), False, False, issued) { _ => }

      when(issued) {
        issued := False

        buffer_cache := old_node + U"x10"

        dbg(Seq("buffer_node_allocate: clear node next"))
        goto(READ_NODE_CAPACITY)
      }
    }

    ALLOC_SLOW_RETRY.whenIsActive {
      // @todo interrupt to call BufferNode::allocate to return old_node
      goto(ALLOC_READ_FREE_LIST)
    }

    READ_NODE_CAPACITY.whenIsActive {
      val addr = node_allocator_ptr_cache.resize(MMUAddrWidth)

      issueReq(io.Mreq, addr, False, U(8), U(0), True, False, issued) { rd =>
        index_cache := (rd(GCElementWidth - 1 downto 0) << 3).resize(GCElementWidth)

        dbg(Seq("read node capacity"))
        goto(ENQUEUE_RES)
      }
    }

    ENQUEUE_RES.whenIsActive {
      last_index_cache := card_index

      val index = (index_cache >> 3) - U(1)
      val addr  = (buffer_cache + index * U(8)).resize(MMUAddrWidth)

      val hitBoundary = addr(4 downto 0) === U(0, 5 bits)
      val isLast      = index_cache === U(1)
      val shouldFlush = hitBoundary || isLast

      val nextWrcnt      = wrcnt + U(1)
      val nextWriteValue = ((writeBufferData << 64).resize(MMUDataWidth) | res.resize(MMUDataWidth)).resize(MMUDataWidth)
      val writeSize      = nextWrcnt * U(8)

      when(shouldFlush) {
        issueReq(io.Mreq, addr, True, writeSize, nextWriteValue, False, False, issued) { _ => }

        when(issued) {
          issued := False
          index_cache     := index_cache - U(8)
          wrcnt           := U(0)
          wraddr          := U(0)
          writeBufferData := U(0)

          dbg(Seq("enqueue res and flush aligned write"))
          io.Aop.Done := True
          goto(IDLE)
        }

      } otherwise {
        index_cache     := index_cache - U(8)
        wrcnt           := nextWrcnt
        wraddr          := addr.resize(GCElementWidth)
        writeBufferData := nextWriteValue

        dbg(Seq("enqueue res into write buffer"))
        io.Aop.Done := True
        goto(IDLE)
      }
    }

    WB_FLUSH_BUFFER_WRITE.whenIsActive {
      when(wrcnt =/= U(0)) {
        val addr       = wraddr.resize(MMUAddrWidth)
        val writeSize  = wrcnt * U(8)
        val writeValue = writeBufferData

        issueReq(io.Mreq, addr, True, writeSize, writeValue, False, False, issued) { _ => }

        when(issued) {
          issued := False

          wrcnt           := U(0)
          wraddr          := U(0)
          writeBufferData := U(0)

          dbg(Seq("NoAopSrc: flush pending buffer write"))
          goto(WB_WRITE_INDEX_BUFFER)
        }
      } otherwise {
        goto(WB_WRITE_INDEX_BUFFER)
      }
    }

    WB_WRITE_INDEX_BUFFER.whenIsActive {
      val addr       = pssAddr(U"x48")
      val writeValue = Cat(buffer_cache, temp_cache, index_cache).asUInt

      issueReq(io.Mreq, addr, True, U(24), writeValue, False, False, issued) { _ => }

      when(issued) {
        issued := False
        dbg(Seq("NoAopSrc: write index_cache"))
        goto(WB_WRITE_OFFSET30_38)
      }
    }

    WB_WRITE_OFFSET30_38.whenIsActive {
      when(parScanOff20_valid) {
        val addr       = pssAddr(U"x30")
        val writeValue = Cat(offset38_cache, offset30_cache).asUInt.resize(MMUDataWidth)

        issueReq(io.Mreq, addr, True, U(16), writeValue, False, False, issued) { _ => }

        when(issued) {
          issued := False

          dbg(Seq("NoAopSrc: write offset30_cache and offset38_cache"))
          goto(WB_WRITE_LAST_INDEX)
        }
      } otherwise {
        goto(WB_WRITE_LAST_INDEX)
      }
    }

    WB_WRITE_LAST_INDEX.whenIsActive {
      when(last_index_valid) {
        val addr       = pssAddr(U"x1b0")
        val writeValue = last_index_cache.resize(MMUDataWidth)

        issueReq(io.Mreq, addr, True, U(8), writeValue, False, False, issued) { _ => }

        when(issued) {
          issued := False

          dbg(Seq("NoAopSrc: write last_index_cache"))
          goto(WB_CLEAR_CACHE)
        }
      } otherwise {
        goto(WB_CLEAR_CACHE)
      }
    }

    WB_CLEAR_CACHE.whenIsActive {
      byte_about_valid   := False
      last_index_valid   := False
      parScanOff40_valid := False
      parScanOff20_valid := False

      wrcnt           := U(0)
      wraddr          := U(0)
      writeBufferData := U(0)

      clearNoAopSrcPending := True

      issued := False

      dbg(Seq("NoAopSrc: writeback done, clear cache valid"))
      goto(IDLE)
    }
  }

  // NoAopSrc latch update
  // clear 和 rise 同拍时，rise 优先，避免丢事件。
  when(clearNoAopSrcPending && !noAopSrcRise) {
    noAopSrcPending := False
  }
  when(noAopSrcRise) {
    noAopSrcPending := True
  }
}

object GCAopVerilog extends App {
  Config.spinal.generateVerilog(new GCAop())
}