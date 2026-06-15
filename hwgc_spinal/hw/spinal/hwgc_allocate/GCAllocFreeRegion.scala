package hwgc_acc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCAllocFreeRegion extends Module with GCParameters with HWParameters {
  val io = new Bundle {
    val Mreq              = master(new LocalMMUIO)
    val ConfigIO          = slave(new GCAllocFreeRegionConfigIO)
    val ToAllocFreeRegion = slave(new GCToAllocFreeRegion)
    val DebugTimeStamp    = in UInt (64 bits)
  }

  // Default outputs
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()

  io.Mreq.Response.ready := True

  io.ToAllocFreeRegion.clearOut()

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCAllocFreeRegion<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  // Constants
  private def ptrConst(v: Int): UInt = U(v, GCElementWidth bits)
  private def zeroPtr: UInt          = U(0, GCElementWidth bits)

  private val FREE_LIST_OFFSET = ptrConst(0x130 + 0xb0)

  private val OFF_LIST_BASE    = ptrConst(0x10)
  private val OFF_LIST_PATCH_0 = ptrConst(0x28)
  private val OFF_LIST_PATCH_1 = ptrConst(0x30)

  private val OFF_REGION_NEXT  = ptrConst(0xd0)
  private val OFF_REGION_PREV  = ptrConst(0xd8)

  private val OFF_HALF_LINE    = ptrConst(0x10)
  private val OFF_FULL_LINE    = ptrConst(0x20)

  private val SZ_8  = U(8)
  private val SZ_16 = U(16)
  private val SZ_24 = U(24)
  private val SZ_32 = U(32)

  // Registers
  val issued = RegInit(False)

  val fromHead       = RegInit(False)
  val freeListPtr    = RegInit(zeroPtr)

  val listHeadPtr    = RegInit(zeroPtr)
  val listTailPtr    = RegInit(zeroPtr)
  val listLastPtr    = RegInit(zeroPtr)
  val listLength     = RegInit(U(0, 32 bits))

  val newAllocRegion = RegInit(zeroPtr)
  val neighborPtr    = RegInit(zeroPtr)

  // Common helpers
  private def listBaseAddr: UInt = freeListPtr + OFF_LIST_BASE
  private def listBaseAligned: Bool = listBaseAddr(4 downto 0) === 0
  private def selectedRegion: UInt = Mux(fromHead, listHeadPtr, listTailPtr)
  private def selectedLinkOffset: UInt = Mux(fromHead, OFF_REGION_NEXT, OFF_REGION_PREV)
  private def neighborBackLinkOffset: UInt = Mux(fromHead, OFF_REGION_PREV, OFF_REGION_NEXT)

  // FSM
  val fsm = new StateMachine {
    val IDLE                  = new State with EntryPoint
    val READ_LIST_META_0      = new State
    val READ_LIST_META_1      = new State
    val READ_SELECTED_LINK    = new State
    val UPDATE_LIST_HEAD_TAIL = new State
    val CLEAR_NEIGHBOR_LINK   = new State
    val CLEAR_REMOVED_LINK    = new State
    val WRITE_LIST_LENGTH     = new State

    def finish(res: UInt): Unit = {
      io.ToAllocFreeRegion.Done := True
      io.ToAllocFreeRegion.newAllocRegion := res
      issued := False
      goto(IDLE)
    }

    def readReqGo(addr: UInt, size: UInt, next: State)(body: UInt => Unit): Unit = {
      issueReq(io.Mreq, addr, False, size, U(0), issued) { rd =>
        body(rd)
        goto(next)
      }
    }

    def writeReqIssuedGo(addr: UInt, size: UInt, data: UInt, next: State): Unit = {
      issueReq(io.Mreq, addr, True, size, data, issued) { _ => }

      when(issued) {
        issued := False
        goto(next)
      }
    }

    // IDLE
    IDLE.whenIsActive {
      io.ToAllocFreeRegion.Ready := True

      when(io.ToAllocFreeRegion.Valid && io.ToAllocFreeRegion.Ready) {
        freeListPtr := io.ConfigIO.G1h + FREE_LIST_OFFSET
        fromHead := (io.ToAllocFreeRegion.heapRegionType & U(2, 8 bits)) === U(0, 8 bits)

        newAllocRegion := zeroPtr
        neighborPtr := zeroPtr
        issued := False

        goto(READ_LIST_META_0)
      }
    }

    // Read free-list metadata part 0
    READ_LIST_META_0.whenIsActive {
      val readSize = Mux(listBaseAligned, SZ_32, SZ_16)

      readReqGo(listBaseAddr, readSize, READ_LIST_META_1) { rd =>
        listLength := rd(31 downto 0)

        when(listBaseAligned) {
          listHeadPtr := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
        }
      }
    }

    READ_LIST_META_1.whenIsActive {
      val addr     = listBaseAddr + Mux(listBaseAligned, OFF_FULL_LINE, OFF_HALF_LINE)
      val readSize = Mux(listBaseAligned, SZ_16, SZ_32)

      issueReq(io.Mreq, addr, False, readSize, U(0), issued) { rd =>
        when(listBaseAligned) {
          listTailPtr := rd(GCElementWidth - 1 downto 0)
          listLastPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        } otherwise {
          listHeadPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          listTailPtr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          listLastPtr := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
        }

        when(listLength === 0) {
          finish(zeroPtr)
        } otherwise {
          goto(READ_SELECTED_LINK)
        }
      }
    }

    READ_SELECTED_LINK.whenIsActive {
      val candidate = selectedRegion

      when(candidate === zeroPtr) {
        // 防御式处理：length 非 0 但 head/tail 为空，避免访问 0xd0 / 0xd8。
        finish(zeroPtr)
      } otherwise {
        val addr = candidate + selectedLinkOffset

        issueReq(io.Mreq, addr, False, SZ_8, U(0), issued) { rd =>
          newAllocRegion := candidate
          neighborPtr := rd(GCElementWidth - 1 downto 0)

          goto(UPDATE_LIST_HEAD_TAIL)
        }
      }
    }

    UPDATE_LIST_HEAD_TAIL.whenIsActive {
      val noNeighbor = neighborPtr === zeroPtr
      val isLast     = newAllocRegion === listLastPtr

      when(noNeighbor && isLast) {
        val addr = freeListPtr + OFF_LIST_PATCH_0
        val data = Cat(zeroPtr, zeroPtr, zeroPtr).asUInt

        // list becomes empty: clear head / tail / last
        writeReqIssuedGo(addr, SZ_24, data, CLEAR_REMOVED_LINK)

      } elsewhen noNeighbor {
        val addr = freeListPtr + OFF_LIST_PATCH_0
        val data = Cat(zeroPtr, zeroPtr).asUInt

        writeReqIssuedGo(addr, SZ_16, data, CLEAR_REMOVED_LINK)

      } elsewhen isLast {
        val addr = freeListPtr + Mux(fromHead, OFF_LIST_PATCH_0, OFF_LIST_PATCH_1)
        val size = Mux(fromHead, SZ_24, SZ_16)
        val data = Mux(fromHead, Cat(zeroPtr, listTailPtr, neighborPtr), Cat(zeroPtr, zeroPtr, neighborPtr)).asUInt

        writeReqIssuedGo(addr, size, data, CLEAR_NEIGHBOR_LINK)

      } otherwise {
        val addr = freeListPtr + Mux(fromHead, OFF_LIST_PATCH_0, OFF_LIST_PATCH_1)

        writeReqIssuedGo(addr, SZ_8, neighborPtr, CLEAR_NEIGHBOR_LINK)
      }
    }

    CLEAR_NEIGHBOR_LINK.whenIsActive {
      val addr = neighborPtr + neighborBackLinkOffset

      listLength := listLength - 1

      writeReqIssuedGo(addr, SZ_8, zeroPtr, CLEAR_REMOVED_LINK)
    }

    CLEAR_REMOVED_LINK.whenIsActive {
      val addr = newAllocRegion + selectedLinkOffset

      writeReqIssuedGo(addr, SZ_8, zeroPtr, WRITE_LIST_LENGTH)
    }

    WRITE_LIST_LENGTH.whenIsActive {
      val addr = freeListPtr + OFF_LIST_BASE

      issueReq(io.Mreq, addr, True, SZ_8, listLength, issued) { _ => }

      when(issued) {
        finish(newAllocRegion)
      }
    }
  }
}

object GCAllocFreeRegionVerilog extends App {
  Config.spinal.generateVerilog(new GCAllocFreeRegion())
}