package hwgc_acc

import hwgc_top.{Config, GCInterruptsIO, GCTopParameters, HWParameters, LocalMMUIO, MyStateMachine}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCNewGCAlloc extends Module with GCTopParameters with HWParameters {
  val io = new Bundle {
    val Mreq              = master(new LocalMMUIO)
    val ToNewGCAlloc      = slave(new GCToNewGCAlloc)
    val ToAllocFreeRegion = master(new GCToNewGCAlloc)
    val Irq               = master(new GCInterruptsIO)
    val ConfigIO          = slave(new GCNewGCAllocConfigIO)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True

  io.Irq.clearOut()
  io.ToNewGCAlloc.clearIn()
  io.ToAllocFreeRegion.clearOut()

  private def ptrConst(v: Int): UInt = U(v, GCElementWidth bits)
  private def zeroPtr: UInt          = U(0, GCElementWidth bits)

  private val SZ_1  = U(1)
  private val SZ_4  = U(4)
  private val SZ_8  = U(8)
  private val SZ_12 = U(12)
  private val SZ_16 = U(16)

  private val HEAP_REGION_YOUNG = U(3, 32 bits)
  private val HEAP_REGION_OLD   = U(0x10, 32 bits)

  private val OFF_NODE_INDEX       = ptrConst(0x30)
  private val OFF_GROW_ARRAY_PTR   = ptrConst(0x400)
  private val OFF_EXPAND_FAILURE   = ptrConst(0x370)
  private val OFF_REGION_ATTR_BASE = ptrConst(0x590)

  private val OFF_REGION_TYPE      = ptrConst(0xbc)
  private val OFF_REGION_REMSET    = ptrConst(0xb0)
  private val OFF_REMSET_STATE     = ptrConst(0xf0)

  val regionPtr      = RegInit(zeroPtr)
  val newAllocRegion = RegInit(zeroPtr)
  val heapRegionType = RegInit(U(0, 32 bits))

  val nodeIndex = RegInit(U(0, 32 bits))
  val arrayMax  = RegInit(U(0, 32 bits))
  val arrayLen  = RegInit(U(0, 32 bits))
  val dataPtr   = RegInit(zeroPtr)

  val remsetPtr = RegInit(zeroPtr)
  val hrmIndex  = RegInit(U(0, 32 bits))

  val growArrayPtrValid = RegInit(False)
  val growArrayPtr      = RegInit(zeroPtr)

  val regionAttrBaseValid = RegInit(False)
  val regionAttrBase      = RegInit(zeroPtr)

  val expandFailureValid = RegInit(False)
  val expandFailureCache = RegInit(False)

  val remsetStateWritten = RegInit(False)

  val allocFreeRegionDone = RegInit(False)
  val allocFreeRegion     = RegInit(zeroPtr)

  val againCallAllocRegion = RegInit(False)

  when(io.ToAllocFreeRegion.done.valid) {
    allocFreeRegionDone := True
    allocFreeRegion     := io.ToAllocFreeRegion.done.payload.NewAllocRegion
  }

  private def isYoungRegion: Bool = heapRegionType === HEAP_REGION_YOUNG
  private def growArrayEntryAddr: UInt = (dataPtr + (arrayLen.resize(GCElementWidth) << 3)).resize(MMUAddrWidth)
  private def regionAttrAddr: UInt = (regionAttrBase + (hrmIndex.resize(GCElementWidth) << 1)).resize(MMUAddrWidth)
  private def needsRemsetUpdate: Bool = (heapRegionType & HEAP_REGION_OLD) === U(0, 32 bits)
  private def remsetStateValue: UInt = {
    val value = Mux(
      (heapRegionType & U(2, 32 bits)) =/= U(0, 32 bits),
      U(2, 32 bits),
      Mux((heapRegionType & U(0x10, 32 bits)) =/= U(0, 32 bits), U(0, 32 bits), U(1, 32 bits))
    )

    value.resize(GCElementWidth)
  }

  val fsm = new MyStateMachine {
    val IDLE                          = new State with EntryPoint
    val READ_NODE_INDEX               = new State
    val CALL_ALLOC_FREE               = new State
    val WAIT_ALLOC_AND_CONFIG         = new State
    val CALL_EXPAND_SINGLE_REGION_IRQ = new State
    val WAIT_ALLOC_IRQ                = new State
    val WRITE_REGION_TYPE             = new State
    val READ_GROW_ARRAY               = new State
    val SEND_GROW_IRQ                 = new State
    val WAIT_GROW_IRQ                 = new State
    val READ_DATA_PTR                 = new State
    val WRITE_GROW_ARRAY_LEN          = new State
    val WRITE_GROW_ARRAY_ITEM         = new State
    val READ_REGION_INFO              = new State
    val FINAL_UPDATE                  = new State

    def finish(res: UInt): Unit = {
      io.ToNewGCAlloc.done.valid := True
      io.ToNewGCAlloc.done.payload.NewAllocRegion := res
      remsetStateWritten := False
      goto(IDLE)
    }

    IDLE.whenIsActive {
      io.ToNewGCAlloc.cmd.ready := True

      when(io.ToNewGCAlloc.cmd.fire) {
        regionPtr := io.ToNewGCAlloc.cmd.payload.RegionPtr
        heapRegionType := Mux(io.ToNewGCAlloc.cmd.payload.RegionType === U(1), HEAP_REGION_OLD, HEAP_REGION_YOUNG)

        newAllocRegion := zeroPtr
        issued         := False
        allocFreeRegionDone := False
        allocFreeRegion     := zeroPtr
        remsetStateWritten := False
        againCallAllocRegion := False

        goto(READ_NODE_INDEX)
      }
    }

    READ_NODE_INDEX.whenIsActive {
      val addr = regionPtr + OFF_NODE_INDEX
      issueDirectRead(io.Mreq, addr, SZ_4, CALL_ALLOC_FREE) { rd =>
        nodeIndex := rd(31 downto 0)
      }
    }

    CALL_ALLOC_FREE.whenIsActive {
      io.ToAllocFreeRegion.cmd.valid := True
      io.ToAllocFreeRegion.cmd.payload.RegionType := heapRegionType.resized

      when(io.ToAllocFreeRegion.cmd.fire) {
        goto(WAIT_ALLOC_AND_CONFIG)
      }
    }

    WAIT_ALLOC_AND_CONFIG.whenIsActive {
      val allocDoneNow = allocFreeRegionDone || io.ToAllocFreeRegion.done.valid
      val allocResult  = Mux(allocFreeRegionDone, allocFreeRegion, io.ToAllocFreeRegion.done.payload.NewAllocRegion)

      when(isYoungRegion && !growArrayPtrValid) {
        val addr = io.ConfigIO.G1h + OFF_GROW_ARRAY_PTR
        issueDirectRead(io.Mreq, addr, SZ_8, WAIT_ALLOC_AND_CONFIG) { rd =>
          growArrayPtr      := rd(GCElementWidth - 1 downto 0)
          growArrayPtrValid := True
        }

      } elsewhen(!regionAttrBaseValid) {
        val addr = io.ConfigIO.G1h + OFF_REGION_ATTR_BASE
        issueDirectRead(io.Mreq, addr, SZ_8, WAIT_ALLOC_AND_CONFIG) { rd =>
          regionAttrBase      := rd(GCElementWidth - 1 downto 0)
          regionAttrBaseValid := True
        }

      } elsewhen allocDoneNow {
        allocFreeRegionDone := False
        newAllocRegion      := allocResult

        when(againCallAllocRegion || allocResult =/= zeroPtr){
          againCallAllocRegion := False
          goto(WRITE_REGION_TYPE)
        }.elsewhen(allocResult === zeroPtr) {
          when(!expandFailureValid) {
            val addr = io.ConfigIO.G1h + OFF_EXPAND_FAILURE
            issueDirectRead(io.Mreq, addr, SZ_1, CALL_EXPAND_SINGLE_REGION_IRQ) { rd =>
              expandFailureValid := True
              expandFailureCache := rd(0)
            }
          } otherwise {
            goto(CALL_EXPAND_SINGLE_REGION_IRQ)
          }
        }
      }
    }

    CALL_EXPAND_SINGLE_REGION_IRQ.whenIsActive {
      when(expandFailureCache){
        // @notice: interrupt to call _g1h_expand_single_region to return whether call alloc_free_region again
        io.Irq.req.valid := True
        io.Irq.req.payload.par0 := nodeIndex.resized
        io.Irq.req.payload.cmd := IRQ_EXPAND
        when(io.Irq.req.fire){
          goto(WAIT_ALLOC_IRQ)
        }
      }.otherwise{
        goto(WRITE_REGION_TYPE)
      }
    }

    WAIT_ALLOC_IRQ.whenIsActive {
      // @notice: interrupt res
      when(io.Irq.resp.valid){
        when(io.Irq.resp.payload.res0(7 downto 0) =/= U(0)){
          againCallAllocRegion := True
          goto(CALL_ALLOC_FREE)
        }.otherwise{
          expandFailureCache := False
          issueDirectWriteWithoutResp(io.Mreq, io.ConfigIO.G1h + U"x370", U(1), U(0), WRITE_REGION_TYPE)()
        }
      }
    }

    WRITE_REGION_TYPE.whenIsActive {
      when(newAllocRegion =/= U(0)) {
        val addr = newAllocRegion + OFF_REGION_TYPE
        issueReq(io.Mreq, addr, True, SZ_4, heapRegionType.resize(GCElementWidth), False, False, issued) { _ => }

        when(issued) {
          issued := False

          when(isYoungRegion) {
            goto(READ_GROW_ARRAY)
          } otherwise {
            goto(READ_REGION_INFO)
          }
        }
      }.otherwise{
        finish(newAllocRegion)
      }
    }

    READ_GROW_ARRAY.whenIsActive {
      val canReadDataPtrTogether = (growArrayPtr + U(8))(4 downto 0) =/= U(0)
      val readSize = Mux(canReadDataPtrTogether, SZ_16, SZ_8)
      issueReq(io.Mreq, growArrayPtr, False, readSize, U(0), True, False, issued) { rd =>
        arrayLen := rd(31 downto 0)
        arrayMax := rd(63 downto 32)
        dataPtr  := rd(127 downto 64)

        when(rd(31 downto 0) === rd(63 downto 32)) {
          goto(SEND_GROW_IRQ)
        }.elsewhen (canReadDataPtrTogether) {
          goto(READ_DATA_PTR)
        }.otherwise {
          goto(WRITE_GROW_ARRAY_LEN)
        }
      }
    }

    SEND_GROW_IRQ.whenIsActive {
      // @notice: interrupt to call grow(len) to grow len
      io.Irq.req.valid := True
      io.Irq.req.payload.par0 := growArrayPtr
      io.Irq.req.payload.par1 := arrayLen.resized
      io.Irq.req.payload.cmd := IRQ_GROW
      when(io.Irq.req.fire){
        goto(WAIT_GROW_IRQ)
      }
    }

    WAIT_GROW_IRQ.whenIsActive {
      // @notice: wait interrupt resp
      when(io.Irq.resp.valid){
        goto(READ_DATA_PTR)
      }
    }

    READ_DATA_PTR.whenIsActive {
      issueDirectRead(io.Mreq, growArrayPtr + U(8), SZ_8, WRITE_GROW_ARRAY_LEN) { rd =>
        dataPtr := rd(GCElementWidth - 1 downto 0)
      }
    }

    WRITE_GROW_ARRAY_LEN.whenIsActive {
      val writeValue = (arrayLen + U(1)).resize(GCElementWidth)
      issueDirectWriteWithoutResp(io.Mreq, growArrayPtr, SZ_4, writeValue, WRITE_GROW_ARRAY_ITEM)()
    }

    WRITE_GROW_ARRAY_ITEM.whenIsActive {
      issueDirectWriteWithoutResp(io.Mreq, growArrayEntryAddr, SZ_8, newAllocRegion, READ_REGION_INFO)()
    }

    READ_REGION_INFO.whenIsActive {
      val addr = newAllocRegion + OFF_REGION_REMSET

      issueDirectRead(io.Mreq, addr, SZ_12, FINAL_UPDATE) { rd =>
        remsetPtr := rd(GCElementWidth - 1 downto 0)
        hrmIndex  := rd(GCElementWidth + 31 downto GCElementWidth)
        remsetStateWritten := False
      }
    }

    FINAL_UPDATE.whenIsActive {
      val shouldWriteRemsetState = remsetStateValue =/= U(1, GCElementWidth bits)

      when(shouldWriteRemsetState && !remsetStateWritten) {
        val addr = remsetPtr + OFF_REMSET_STATE
        issueDirectWriteWithoutResp(io.Mreq, addr, SZ_4, remsetStateValue, FINAL_UPDATE) {
          remsetStateWritten := True
        }

      } otherwise {
        val writeValue = needsRemsetUpdate.asUInt.resize(GCElementWidth)
        issueReq(io.Mreq, regionAttrAddr, True, SZ_1, writeValue, False, False, issued) { _ => }

        when(issued) {
          finish(newAllocRegion)
        }
      }
    }
  }
}

object GCNewGCAllocVerilog extends App {
  Config.spinal.generateVerilog(new GCNewGCAlloc())
}