package hwgc_allocate

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCNewGCAlloc extends Module with GCTopParameters with HWParameters {
  val io = new Bundle {
    val Mreq              = master(new LocalMMUIO)
    val ToNewGCAlloc      = slave(new GCToNewGCAlloc)
    val ToAllocFreeRegion = master(new GCToAllocFreeRegion)
    val ConfigIO          = slave(new GCNewGCAllocConfigIO)
  }

  // Default outputs
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()

  io.Mreq.Response.ready := True

  io.ToNewGCAlloc.clearOut()
  io.ToAllocFreeRegion.clearIn()

  // Constants
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

  // Registers
  val issued = RegInit(False)

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

  when(io.ToAllocFreeRegion.Done) {
    allocFreeRegionDone := True
    allocFreeRegion     := io.ToAllocFreeRegion.newAllocRegion
  }

  // Helper expressions
  private def isYoungRegion: Bool = heapRegionType === HEAP_REGION_YOUNG

  private def growArrayEntryAddr: UInt = (dataPtr + (arrayLen.resize(GCElementWidth) << 3)).resize(MMUAddrWidth)

  private def regionAttrAddr: UInt = (regionAttrBase + (hrmIndex.resize(GCElementWidth) << 1)).resize(MMUAddrWidth)

  private def needsRemsetUpdate: Bool = (heapRegionType & HEAP_REGION_OLD) === U(0, 32 bits)

  private def remsetStateValue: UInt = {
    val value = Mux((heapRegionType & U(2, 32 bits)) =/= U(0, 32 bits),
      U(2, 32 bits),
      Mux((heapRegionType & U(0x10, 32 bits)) =/= U(0, 32 bits), U(0, 32 bits), U(1, 32 bits))
    )

    value.resize(GCElementWidth)
  }

  // FSM
  val fsm = new StateMachine {
    val IDLE                          = new State with EntryPoint
    val READ_NODE_INDEX               = new State
    val CALL_ALLOC_FREE               = new State
    val WAIT_ALLOC_AND_CONFIG         = new State
    val WRITE_REGION_TYPE             = new State
    val READ_GROW_ARRAY               = new State
    val READ_DATA_PTR                 = new State
    val WRITE_GROW_ARRAY_LEN          = new State
    val WRITE_GROW_ARRAY_ITEM         = new State
    val READ_REGION_INFO              = new State
    val FINAL_UPDATE                  = new State
    val CALL_EXPAND_SINGLE_REGION_IRQ = new State
    val WAIT_IRQ                      = new State

    def finish(res: UInt): Unit = {
      io.ToNewGCAlloc.Done := True
      io.ToNewGCAlloc.newAllocRegion := res
      issued := False
      remsetStateWritten := False
      goto(IDLE)
    }

    def writeReqIssuedGo(addr: UInt, size: UInt, data: UInt, next: State): Unit = {
      issueReq(io.Mreq, addr, True, size, data, False, False, issued) { _ => }

      when(issued) {
        issued := False
        goto(next)
      }
    }

    // IDLE
    IDLE.whenIsActive {
      io.ToNewGCAlloc.Ready := True

      when(io.ToNewGCAlloc.Valid && io.ToNewGCAlloc.Ready) {
        regionPtr := io.ToNewGCAlloc.regionPtr
        heapRegionType := Mux(io.ToNewGCAlloc.regionType === U(1), HEAP_REGION_OLD, HEAP_REGION_YOUNG)

        newAllocRegion := zeroPtr
        issued         := False
        allocFreeRegionDone := False
        allocFreeRegion     := zeroPtr
        remsetStateWritten := False
        againCallAllocRegion := False

        goto(READ_NODE_INDEX)
      }
    }

    // Read region node index
    READ_NODE_INDEX.whenIsActive {
      val addr = regionPtr + OFF_NODE_INDEX

      issueReq(io.Mreq, addr, False, SZ_4, U(0), True, False, issued) { rd =>
        nodeIndex := rd(31 downto 0)
        goto(CALL_ALLOC_FREE)
      }
    }

    // Start AllocFreeRegion
    CALL_ALLOC_FREE.whenIsActive {
      io.ToAllocFreeRegion.Valid := True
      io.ToAllocFreeRegion.heapRegionType := heapRegionType

      when(io.ToAllocFreeRegion.Valid && io.ToAllocFreeRegion.Ready) {
        goto(WAIT_ALLOC_AND_CONFIG)
      }
    }

    // -----------------------------------------------------------------------
    // Wait AllocFreeRegion and prefetch needed config
    //
    // 这个状态合并了原来的 s3 / s4 / s5 / s6 / s7：
    // - young region 才需要 grow_array_ptr
    // - region_attr_base 后续成功分配时一定需要，提前 cache
    // - AllocFreeRegion 的 Done 用 allocFreeRegionDone 防漏
    // - expand_failure 原逻辑是 TODO，这里只读 cache，不让状态机死锁
    // -----------------------------------------------------------------------

    WAIT_ALLOC_AND_CONFIG.whenIsActive {
      val allocDoneNow = allocFreeRegionDone || io.ToAllocFreeRegion.Done
      val allocResult  = Mux(allocFreeRegionDone, allocFreeRegion, io.ToAllocFreeRegion.newAllocRegion)

      when(isYoungRegion && !growArrayPtrValid) {
        val addr = io.ConfigIO.G1h + OFF_GROW_ARRAY_PTR
        issueReq(io.Mreq, addr, False, SZ_8, U(0), True, False, issued) { rd =>
          growArrayPtr      := rd(GCElementWidth - 1 downto 0)
          growArrayPtrValid := True
          goto(WAIT_ALLOC_AND_CONFIG)
        }

      } elsewhen(!regionAttrBaseValid) {
        val addr = io.ConfigIO.G1h + OFF_REGION_ATTR_BASE
        issueReq(io.Mreq, addr, False, SZ_8, U(0), True, False, issued) { rd =>
          regionAttrBase      := rd(GCElementWidth - 1 downto 0)
          regionAttrBaseValid := True
          goto(WAIT_ALLOC_AND_CONFIG)
        }

      } elsewhen allocDoneNow {
        allocFreeRegionDone := False
        newAllocRegion      := allocResult

        when(againCallAllocRegion || allocResult =/= zeroPtr){
          goto(WRITE_REGION_TYPE)
        }.elsewhen(allocResult === zeroPtr) {
          when(!expandFailureValid) {
            val addr = io.ConfigIO.G1h + OFF_EXPAND_FAILURE
            issueReq(io.Mreq, addr, False, SZ_1, U(0), True, False, issued) { rd =>
              expandFailureValid := True
              expandFailureCache := rd(0)

              goto(CALL_EXPAND_SINGLE_REGION_IRQ)
            }
          } otherwise {
            goto(CALL_EXPAND_SINGLE_REGION_IRQ)
          }
        }
      }
    }

    CALL_EXPAND_SINGLE_REGION_IRQ.whenIsActive {
      when(expandFailureCache){
        // @todo send to irq
      }.otherwise{
        goto(WRITE_REGION_TYPE)
      }
    }

    // @todo wait irq res
    WAIT_IRQ.whenIsActive {
     // when(irq.res){
     //   goto(CALL_ALLOC_FREE)
     // }.otherwise{
     //   expandFailureCache := False
     //   writeReqIssuedGo(io.ConfigIO.G1h + U"x370", U(1), U(0), WRITE_REGION_TYPE)
     // }
    }

    WRITE_REGION_TYPE.whenIsActive {
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
    }

    READ_GROW_ARRAY.whenIsActive {
      issueReq(io.Mreq, growArrayPtr, False, SZ_8, U(0), True, False, issued) { rd =>
        arrayLen := rd(31 downto 0)
        arrayMax := rd(63 downto 32)

        when(rd(31 downto 0) === rd(63 downto 32)) {
          /* @todo send irq
           * 原代码这里准备 callGrowIRQ，但没有真正的 IRQ 握手。
           * 当前版本不死等，跳过 grow array 写入，继续后续 region 初始化。
           */
          goto(READ_REGION_INFO)
        } otherwise {
          goto(READ_DATA_PTR)
        }
      }
    }

    READ_DATA_PTR.whenIsActive {
      issueReq(io.Mreq, growArrayPtr + U(8), False, SZ_8, U(0), True, False, issued) { rd =>
        dataPtr := rd(GCElementWidth - 1 downto 0)
        goto(WRITE_GROW_ARRAY_LEN)
      }
    }

    WRITE_GROW_ARRAY_LEN.whenIsActive {
      val writeValue = (arrayLen + U(1)).resize(GCElementWidth)
      writeReqIssuedGo(addr = growArrayPtr, size = SZ_4, data = writeValue, next = WRITE_GROW_ARRAY_ITEM)
    }

    // Young region: grow_array.data[array_len] = newAllocRegion
    WRITE_GROW_ARRAY_ITEM.whenIsActive {
      writeReqIssuedGo(addr = growArrayEntryAddr, size = SZ_8, data = newAllocRegion, next = READ_REGION_INFO)
    }

    // Read remset pointer and HRM index from new region
    READ_REGION_INFO.whenIsActive {
      val addr = newAllocRegion + OFF_REGION_REMSET

      issueReq(io.Mreq, addr, False, SZ_12, U(0), True, False, issued) { rd =>
        remsetPtr := rd(GCElementWidth - 1 downto 0)
        hrmIndex  := rd(GCElementWidth + 31 downto GCElementWidth)
        remsetStateWritten := False

        goto(FINAL_UPDATE)
      }
    }

    FINAL_UPDATE.whenIsActive {
      val shouldWriteRemsetState = remsetStateValue =/= U(1, GCElementWidth bits)

      when(shouldWriteRemsetState && !remsetStateWritten) {
        val addr = remsetPtr + OFF_REMSET_STATE
        issueReq(io.Mreq, addr, True, SZ_4, remsetStateValue, False, False, issued) { _ => }

        when(issued) {
          issued := False
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