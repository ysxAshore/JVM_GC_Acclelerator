package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCAllocate extends Module with GCTopParameters with HWParameters {
  val io = new Bundle {
    val Mreq          = master(new LocalMMUIO)
    val ToAllocate    = slave(new GCToAllocate)
    val ToParAllocate = master(new GCToParAllocate)
    val ConfigIO      = slave(new GCAllocateConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // Default IO
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ToAllocate.clearOut()
  io.ToParAllocate.clearIn()

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCAllocate<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  val issued = RegInit(False)

  val size         = RegInit(U(0, 32 bits))
  val destAttrType = RegInit(U(0, 8 bits))

  val destObjPtr     = RegInit(U(0, GCElementWidth bits))
  val actualPlabSize = RegInit(U(0, GCElementWidth bits))

  val plab_word_size  = RegInit(U(0, GCElementWidth bits))
  val required_in_plab = RegInit(U(0, GCElementWidth bits))

  val buffer0_ptr = RegInit(U(0, GCElementWidth bits))
  val buffer1_ptr = RegInit(U(0, GCElementWidth bits))

  val top_ptr          = RegInit(U(0, GCElementWidth bits))
  val hard_end_ptr     = RegInit(U(0, GCElementWidth bits))
  val off48_buffer_ptr = RegInit(U(0, GCElementWidth bits))

  val plab_stats_valid = Vec(RegInit(False), 2)
  val plab_stats_value = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  val allocator_ptr_valid = RegInit(False)
  val allocator_ptr_cache = RegInit(U(0, GCElementWidth bits))

  val buffer_valid = Vec(RegInit(False), 2)
  val buffer_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  val par_allocate_done     = RegInit(False)
  val par_allocate_return   = RegInit(False)
  val par_allocate_destObj  = RegInit(U(0, GCElementWidth bits))
  val par_allocate_plabSize = RegInit(U(0, GCElementWidth bits))

  val plab_refill_failed = RegInit(False)

  // Latched memory request, used to shorten timing path.
  val memReqAddr  = Reg(UInt(GCElementWidth bits))
  val memReqWrite = Reg(Bool())
  val memReqSize  = Reg(UInt(LineBytesNumBitSize bits))
  val memReqData  = Reg(UInt(MMUDataWidth bits))

  // Second 32B write after refill.
  val refillHiAddr = Reg(UInt(GCElementWidth bits))
  val refillHiData = Reg(UInt(MMUDataWidth bits))

  // Common combinational values
  val destAttrIdx = Mux(destAttrType === 0, U(0), U(1))
  val activeBufferPtr = buffer_cache(destAttrIdx)
  val words = (hard_end_ptr - top_ptr) >> 3
  val headSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(2), U(3))

  def setMemRead(addr: UInt, sizeBytes: UInt): Unit = {
    memReqAddr  := addr
    memReqWrite := False
    memReqSize  := sizeBytes.resize(LineBytesNumBitSize)
    memReqData  := U(0, MMUDataWidth bits)
  }

  def setMemWrite(addr: UInt, sizeBytes: UInt, data: UInt): Unit = {
    memReqAddr  := addr
    memReqWrite := True
    memReqSize  := sizeBytes.resize(LineBytesNumBitSize)
    memReqData  := data.resize(MMUDataWidth)
  }

  def times8(x: UInt): UInt = (x.resize(GCElementWidth) << 3).resize(GCElementWidth)

  def times10Wide(x: UInt): UInt = {
    val xw = x.resize(GCElementWidth + 4)
    ((xw << 3) + (xw << 1)).resize(GCElementWidth + 4)
  }

  def prepareRefillBufferWrite(obj: UInt, plabSize: UInt): Unit = {
    val delta = plabSize - U(2, GCElementWidth bits)

    val off20 = plabSize
    val off28 = obj
    val requestedWordSize = size.resize(GCElementWidth)
    val off30 = Mux(delta >= requestedWordSize, obj + times8(requestedWordSize), obj)
    val off38 = obj + times8(delta)
    val off40 = obj + times8(plabSize)
    val off48 = off48_buffer_ptr + plabSize

    val lowData = Cat(off38, off30, off28, off20).asUInt
    val highData = Cat(off48, off40).asUInt

    setMemWrite(activeBufferPtr + U"x20", U(32), lowData)

    refillHiAddr := activeBufferPtr + U"x40"
    refillHiData := highData.resize(MMUDataWidth)
  }

  def prepareCollapseBufferWrite(): Unit = {
    val data = Cat(hard_end_ptr, hard_end_ptr, hard_end_ptr).asUInt
    setMemWrite(activeBufferPtr + U"x28", U(24), data)
  }

  // Sticky capture for par_allocate result.
  when(io.ToParAllocate.done.valid) {
    par_allocate_done     := True
    par_allocate_destObj  := io.ToParAllocate.done.payload.DestObjPtr
    par_allocate_plabSize := io.ToParAllocate.done.payload.ActualPlabSize
  }

  val fsm = new StateMachine {

    def finish(destObj: UInt, failed: Bool): Unit = {
      io.ToAllocate.done.valid := True
      io.ToAllocate.done.payload.DestOopPtr := destObj
      io.ToAllocate.done.payload.PlabRefillFailed := failed

      issued := False
      goto(sIdle)

      dbg(Seq("The task has done, destObj = ", destObj, ", failed = ", failed))
    }

    def sendToParAllocate(nextState: State, minWordSize: UInt, desiredWordSize: UInt): Unit = {
      io.ToParAllocate.cmd.valid := True

      io.ToParAllocate.cmd.payload.MinWordSize := minWordSize
      io.ToParAllocate.cmd.payload.DesiredWordSize := desiredWordSize
      io.ToParAllocate.cmd.payload.DestAttrIdx := Mux(destAttrType === 0, U(0), U(1))
      io.ToParAllocate.cmd.payload.AllocatorPtr := allocator_ptr_cache

      when(io.ToParAllocate.cmd.fire) {
        goto(nextState)
      }
    }

    def issueLatchedRead(nextState: State)(onRead: Bits => Unit): Unit = {
      issueReq(io.Mreq, memReqAddr, False, memReqSize, U(0), True, False, issued) { rd =>
        onRead(rd.asBits)
        goto(nextState)
      }
    }

    def issueLatchedWriteWithoutResp(nextState: State)(onAccepted: => Unit = {}): Unit = {
      issueReq(io.Mreq, memReqAddr, True, memReqSize, memReqData, False, False, issued) { _ => }

      when(issued) {
        issued := False
        onAccepted
        goto(nextState)
      }
    }

    // recursive value sIdle needs type
    //        issueLatchedWrite(sIdle) {
    // Scala 的前向引用类型推断问题: Scala 在匿名 StateMachine block 里遇到这种互相引用时，推不出类型，就报这个报错
    // 把所有状态都显式写成 : State 即可
    val sIdle: State = new State with EntryPoint {
      whenIsActive {
        issued := False

        io.ToAllocate.cmd.ready := True

        when(io.ToAllocate.cmd.fire) {
          size := io.ToAllocate.cmd.payload.Size
          destAttrType := io.ToAllocate.cmd.payload.DestAttrType

          plab_refill_failed := False
          par_allocate_return := False
          par_allocate_done := False

          dbg(Seq("Receive allocation task, size = ", io.ToAllocate.cmd.payload.Size, ", destAttrType = ", io.ToAllocate.cmd.payload.DestAttrType))

          goto(sCheckPlabStats)
        }
      }
    }

    val sCheckPlabStats: State = new State {
      whenIsActive {
        when(!plab_stats_valid(destAttrIdx)) {
          val addr = io.ConfigIO.G1h + Mux(destAttrType === 0, U"x250", U"x2e0") + U"x30"
          setMemRead(addr, U(8))
          goto(sReadPlabStats)
        } otherwise {
          goto(sPrepareSize)
        }
      }
    }

    val sReadPlabStats: State = new State {
      whenIsActive {
        issueLatchedRead(sPrepareSize) { rd =>
          plab_stats_valid(destAttrIdx) := True
          plab_stats_value(destAttrIdx) := rd(GCElementWidth - 1 downto 0).asUInt
        }
      }
    }

    val sPrepareSize: State = new State {
      whenIsActive {
        plab_word_size := Min(Max(plab_stats_value(destAttrIdx), U"x102"), U"x40000")

        required_in_plab := (size + U(2)).resize(GCElementWidth)

        when(!allocator_ptr_valid) {
          setMemRead(io.ConfigIO.PlabAllocatorPtr + U(8), U(24))
          goto(sReadAllocator)
        } otherwise {
          goto(sDecideAllocateMode)
        }
      }
    }

    val sReadAllocator: State = new State {
      whenIsActive {
        issueLatchedRead(sDecideAllocateMode) { rd =>
          allocator_ptr_valid := True
          allocator_ptr_cache := rd(GCElementWidth - 1 downto 0).asUInt
          buffer0_ptr := rd(GCElementWidth * 2 - 1 downto GCElementWidth).asUInt
          buffer1_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2).asUInt
        }
      }
    }

    val sDecideAllocateMode: State = new State {
      whenIsActive {
        val req10 = times10Wide(required_in_plab)
        val plabWide = plab_word_size.resize(GCElementWidth + 4)
        val useNewPlab = (required_in_plab <= plab_word_size) && (req10 < plabWide)

        when(useNewPlab) {
          par_allocate_return := False
          sendToParAllocate(sCheckBufferPtr, required_in_plab, plab_word_size)
        } otherwise {
          par_allocate_return := True
          sendToParAllocate(sWaitParAllocate, size.resize(GCElementWidth), size.resize(GCElementWidth))
        }
      }
    }

    val sCheckBufferPtr: State = new State {
      whenIsActive {
        when(!buffer_valid(destAttrIdx)) {
          val addr = Mux(destAttrType === 0, buffer0_ptr, buffer1_ptr)
          setMemRead(addr, U(8))
          goto(sReadBufferPtr)
        } otherwise {
          goto(sPrepareReadBufferMeta)
        }
      }
    }

    val sReadBufferPtr: State = new State {
      whenIsActive {
        issueLatchedRead(sPrepareReadBufferMeta) { rd =>
          buffer_valid(destAttrIdx) := True
          buffer_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0).asUInt
        }
      }
    }

    val sPrepareReadBufferMeta: State = new State {
      whenIsActive {
        setMemRead(buffer_cache(destAttrIdx) + U"x30", U(32))
        goto(sReadBufferMeta)
      }
    }

    val sReadBufferMeta: State = new State {
      whenIsActive {
        issueLatchedRead(sPrepareOldPlabHeader) { rd =>
          top_ptr := rd(GCElementWidth - 1 downto 0).asUInt
          hard_end_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2).asUInt
          off48_buffer_ptr := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3).asUInt
        }
      }
    }

    val sPrepareOldPlabHeader: State = new State {
      whenIsActive {
        when(top_ptr < hard_end_ptr) {
          val enoughWords = words >= headSize
          val klassPtr = Mux(enoughWords, io.ConfigIO.IntArrayKlassObj, io.ConfigIO.ObjectKlassObj)
          val writeOff0 = U(1, 64 bits)
          val writeOff8 = Mux(io.ConfigIO.UseCompressedKlassPointers,
            ((klassPtr - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64),
            klassPtr.resize(64)
          )
          val writeOff12_16 = ((words - headSize).resize(GCElementWidth) * U(2, GCElementWidth bits)).resize(32)
          val writeValue = Mux(io.ConfigIO.UseCompressedKlassPointers,
            Cat(writeOff12_16, writeOff8.resize(32), writeOff0).asUInt.resize(MMUDataWidth),
            Cat(writeOff12_16, writeOff8, writeOff0).asUInt.resize(MMUDataWidth)
          )
          val writeSize = Mux(io.ConfigIO.UseCompressedKlassPointers && enoughWords,
            U(16),
            Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), Mux(enoughWords, U(20), U(16)))
          )

          setMemWrite(top_ptr, writeSize, writeValue)
          goto(sWriteOldPlabHeader)
        } otherwise {
          goto(sWaitParAllocate)
        }
      }
    }

    val sWriteOldPlabHeader: State = new State {
      whenIsActive {
        issueLatchedWriteWithoutResp(sWaitParAllocate)()
      }
    }

    val sWaitParAllocate: State = new State {
      whenIsActive {
        val hasParResult = io.ToParAllocate.done.valid || par_allocate_done
        val returnObjPtr = Mux(par_allocate_done, par_allocate_destObj, io.ToParAllocate.done.payload.DestObjPtr)
        val returnPlabSize = Mux(par_allocate_done, par_allocate_plabSize, io.ToParAllocate.done.payload.ActualPlabSize)

        when(hasParResult) {
          par_allocate_done := False

          destObjPtr := returnObjPtr
          actualPlabSize := returnPlabSize

          when(par_allocate_return) {
            par_allocate_return := False
            finish(returnObjPtr, plab_refill_failed)
          } otherwise {
            when(returnObjPtr =/= 0) {
              prepareRefillBufferWrite(returnObjPtr, returnPlabSize)
              goto(sWriteRefillLow32B)
            } elsewhen(top_ptr < hard_end_ptr) {
              prepareCollapseBufferWrite()
              goto(sWriteCollapseBuffer)
            } otherwise {
              par_allocate_return := True
              plab_refill_failed := True
              goto(sSendDirectRetry)
            }
          }
        }
      }
    }

    val sWriteRefillLow32B: State = new State {
      whenIsActive {
        issueLatchedWriteWithoutResp(sWriteRefillHigh32B) {
          setMemWrite(refillHiAddr, U(16), refillHiData)
        }
      }
    }

    val sWriteRefillHigh32B: State = new State {
      whenIsActive {
        issueLatchedWriteWithoutResp(sIdle) {
          io.ToAllocate.done.valid := True
          io.ToAllocate.done.payload.DestOopPtr := destObjPtr
          io.ToAllocate.done.payload.PlabRefillFailed := False

          dbg(Seq("Refill buffer done, destObj = ", destObjPtr))
        }
      }
    }

    val sWriteCollapseBuffer: State = new State {
      whenIsActive {
        issueLatchedWriteWithoutResp(sSendDirectRetry) {
          par_allocate_return := True
          plab_refill_failed := True
        }
      }
    }

    val sSendDirectRetry: State = new State {
      whenIsActive {
        sendToParAllocate(sWaitParAllocate, size.resize(GCElementWidth), size.resize(GCElementWidth))
      }
    }
  }
}

object GCAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCAllocate())
}