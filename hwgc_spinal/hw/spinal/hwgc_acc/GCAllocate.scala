package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, MyStateMachine}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCAllocate extends Module with GCTopParameters with HWParameters {
  val io = new Bundle {
    val Mreq           = master(new LocalMMUIO)
    val ToAllocate     = slave(new GCToAllocate)
    val ToParAllocate  = master(new GCToParAllocate)
    val ConfigIO       = slave(new GCAllocateConfigIO)
    val DebugTimeStamp = in UInt (64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ToAllocate.clearIn()
  io.ToParAllocate.clearOut()

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) {
      report(Seq("[GCAllocate<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))
    }

  val size         = RegInit(U(0, 32 bits))
  val destAttrType = RegInit(U(0, 8 bits))

  val destObjPtr     = RegInit(U(0, GCElementWidth bits))
  val actualPlabSize = RegInit(U(0, GCElementWidth bits))

  val plab_word_size   = RegInit(U(0, GCElementWidth bits))
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

  val par_allocate_done = RegInit(False)
  val par_allocate_return = RegInit(False)
  val par_allocate_destObj = RegInit(U(0, GCElementWidth bits))
  val par_allocate_plabSize = RegInit(U(0, GCElementWidth bits))

  val plab_refill_failed = RegInit(False)

  val destAttrIdx = Mux(destAttrType === 0, U(0), U(1))
  val activeBufferPtr = buffer_cache(destAttrIdx)
  val words = (hard_end_ptr - top_ptr) >> 3
  val headSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(2), U(3))

  val writeSizeReg = Reg(UInt(32 bits))
  val writeValueReg = Reg(UInt(MMUDataWidth bits))

  def times2(x: UInt): UInt = (x.resize(GCElementWidth) << 1).resize(GCElementWidth)
  def times8(x: UInt): UInt = (x.resize(GCElementWidth) << 3).resize(GCElementWidth)
  def times10Wide(x: UInt): UInt = {
    val xw = x.resize(GCElementWidth + 4)
    ((xw << 3) + (xw << 1)).resize(GCElementWidth + 4)
  }

  // Sticky capture for par_allocate result
  when(io.ToParAllocate.done.valid) {
    par_allocate_done := True
    par_allocate_destObj := io.ToParAllocate.done.payload.DestObjPtr
    par_allocate_plabSize := io.ToParAllocate.done.payload.ActualPlabSize
  }

  val fsm = new MyStateMachine{
    val sIdle = new State with EntryPoint
    val sCheckPlabStats = new State
    val sPrepareSize = new State
    val sDecideAllocateMode = new State
    val sCheckBufferPtr = new State
    val sReadBufferMeta = new State
    val sPrepareWriteOldPlabHeader = new State
    val sWriteOldPlabHeader = new State
    val sWaitParAllocate = new State
    val sWriteRefillLow32B = new State
    val sWriteRefillHigh16B = new State
    val sWriteCollapseBuffer = new State
    val sSendDirectRetry = new State

    // FSM helpers
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

    sIdle.whenIsActive {
      issued := False

      io.ToAllocate.cmd.ready := True

      when(io.ToAllocate.cmd.fire) {
        size := io.ToAllocate.cmd.payload.Size
        destAttrType := io.ToAllocate.cmd.payload.DestAttrType

        plab_refill_failed := False
        par_allocate_return := False
        par_allocate_done := False

        goto(sCheckPlabStats)

        dbg(Seq("Receive allocation task, size = ", io.ToAllocate.cmd.payload.Size, ", destAttrType = ", io.ToAllocate.cmd.payload.DestAttrType))
      }
    }

    sCheckPlabStats.whenIsActive {
      when(!plab_stats_valid(destAttrIdx)) {
        val addr = io.ConfigIO.G1h + Mux(destAttrType === 0, U"x250", U"x2e0") + U"x30"
        issueDirectRead(io.Mreq, addr, U(8), sPrepareSize) { rd =>
          plab_stats_valid(destAttrIdx) := True
          plab_stats_value(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
        }
      } otherwise {
        goto(sPrepareSize)
      }
    }

    sPrepareSize.whenIsActive {
      // @notice: plab_stats_value / no_of_gc_workers
      plab_word_size := Min(Max(plab_stats_value(destAttrIdx), U"x102"), U"x40000")
      required_in_plab := (size.resize(GCElementWidth) + U(2)).resize(GCElementWidth)

      when(!allocator_ptr_valid) {
        val addr = io.ConfigIO.PlabAllocatorPtr + U(8)

        issueDirectRead(io.Mreq, addr, U(24), sDecideAllocateMode) { rd =>
          allocator_ptr_valid := True
          allocator_ptr_cache := rd(GCElementWidth - 1 downto 0)
          buffer0_ptr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          buffer1_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        }
      } otherwise {
        goto(sDecideAllocateMode)
      }
    }

    sDecideAllocateMode.whenIsActive {
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

    sCheckBufferPtr.whenIsActive {
      when(!buffer_valid(destAttrIdx)) {
        val addr = Mux(destAttrType === 0, buffer0_ptr, buffer1_ptr)

        issueDirectRead(io.Mreq, addr, U(8), sReadBufferMeta) { rd =>
          buffer_valid(destAttrIdx) := True
          buffer_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
        }
      } otherwise {
        goto(sReadBufferMeta)
      }
    }

    sReadBufferMeta.whenIsActive {
      val addr = activeBufferPtr + U"x30"

      issueDirectRead(io.Mreq, addr, U(32), sPrepareWriteOldPlabHeader) { rd =>
        top_ptr := rd(GCElementWidth - 1 downto 0)
        hard_end_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        off48_buffer_ptr := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
      }
    }

    sPrepareWriteOldPlabHeader.whenIsActive {
      when(top_ptr < hard_end_ptr) {
        val enoughWords = words >= headSize
        val klassPtr = Mux(enoughWords, io.ConfigIO.IntArrayKlassObj, io.ConfigIO.ObjectKlassObj)
        val compressedKlassPtr = ((klassPtr - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64)

        val writeOff0 = U(1, 64 bits)
        val writeOff8 = Mux(
          io.ConfigIO.UseCompressedKlassPointers,
          compressedKlassPtr,
          klassPtr.resize(64)
        )
        val writeOff12_16 = times2(words - headSize).resize(32)

        val compressedWriteValue = Cat(writeOff12_16, writeOff8.resize(32), writeOff0).asUInt.resize(MMUDataWidth)
        val normalWriteValue = Cat(writeOff12_16, writeOff8, writeOff0).asUInt.resize(MMUDataWidth)

        val writeValue = Mux(
          io.ConfigIO.UseCompressedKlassPointers,
          compressedWriteValue,
          normalWriteValue
        )
        val writeSize = Mux(
          io.ConfigIO.UseCompressedKlassPointers && enoughWords,
          U(16),
          Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), Mux(enoughWords, U(20), U(16)))
        )

        writeSizeReg := writeSize.resized
        writeValueReg := writeValue

        goto(sWriteOldPlabHeader)
      } otherwise {
        goto(sWaitParAllocate)
      }
    }

    sWriteOldPlabHeader.whenIsActive {
      issueDirectWriteWithoutResp(io.Mreq, top_ptr, writeSizeReg, writeValueReg, sWaitParAllocate)()
    }

    sWaitParAllocate.whenIsActive {
      val hasParResult = io.ToParAllocate.done.valid || par_allocate_done

      val returnObjPtr = Mux(
        par_allocate_done,
        par_allocate_destObj,
        io.ToParAllocate.done.payload.DestObjPtr
      )

      val returnPlabSize = Mux(
        par_allocate_done,
        par_allocate_plabSize,
        io.ToParAllocate.done.payload.ActualPlabSize
      )

      when(hasParResult) {
        par_allocate_done := False
        destObjPtr := returnObjPtr
        actualPlabSize := returnPlabSize

        when(par_allocate_return) {
          par_allocate_return := False
          finish(returnObjPtr, plab_refill_failed)
        } otherwise {
          when(returnObjPtr =/= 0) {
            goto(sWriteRefillLow32B)
          } elsewhen(top_ptr < hard_end_ptr) {
            goto(sWriteCollapseBuffer)
          } otherwise {
            par_allocate_return := True
            plab_refill_failed := True
            goto(sSendDirectRetry)
          }
        }
      }
    }

    sWriteRefillLow32B.whenIsActive {
      val delta = actualPlabSize - U(2, GCElementWidth bits)
      val requestedWordSize = size.resize(GCElementWidth)
      val off20 = actualPlabSize
      val off28 = destObjPtr
      val off30 = Mux(
        delta >= requestedWordSize,
        destObjPtr + times8(requestedWordSize),
        destObjPtr
      )
      val off38 = destObjPtr + times8(delta)
      val lowData = Cat(off38, off30, off28, off20).asUInt
      val addr = activeBufferPtr + U"x20"

      issueDirectWriteWithoutResp(io.Mreq, addr, U(32), lowData, sWriteRefillHigh16B)()
    }

    sWriteRefillHigh16B.whenIsActive {
      val off40 = destObjPtr + times8(actualPlabSize)
      val off48 = off48_buffer_ptr + actualPlabSize
      val highData = Cat(off48, off40).asUInt
      val addr = activeBufferPtr + U"x40"

      issueDirectWriteWithoutResp(io.Mreq, addr, U(16), highData, sIdle) {
        io.ToAllocate.done.valid := True
        io.ToAllocate.done.payload.DestOopPtr := destObjPtr
        io.ToAllocate.done.payload.PlabRefillFailed := False
        dbg(Seq("Refill buffer done, destObj = ", destObjPtr))
      }
    }

    sWriteCollapseBuffer.whenIsActive {
      val data = Cat(hard_end_ptr, hard_end_ptr, hard_end_ptr).asUInt
      val addr = activeBufferPtr + U"x28"

      issueDirectWriteWithoutResp(io.Mreq, addr, U(24), data, sSendDirectRetry) {
        par_allocate_return := True
        plab_refill_failed := True
      }
    }

    sSendDirectRetry.whenIsActive {
      sendToParAllocate(sWaitParAllocate, size.resize(GCElementWidth), size.resize(GCElementWidth))
    }
  }
}

object GCAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCAllocate())
}