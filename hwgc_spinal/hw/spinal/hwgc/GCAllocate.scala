package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
class GCAllocate extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToAllocate = slave(new GCToAllocate)
    val ToParAllocate = master(new GCToParAllocate)
    val ConfigIO = slave(new GCAllocateConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := False

  io.ToAllocate.clearOut()
  io.ToParAllocate.clearIn()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(10)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCAllocate<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def resetState(destObj: UInt): Unit = {
    io.ToAllocate.Done := True
    io.ToAllocate.DestObjPtr := destObj
    state := overall_state.states(0)
    dbg(Seq("The task has done, and the destObj is ", destObj))
  }

  def sendToParAllocate(select: Bool, minWordSize: UInt, desiredWordSize: UInt): Unit = {
    io.ToParAllocate.Valid := True

    io.ToParAllocate.NodeIndex := U(0)
    io.ToParAllocate.MinWordSize := minWordSize
    io.ToParAllocate.DestAttrIdx := Mux(destAttrType === 0, U(0), U(1))
    io.ToParAllocate.AllocatorPtr := allocator_ptr_cache
    io.ToParAllocate.DesiredWordSize := desiredWordSize

    when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
      state := Mux(select, overall_state.states(4), overall_state.states(7))
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val size = RegInit(U(0, 32 bits))
  val destAttrType = RegInit(U(0, 8 bits))

  val destObjPtr = RegInit(U(0, GCElementWidth bits))
  val actualPlabSize = RegInit(U(0, GCElementWidth bits))

  val plab_word_size = RegInit(U(0, GCElementWidth bits))
  val required_in_plab = RegInit(U(0, GCElementWidth bits))
  val buffer0_ptr = RegInit(U(0, GCElementWidth bits))
  val buffer1_ptr = RegInit(U(0, GCElementWidth bits))
  val top_ptr = RegInit(U(0, GCElementWidth bits))
  val hard_end_ptr = RegInit(U(0, GCElementWidth bits))

  val words = (hard_end_ptr - top_ptr) / U(8)
  val headSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(2), U(3))
  val destAttrIdx = Mux(destAttrType === 0, U(0), U(1))

  val plab_stats_valid = Vec(RegInit(False), 2)
  val plab_stats_value = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val allocator_ptr_valid = RegInit(False)
  val allocator_ptr_cache = RegInit(U(0, GCElementWidth bits))
  val buffer_valid = Vec(RegInit(False), 2)
  val buffer_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  val par_allocate_return = RegInit(False)
  val par_allocate_done = RegInit(False)
  val par_allocate_destObj = RegInit(U(0, GCElementWidth bits))
  val par_allocate_plabSize = RegInit(U(0, GCElementWidth bits))
  when(io.ToParAllocate.Done){
    par_allocate_done := True
    par_allocate_destObj := io.ToParAllocate.DestObjPtr
    par_allocate_plabSize := io.ToParAllocate.ActualPlabSize
  }

  switch(state){
    is(overall_state.states(0)){
      io.ToAllocate.Ready := True
      when(io.ToAllocate.Valid && io.ToAllocate.Ready){
        size := io.ToAllocate.Size
        destAttrType := io.ToAllocate.DestAttrType
        state := overall_state.states(1)

        dbg(Seq("Receive the task which sent to Allocate module. size = ", io.ToAllocate.Size, "destAttrType = ", io.ToAllocate.DestAttrType))
      }
    }

    is(overall_state.states(1)){
      when(!plab_stats_valid(destAttrIdx)){
        val addr = io.ConfigIO.G1h + Mux(destAttrType === 0, U"x250", U"x2e0") + U"x30"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          plab_stats_valid(destAttrIdx) := True
          plab_stats_value(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(2)
        }
      }.otherwise{
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      plab_word_size := Min(Max(plab_stats_value(destAttrIdx), U"x102"), U"x40000")
      required_in_plab := (size + 2).resize(GCElementWidth)

      when(!allocator_ptr_valid){
        val addr = io.ConfigIO.PlabAllocatorPtr + U(8)
        issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
          allocator_ptr_valid := True
          allocator_ptr_cache := rd(GCElementWidth - 1 downto 0)
          buffer0_ptr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          buffer1_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          state := overall_state.states(3)
        }
      }.otherwise{
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      val cond = required_in_plab <= plab_word_size && required_in_plab * U(100) < plab_word_size * U(10)
      when(cond){
        par_allocate_return := False
        sendToParAllocate(True, required_in_plab, plab_word_size)
      }.otherwise{
        par_allocate_return := True
        sendToParAllocate(False, size.resize(GCElementWidth), size.resize(GCElementWidth))
      }
    }

    is(overall_state.states(4)){
      when(!buffer_valid(destAttrIdx)){
        val addr = Mux(destAttrType === 0, buffer0_ptr, buffer1_ptr)
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          buffer_valid(destAttrIdx) := True
          buffer_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(5)
        }
      }.otherwise{
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      val addr = buffer_cache(destAttrIdx) + U"x30"
      issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
        top_ptr := rd(GCElementWidth - 1 downto 0)
        hard_end_ptr := rd(GCElementWidth * 3 - 1  downto GCElementWidth * 2)
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(6)){
      when(top_ptr < hard_end_ptr){
        val cond = words >= headSize
        val temp_klass_ptr = Mux(cond, io.ConfigIO.intArrayKlassObj, io.ConfigIO.objectKlassObj)

        val writeOff0 = U(1, 64 bits) // markWord
        val writeOff8 = Mux(io.ConfigIO.UseCompressedKlassPointers, ((temp_klass_ptr - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64), temp_klass_ptr) // klass
        val writeOff12_16 = ((words - headSize) * 2).resize(32) // arrayLen
        val writeValue = Mux(io.ConfigIO.UseCompressedKlassPointers, Cat(writeOff12_16, writeOff8.resize(32), writeOff0).resize(MMUDataWidth), Cat(writeOff12_16, writeOff8, writeOff0).resize(MMUDataWidth)).asUInt
        val writeSize =
          Mux(io.ConfigIO.UseCompressedKlassPointers && cond, U(16),
            Mux(io.ConfigIO.UseCompressedKlassPointers, U(12),
              Mux(cond, U(20), U(16)))).resize(LineBytesNumBitSize)

        issueReq(io.Mreq, top_ptr, True, writeSize, writeValue, issued) { _ =>
          state := overall_state.states(7)
        }
      }.otherwise{
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      when(io.ToParAllocate.Done || par_allocate_done){
        par_allocate_done := False

        val returnObjPtr = Mux(par_allocate_done, par_allocate_destObj, io.ToParAllocate.DestObjPtr)
        destObjPtr := returnObjPtr
        actualPlabSize := Mux(par_allocate_done, par_allocate_plabSize, io.ToParAllocate.ActualPlabSize)

        when(par_allocate_return){
          par_allocate_return := False
          resetState(returnObjPtr)
        }.otherwise{
          state := overall_state.states(8)
        }
      }
    }

    is(overall_state.states(8)){
      when(destObjPtr =/= 0){
        val delta = actualPlabSize - 2
        val off28 = destObjPtr
        val off30 = Mux(delta >= size, (destObjPtr + size * 8).resize(GCElementWidth), destObjPtr)
        val off38 = (destObjPtr + delta * 8).resize(GCElementWidth)
        val off40 = (destObjPtr + actualPlabSize * 8).resize(GCElementWidth)
        val writeValue = Cat(off40, off38, off30, off28).asUInt
        val addr = buffer_cache(destAttrIdx) + U"x28"
        issueReq(io.Mreq, addr, True, U(32), writeValue, issued) { _ =>
          resetState(Mux(delta < size, U(0), destObjPtr))
        }
      }.otherwise{
        par_allocate_return := True
        sendToParAllocate(False, size.resize(GCElementWidth), size.resize(GCElementWidth))
      }
    }
  }
}

object GCAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCAllocate())
}