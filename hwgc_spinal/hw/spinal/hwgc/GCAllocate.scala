package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
class GCAllocate extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToAllocate = slave(new GCToAllocate)
    val ToParAllocate = master(new GCToParAllocate)
    val ToAttemptAllocate = master(new GCToAttemptAllocate)
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
  io.ToAttemptAllocate.clearIn()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(25)(_ => newElement())
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

  def resetDuringGCSelect(): Unit = {
    when(during_gc_select){
      resetState(destObjPtr)
      dbg(Seq("The task has done, and the destObj is ", destObjPtr))
    }.otherwise{
      state := overall_state.states(7)
    }
  }

  def sendToParAllocate(select: Bool) : Unit = {
    io.ToParAllocate.Valid := True
    io.ToParAllocate.excuteAll := destAttrType =/= 0
    io.ToParAllocate.botUpdates := True
    io.ToParAllocate.allocRegion := alloc_region
    io.ToParAllocate.minWordSize := min_word_size
    io.ToParAllocate.desiredWordSize := desired_word_size

    when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
      when(select) {
        state := overall_state.states(15)
      }.otherwise{
        state := overall_state.states(11)
      }
    }
  }

  def sendToState8(select: Bool, size1: UInt, size2: UInt): Unit = {
    during_gc_select := select
    min_word_size := size1
    desired_word_size := size2
    state := overall_state.states(8)
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val size = RegInit(U(0, 32 bits))
  val destAttrType = RegInit(U(0, 8 bits))

  val destObjPtr = RegInit(U(0, GCElementWidth bits))
  val actualPlabSize = RegInit(U(0, GCElementWidth bits))

  val plab_word_size = RegInit(U(0, GCElementWidth bits))
  val required_in_plab = RegInit(U(0, GCElementWidth bits))

  val region_top = RegInit(U(0, GCElementWidth bits))
  val region_end = RegInit(U(0, GCElementWidth bits))
  val offset_buffer48 = RegInit(U(0, GCElementWidth bits))
  val during_gc_select = RegInit(False)
  val min_word_size = RegInit(U(0, GCElementWidth bits))
  val desired_word_size = RegInit(U(0, GCElementWidth bits))
  val region_ptr = RegInit(U(0, GCElementWidth bits))
  val alloc_region = RegInit(U(0, GCElementWidth bits))

  val par_allocate_done = RegInit(False)
  val attempt_allocate_done  = RegInit(False)

  val off_plab_allocator10 = Reg(U(0, GCElementWidth bits))
  val off_plab_allocator18 = Reg(U(0, GCElementWidth bits))

  val plab_stats_valid = Vec(RegInit(False), 2)
  val plab_stats_value = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val allocator_ptr_valid = RegInit(False)
  val allocator_ptr_cache = RegInit(U(0, GCElementWidth bits))
  val buffer_valid = Vec(RegInit(False), 2)
  val buffer_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val region_ptr_valid = RegInit(False)
  val region_ptr_cache = RegInit(U(0, GCElementWidth bits))
  val is_full_cache = RegInit(U(0, 16 bits))

  val destAttrIdx = Mux(destAttrType === 0, U(0), U(1))
  val headSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(2), U(3))

  when(io.ToParAllocate.Done){
    par_allocate_done := True
  }

  when(io.ToAttemptAllocate.Done){
    attempt_allocate_done := True
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
      when(!plab_stats_valid(destAttrIdx)) {
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
          off_plab_allocator10 := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          off_plab_allocator18 := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          state := overall_state.states(3)
        }
      }.otherwise{
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      val cond = required_in_plab <= plab_word_size && required_in_plab * U(100) < plab_word_size * U(10)
      when(cond){
        when(!buffer_valid(destAttrIdx)) {
          val addr = Mux(destAttrType === 0, off_plab_allocator10, off_plab_allocator18)
          issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
            buffer_valid(destAttrIdx) := True
            buffer_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
            state := overall_state.states(4)
          }
        }.otherwise{
          state := overall_state.states(4)
        }
      }.otherwise{
        sendToState8(True, size.resize(GCElementWidth bits), size.resize(GCElementWidth bits))
      }
    }

    is(overall_state.states(4)){
      val addr = buffer_cache(destAttrIdx) + U"x30"
      issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
        region_top := rd(GCElementWidth - 1 downto 0)
        region_end := rd(GCElementWidth * 3 - 1  downto GCElementWidth * 2)
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      when(region_top < region_end){
        val words = (region_end - region_top) / U(8)
        val temp = Mux(words >= headSize, io.ConfigIO.intArrayKlassObj, io.ConfigIO.objectKlassObj)
        val writeOff0 = U(1) // markWord
        val writeOff8 = Mux(io.ConfigIO.UseCompressedKlassPointers, ((temp - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64 bits), temp) // klass
        val writeLen = (words - headSize) * U(2)
        val writeValue = Mux(io.ConfigIO.UseCompressedKlassPointers, Cat(writeLen.resize(32 bits), writeOff8.resize(32 bits), writeOff0.resize(64 bits)).resize(MMUDataWidth),
                             Cat(writeLen.resize(32 bits), writeOff8.resize(64 bits), writeOff0.resize(64 bits)).resize(MMUDataWidth)).asUInt
        val writeSize = Mux(words >= headSize && io.ConfigIO.UseCompressedKlassPointers, U(16), // write arraylen and klass use compressed, 8 + 4 + 4
                        Mux(words >= headSize, U(20), // 8 + 8 + 4
                        Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16)))).resize(LineBytesNumBitSize)
        issueReq(io.Mreq, region_top, True, writeSize, writeValue, issued) { _ =>
          state := overall_state.states(6)
        }
      }.otherwise{
        sendToState8(False, required_in_plab, plab_word_size)
      }
    }

    is(overall_state.states(6)){
      val addr = buffer_cache(destAttrIdx) + U"x28"
      val writeValue = Cat(region_end, region_end, region_end).asUInt
      issueReq(io.Mreq, addr, True, U(24), writeValue, issued) { _ =>
        sendToState8(False, required_in_plab, plab_word_size)
      }
    }

    is(overall_state.states(7)){
      when(destObjPtr =/= U(0)){
        val delta = actualPlabSize - 2
        val off28 = destObjPtr
        val off30 = Mux(delta >= size, (destObjPtr + size * U(8)).resize(GCElementWidth bits), destObjPtr)
        val off38 = (destObjPtr + delta * U(8)).resize(GCElementWidth bits)
        val off40 = (destObjPtr + actualPlabSize * U(8)).resize(GCElementWidth bits)
        val writeValue = Cat(off40, off38, off30, off28).asUInt
        val addr = buffer_cache(destAttrIdx) + U"x28"
        issueReq(io.Mreq, addr, True, U(32), writeValue, issued) { _ =>
          resetState(Mux(delta < size, U(0), destObjPtr))
        }
      }.otherwise{
        sendToState8(True, size.resize(GCElementWidth bits), size.resize(GCElementWidth bits))
      }
    }

    is(overall_state.states(8)){
      when(destAttrType === 0 && !region_ptr_valid){
        val addr = allocator_ptr_cache + U"x10"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          is_full_cache := rd(15 downto 0)

          region_ptr_valid := True
          region_ptr_cache := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
          region_ptr := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)

          state := overall_state.states(9)
        }
      }.otherwise{
        region_ptr := Mux(destAttrType === 0, region_ptr_cache, allocator_ptr_cache + U"x30")
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      val addr = region_ptr + U(8)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        alloc_region := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      sendToParAllocate(False)
    }

    is(overall_state.states(11)){
      when(!region_ptr_valid){
        val addr = allocator_ptr_cache + U"x10"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          is_full_cache := rd(15 downto 0)

          region_ptr_valid := True
          region_ptr_cache := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
          region_ptr := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)

          state := overall_state.states(12)
        }
      }.otherwise{
        state := overall_state.states(12)
      }
    }

    is(overall_state.states(12)){
      when(io.ToParAllocate.Done || par_allocate_done){
        par_allocate_done := False
        destObjPtr := io.ToParAllocate.DestObjPtr
        actualPlabSize := io.ToParAllocate.ActualPlabSize
        state := overall_state.states(13)
      }
    }

    is(overall_state.states(13)){
      when(destObjPtr === 0){
        val is_full = Mux(destAttrType === 0, is_full_cache(7 downto 0), is_full_cache(15 downto 8))
        when(is_full =/= 0){
          resetDuringGCSelect()
        }.otherwise{
          issueReq(io.Mreq, io.ConfigIO.LockPtr, True, U(8), io.ConfigIO.Thread, issued) { _ =>
            state := overall_state.states(14)
          }
        }
      }.otherwise{
        resetDuringGCSelect()
      }
    }

    is(overall_state.states(14)){
      sendToParAllocate(True)
    }

    is(overall_state.states(15)){
      when(io.ToParAllocate.Done || par_allocate_done){
        par_allocate_done := False
        destObjPtr := io.ToParAllocate.DestObjPtr
        actualPlabSize := io.ToParAllocate.ActualPlabSize
        state := overall_state.states(16)
      }
    }

    is(overall_state.states(16)){
      when(destObjPtr === U(0)){
        io.ToAttemptAllocate.Valid := True
        io.ToAttemptAllocate.regionPtr := region_ptr
        io.ToAttemptAllocate.allocRegion := alloc_region
        io.ToAttemptAllocate.desiredWordSize := desired_word_size

        when(io.ToAttemptAllocate.Valid && io.ToAttemptAllocate.Ready){
          state := overall_state.states(17)
        }
      }.otherwise{
        state := overall_state.states(18)
      }
    }

    is(overall_state.states(17)){
      when(io.ToAttemptAllocate.Done || attempt_allocate_done){
        attempt_allocate_done := False
        destObjPtr := io.ToAttemptAllocate.DestObjPtr
        actualPlabSize := io.ToAttemptAllocate.ActualPlabSize
        state := overall_state.states(18)
      }
    }

    is(overall_state.states(18)){
      when(destObjPtr === 0) {
        when(destAttrType === 0) {
          is_full_cache(7 downto 0) := 1
        }.otherwise {
          is_full_cache(15 downto 8) := 1
        }
      }

      issueReq(io.Mreq, io.ConfigIO.LockPtr, True, U(8), U(0), issued) { _ =>
        resetDuringGCSelect()
      }
    }
  }
}

object GCAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCAllocate())
}