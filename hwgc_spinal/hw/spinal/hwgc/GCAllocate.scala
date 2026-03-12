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
      state := overall_state.states(11)
    }
  }

  def sendToParAllocate(select: Bool) : Unit = {
    io.ToParAllocate.Valid := True
    io.ToParAllocate.excuteAll := destAttrType =/= U(0)
    io.ToParAllocate.botUpdates := True
    io.ToParAllocate.allocRegion := alloc_region
    io.ToParAllocate.minWordSize := min_word_size
    io.ToParAllocate.desiredWordSize := desired_word_size

    when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
      when(select) {
        state := overall_state.states(20)
      }.otherwise{
        state := overall_state.states(16)
      }
    }
  }

  def sendToState13(select: Bool, size1: UInt, size2: UInt): Unit = {
    during_gc_select := select
    min_word_size := size1
    desired_word_size := size2
    state := overall_state.states(13)
  }


  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val size = RegInit(U(0, 32 bits))
  val destAttrType = RegInit(U(0, 8 bits))

  val destObjPtr = RegInit(U(0, GCElementWidth bits))
  val actualPlabSize = RegInit(U(0, GCElementWidth bits))

  val temp_value = RegInit(U(0, GCElementWidth bits))
  val plab_word_size = RegInit(U(0, GCElementWidth bits))
  val required_in_plab = RegInit(U(0, GCElementWidth bits))
  val allocator_ptr = RegInit(U(0, GCElementWidth bits))
  val off_plab_allocator10 = RegInit(U(0, GCElementWidth bits))
  val off_plab_allocator18 = RegInit(U(0, GCElementWidth bits))
  val buffer = RegInit(U(0, GCElementWidth bits))
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
      val addr = io.ConfigIO.G1h + Mux(destAttrType === U(0), U"x250", U"x2e0") + U"x30"
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        temp_value := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      plab_word_size := Min(Max(temp_value, U"x102"), U"x40000")
      required_in_plab := (size + U(2)).resize(GCElementWidth bits)

      val addr = io.ConfigIO.PlabAllocatorPtr + U(8)
      issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
        allocator_ptr := rd(GCElementWidth - 1 downto 0)
        off_plab_allocator10 := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        off_plab_allocator18 := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      val cond = required_in_plab * U(100) < plab_word_size * U(10)
      when(required_in_plab <= plab_word_size && cond){
        val addr = Mux(destAttrType === U(0), off_plab_allocator10, off_plab_allocator18)
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          buffer := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(4)
        }
      }.otherwise{
        sendToState13(True, size.resize(GCElementWidth bits), size.resize(GCElementWidth bits))
      }
    }

    is(overall_state.states(4)){
      val addr = buffer + U"x30"
      issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
        region_top := rd(GCElementWidth - 1 downto 0)
        region_end := rd(GCElementWidth * 3 - 1  downto GCElementWidth * 2)
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      when(region_top < region_end){
        val words = (region_end - region_top) / U(8)
        val headSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(2), U(3))
        val temp = Mux(words >= headSize, io.ConfigIO.intArrayKlassObj, io.ConfigIO.objectKlassObj)
        val writeOff0 = U(1)
        val writeOff8 = Mux(io.ConfigIO.UseCompressedKlassPointers, ((temp - io.ConfigIO.CompressedKlassPointerBase) >> io.ConfigIO.CompressedKlassPointerShift).resize(64 bits), temp)
        val writeLen = (words - headSize) * U(2)
        val writeValue = Mux(io.ConfigIO.UseCompressedKlassPointers, Cat(writeLen.resize(32 bits), writeOff8.resize(32 bits), writeOff0.resize(64 bits)).resize(MMUDataWidth), Cat(writeLen.resize(32 bits), writeOff8.resize(64 bits), writeOff0.resize(64 bits)).resize(MMUDataWidth)).asUInt
        val writeSize = Mux(words >= headSize && io.ConfigIO.UseCompressedKlassPointers, U(16),
                        Mux(words >= headSize, U(20),
                        Mux(io.ConfigIO.UseCompressedKlassPointers, U(12), U(16)))).resize(8 bits)
        issueReq(io.Mreq, region_top, True, writeSize, writeValue, issued) { _ =>
          state := overall_state.states(6)
        }
      }.otherwise{
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(6)){
      val addr = buffer + U"x28"
      val writeValue = Cat(region_end, region_end, region_end).asUInt
      issueReq(io.Mreq, addr, True, U(24), writeValue, issued) { _ =>
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      val addr = buffer + U"x48"
      issueReq(io.Mreq, addr, False, U(16), U(0), issued) { rd =>
        offset_buffer48 := rd(GCElementWidth - 1 downto 0)
        temp_value := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
        state := overall_state.states(8)
      }
    }

    is(overall_state.states(8)){
      val addr = buffer + U"x50"
      val writeValue = (temp_value + (region_end - region_top) / U(8)).resize(GCElementWidth bits)
      issueReq(io.Mreq, addr, True, U(8), writeValue, issued) { _ =>
        state := overall_state.states(9)
      }
    }

    is(overall_state.states(9)){
      val addr = (io.ConfigIO.PlabAllocatorPtr + U"x30" + destAttrType * U(8)).resize(MMUAddrWidth bits)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        temp_value := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(10)
      }
    }

    is(overall_state.states(10)){
      val addr = (io.ConfigIO.PlabAllocatorPtr + U"x30" + destAttrType * U(8)).resize(MMUAddrWidth bits)
      val writeValue = temp_value + U(1)
      issueReq(io.Mreq, addr, True, U(8), writeValue, issued) { _ =>
        sendToState13(False, required_in_plab, plab_word_size)
      }
    }

    is(overall_state.states(11)){
      when(destObjPtr =/= U(0)){
        val off20 = actualPlabSize
        val off28 = destObjPtr
        val off30 = Mux(actualPlabSize - U(2) >= size, (destObjPtr + size * U(8)).resize(GCElementWidth bits), destObjPtr)
        val off38 = (destObjPtr + (actualPlabSize - U(2)) * U(8)).resize(GCElementWidth bits)
        val writeValue = Cat(off38, off30, off28, off20).asUInt
        val addr = buffer + U"x20"
        issueReq(io.Mreq, addr, True, U(32), writeValue, issued) { _ =>
          state := overall_state.states(12)
        }
      }.otherwise{
        sendToState13(True, size.resize(GCElementWidth bits), size.resize(GCElementWidth bits))
      }
    }

    is(overall_state.states(12)){
      val off40 = (destObjPtr + actualPlabSize * U(8)).resize(GCElementWidth bits)
      val off48 = offset_buffer48 + actualPlabSize
      val writeValue = Cat(off48, off40).asUInt
      val addr = buffer + U"x40"
      issueReq(io.Mreq, addr, True, U(16), writeValue, issued) { _ =>
        resetState(Mux(actualPlabSize - U(2) < size, U(0), destObjPtr))
      }
    }

    is(overall_state.states(13)){
      when(destAttrType === U(0)){
        val addr = allocator_ptr + U"x28"
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          region_ptr := rd(GCElementWidth - 1 downto 0)
          state := overall_state.states(14)
        }
      }.otherwise{
        region_ptr := allocator_ptr + U"x30"
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      val addr = region_ptr + U(8)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        alloc_region := rd(GCElementWidth - 1 downto 0)
        state := overall_state.states(15)
      }
    }

    is(overall_state.states(15)){
      sendToParAllocate(False)
    }

    is(overall_state.states(16)){
      when(io.ToParAllocate.Done || par_allocate_done){
        par_allocate_done := False
        destObjPtr := io.ToParAllocate.DestObjPtr
        actualPlabSize := io.ToParAllocate.ActualPlabSize
        state := overall_state.states(17)
      }
    }

    is(overall_state.states(17)){
      when(destObjPtr === U(0)){
        val addr = allocator_ptr + U"x10"
        issueReq(io.Mreq, addr, False, U(1), U(0), issued) { rd =>
          temp_value := rd(7 downto 0).resize(GCElementWidth bits)
          state := overall_state.states(18)
        }
      }.otherwise{
        resetDuringGCSelect()
      }
    }

    is(overall_state.states(18)){
      val is_full = Mux(destAttrType === U(0), temp_value(0), temp_value(1))
      when(is_full){
        resetDuringGCSelect()
      }.otherwise{
        issueReq(io.Mreq, io.ConfigIO.LockPtr, True, U(8), io.ConfigIO.Thread, issued) { _ =>
          state := overall_state.states(19)
        }
      }
    }

    is(overall_state.states(19)){
      sendToParAllocate(True)
    }

    is(overall_state.states(20)){
      when(io.ToParAllocate.Done || par_allocate_done){
        par_allocate_done := False
        destObjPtr := io.ToParAllocate.DestObjPtr
        actualPlabSize := io.ToParAllocate.ActualPlabSize
        state := overall_state.states(21)
      }
    }

    is(overall_state.states(21)){
      when(destObjPtr === U(0)){
        io.ToAttemptAllocate.Valid := True
        io.ToAttemptAllocate.regionPtr := region_ptr
        io.ToAttemptAllocate.allocRegion := alloc_region
        io.ToAttemptAllocate.desiredWordSize := desired_word_size

        when(io.ToAttemptAllocate.Valid && io.ToAttemptAllocate.Ready){
          state := overall_state.states(22)
        }
      }.otherwise{
        state := overall_state.states(24)
      }
    }

    is(overall_state.states(22)){
      when(io.ToAttemptAllocate.Done || attempt_allocate_done){
        attempt_allocate_done := False
        destObjPtr := io.ToAttemptAllocate.DestObjPtr
        actualPlabSize := io.ToAttemptAllocate.ActualPlabSize
        state := overall_state.states(23)
      }
    }

    is(overall_state.states(23)){
      when(destObjPtr === U(0)) {
        val addr = Mux(destAttrType === U(0), allocator_ptr + U"x10", allocator_ptr + U"x11")
        issueReq(io.Mreq, addr, True, U(1), U(1), issued) { _ =>
          state := overall_state.states(24)
        }
      }.otherwise{
        state := overall_state.states(24)
      }
    }

    is(overall_state.states(24)){
      issueReq(io.Mreq, io.ConfigIO.LockPtr, True, U(8), U(0), issued) { _ =>
        resetDuringGCSelect()
      }
    }
  }
}

object GCAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCAllocate())
}