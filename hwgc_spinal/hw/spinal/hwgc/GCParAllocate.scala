package hwgc

import spinal.core
import spinal.core._
import spinal.lib._

import scala.language.postfixOps
class GCParAllocate extends Module with GCParameters with HWParameters {
  val io = new Bundle{
    val Mreq = master(new LocalMMUIO)
    val ToNewGCAlloc = master(new GCToNewGCAlloc)
    val ToParAllocate = slave(new GCToParAllocate)
    val ConfigIO = slave(new GCParAllocateConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ToParAllocate.clearOut()
  io.ToNewGCAlloc.clearIn()

  object overall_state extends SpinalEnum {
    val states = Array.tabulate(10)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCParAllocate<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def resetState(): Unit = {
    io.ToParAllocate.Done := True
    io.ToParAllocate.DestObjPtr := destObjPtr
    io.ToParAllocate.ActualPlabSize := actualPlabSize
    state := overall_state.states(0)
    dbg(Seq("The task in par_allocate module has done"))
  }


  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val nodeIndex =  RegInit(U(0, 8 bits))
  val destAttrIdx = RegInit(U(0, 1 bits))
  val minWordSize = RegInit(U(0, GCElementWidth bits))
  val allocatorPtr = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val actualPlabSize = RegInit(U(0, GCElementWidth bits))
  val destObjPtr = RegInit(U(0, GCElementWidth bits))

  val region_ptr_off10 = RegInit(U(0, GCElementWidth bits))
  val region_ptr_off18 = RegInit(U(0, GCElementWidth bits))
  val region_ptr_off20 = RegInit(False)
  val bot_updates = RegInit(False)
  val write_lock0 = RegInit(False)
  val blk_start = RegInit(U(0, GCElementWidth bits))
  val blk_end = RegInit(U(0, GCElementWidth bits))
  val end_index = RegInit(U(0, GCElementWidth bits))
  val begin = RegInit(U(0, GCElementWidth bits))
  val remaining = RegInit(U(0, GCElementWidth bits))
  val iterator = RegInit(U(0, 4 bits))
  val start_ptr = RegInit(U(0, GCElementWidth bits))

  val region_ptr_valid = Vec(RegInit(False), 2)
  val region_ptr_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val alloc_region_valid = Vec(RegInit(False), 2)
  val alloc_region_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  val alloc_about_valid = Vec(RegInit(False), 2)
  val alloc_top_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val alloc_end_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val alloc_bottom_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  val par_allocate_valid = RegInit(False)
  val bot_part_ptr = RegInit(U(0, GCElementWidth bits))
  val next_offset_threshold = RegInit(U(0, GCElementWidth bits))
  val index = RegInit(U(0, GCElementWidth bits))
  val bot_ptr = RegInit(U(0, GCElementWidth bits))

  val bot_ptr_about_valid = RegInit(False)
  val array = RegInit(U(0, GCElementWidth bits))
  val reserved_start = RegInit(U(0, GCElementWidth bits))

  val cm_valid = RegInit(False)
  val cm = RegInit(U(0, GCElementWidth bits))
  val root_regions_array_valid = RegInit(False)
  val root_regions_array = RegInit(U(0, GCElementWidth bits))
  val idx = RegInit(U(0, GCElementWidth bits))

  val new_gc_alloc_done = RegInit(False)
  val new_alloc_region = RegInit(U(0, GCElementWidth bits))
  when(io.ToNewGCAlloc.Done){
    new_gc_alloc_done := True
    new_alloc_region := io.ToNewGCAlloc.newAllocRegion
  }

  val parAllocateIml = new Area {
    val sub_state = RegInit(overall_state.states(0))

    val start = False
    val busy  = RegInit(False)
    val done  = RegInit(False)

    val min_word_size_r     = Reg(UInt(GCElementWidth bits)) init(0)
    val desired_word_size_r = Reg(UInt(GCElementWidth bits)) init(0)

    done := False

    def fire(min_word_size: UInt, desired_word_size: UInt): Unit = {
      when(!busy) {
        start := True
        min_word_size_r     := min_word_size
        desired_word_size_r := desired_word_size
      }
    }

    def finish(): Unit = {
      sub_state := overall_state.states(0)
      busy := False
      done := True
    }

    when(start && !busy) {
      busy := True
      sub_state := overall_state.states(0)
    }

    when(busy) {
      switch(sub_state) {
        is(overall_state.states(0)){
          when(!alloc_about_valid(destAttrIdx)){
            val addr = alloc_region_cache(destAttrIdx)
            issueReq(io.Mreq, addr, False, U(32), U(0), issued) { rd =>
              alloc_bottom_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
              alloc_end_cache(destAttrIdx) := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
              alloc_top_cache(destAttrIdx) := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
              alloc_about_valid(destAttrIdx) := True
              sub_state := overall_state.states(1)
            }
          }.otherwise{
            sub_state := overall_state.states(1)
          }
        }

        is(overall_state.states(1)){
          val available = ((alloc_end_cache(destAttrIdx) - alloc_top_cache(destAttrIdx)) >> 3).resize(GCElementWidth)
          val want_to_allocate = Mux(available > desired_word_size_r, desired_word_size_r, available)
          when(want_to_allocate >= min_word_size_r){
            val origin_top = alloc_top_cache(destAttrIdx)
            alloc_top_cache(destAttrIdx) := (origin_top + (want_to_allocate << 3)).resize(GCElementWidth)

            destObjPtr := origin_top
            actualPlabSize := want_to_allocate

            finish()
          }.otherwise{
            destObjPtr := U(0)
            val addr = alloc_region_cache(destAttrIdx) + U"x10"
            val writeValue = Cat(alloc_top_cache(destAttrIdx)).asUInt
            issueReq(io.Mreq, addr, True, U(8), writeValue, issued) { _ =>
            }
            when(issued){
              issued := False
              finish()
            }
          }
        }
      }
    }
  }

  val parAllocate = new Area {
    val sub_state = RegInit(overall_state.states(0))

    val start = False
    val busy  = RegInit(False)
    val done  = RegInit(False)

    val min_word_size_r     = Reg(UInt(GCElementWidth bits)) init(0)
    val desired_word_size_r = Reg(UInt(GCElementWidth bits)) init(0)

    done := False

    def fire(min_word_size: UInt, desired_word_size: UInt): Unit = {
      when(!busy) {
        start := True
        min_word_size_r     := min_word_size
        desired_word_size_r := desired_word_size
      }
    }

    def finish(): Unit = {
      sub_state := overall_state.states(0)
      busy := False
      done := True
    }

    when(start && !busy) {
      busy := True
      sub_state := overall_state.states(0)
    }

    when(busy) {
      switch(sub_state) {
        // 发起 parAllocateIml
        is(overall_state.states(0)){
          parAllocateIml.fire(min_word_size_r, desired_word_size_r)
          sub_state := overall_state.states(1)
        }

        // 等待 parAllocateIml 完成
        is(overall_state.states(1)){
          when(parAllocateIml.done){
            when(destObjPtr =/= 0 && bot_updates){
              sub_state := overall_state.states(2)
            }.elsewhen(par_allocate_valid) {
              sub_state := overall_state.states(9)
            }.otherwise{
              finish()
            }
          }
        }

        is(overall_state.states(2)){
          blk_start := destObjPtr
          blk_end := (destObjPtr + (actualPlabSize << 3)).resize(GCElementWidth)

          when(!par_allocate_valid){
            val addr = alloc_region_cache(1) + U"x20"
            issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
              next_offset_threshold := rd(GCElementWidth - 1 downto 0)
              index := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
              bot_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
              bot_part_ptr := alloc_region_cache(1) + U"x20"
              par_allocate_valid := True
              sub_state := overall_state.states(3)
            }
          }.otherwise{
            sub_state := overall_state.states(3)
          }
        }

        is(overall_state.states(3)){
          when(blk_end <= next_offset_threshold){
            finish()
          }.otherwise{
            when(!bot_ptr_about_valid){
              issueReq(io.Mreq, bot_ptr, False, U(24), U(0), issued) { rd =>
                reserved_start := rd(GCElementWidth - 1 downto 0)
                array := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
                bot_ptr_about_valid := True
                sub_state := overall_state.states(4)
              }
            }.otherwise{
              sub_state := overall_state.states(4)
            }
          }
        }

        is(overall_state.states(4)){
          val writeValue = ((next_offset_threshold - blk_start) >> 3).resize(8)
          issueReq(io.Mreq, array, True, U(1), writeValue, issued) { _ =>
          }
          when(issued){
            issued := False
            sub_state := overall_state.states(5)
          }
        }

        is(overall_state.states(5)){
          val end_index_value = ((blk_end - 8 - reserved_start) >> 9).resize(GCElementWidth)
          end_index := end_index_value
          when(index + 1 <= end_index_value){
            sub_state := overall_state.states(6)
          }.otherwise{
            sub_state := overall_state.states(8)
          }
        }

        is(overall_state.states(6)){
          val rem_st = (reserved_start + ((index + 1) << 6) << 3).resize(GCElementWidth)
          val rem_end = (reserved_start + ((end_index << 6) + 64) << 3).resize(GCElementWidth)
          val start_card = ((((index + 1) << 6) << 3) >> 9).resize(GCElementWidth)
          val end_card = ((((end_index << 6) + 63) << 3) >> 9).resize(GCElementWidth)
          when(rem_st < rem_end && start_card <= end_card){
            remaining := end_card - start_card + 1
            begin := array + start_card
            iterator := 0
            sub_state := overall_state.states(7)
          }.otherwise{
            sub_state := overall_state.states(8)
          }
        }

        is(overall_state.states(7)){
          when(iterator < 14 && remaining > 0) {
            val chunk = (U(15) << (4 * iterator)).resize(GCElementWidth)
            val nbytes = Mux(remaining < chunk, remaining, chunk)

            val fillByte = (U(64, 8 bits) + iterator.resize(8)).asBits
            val lanes    = MMUDataWidth / 8
            val writeBytes = Vec(Bits(8 bits), lanes)

            for (b <- 0 until lanes) {
              writeBytes(b) := Mux(U(b) < nbytes, fillByte, B(0, 8 bits))
            }

            val writeValue = writeBytes.asBits.asUInt
            issueReq(io.Mreq, begin, True, nbytes, writeValue, issued) { _ =>
            }
            when(issued){
              issued := False
              begin := begin + nbytes
              remaining := remaining - nbytes
              iterator := iterator + 1
              sub_state := overall_state.states(7)
            }
          }.otherwise{
            sub_state := overall_state.states(8)
          }
        }

        is(overall_state.states(8)){
          index := end_index + 1
          next_offset_threshold := (reserved_start + ((end_index << 6) + 64) << 3).resize(GCElementWidth)
          finish()
        }

        is(overall_state.states(9)){
          val addr = bot_part_ptr
          val writeValue = Cat(index, next_offset_threshold).asUInt
          issueReq(io.Mreq, addr, True, U(16), writeValue, issued) { _ =>
          }
          when(issued){
            issued := False
            par_allocate_valid := False
            finish()
          }
        }
      }
    }
  }

  val attemptAlloc = new Area {
    val sub_state = RegInit(overall_state.states(0))

    val start = False
    val busy  = RegInit(False)
    val done  = RegInit(False)

    val desired_word_size_r = Reg(UInt(GCElementWidth bits)) init(0)
    val write_region_ptr_done = RegInit(False)
    val par_allocate_done = RegInit(False)

    done := False

    def fire(desired_word_size: UInt): Unit = {
      when(!busy) {
        start := True
        desired_word_size_r := desired_word_size
      }
    }

    def finish(): Unit = {
      sub_state := overall_state.states(0)
      busy := False
      done := True
      write_region_ptr_done := False
      par_allocate_done := False
    }

    when(start && !busy) {
      busy := True
      sub_state := overall_state.states(0)
    }

    when(busy) {
      switch(sub_state) {
        is(overall_state.states(0)){
          when(alloc_region_cache(destAttrIdx) =/= io.ConfigIO.DummyRegion){
            val allocated_bytes = alloc_top_cache(destAttrIdx) - alloc_bottom_cache(destAttrIdx) - region_ptr_off18
            val addr = io.ConfigIO.G1h + U"x3c1"
            issueReq(io.Mreq, addr, False, U(1), U(0), issued) { rd =>
              when(rd(0) && allocated_bytes > 0){
                sub_state := overall_state.states(1)
              }.otherwise{
                sub_state := overall_state.states(5)
              }
            }
          }.otherwise{
            sub_state := overall_state.states(5)
          }
        }

        is(overall_state.states(1)){
          when(!cm_valid){
            val addr = io.ConfigIO.G1h + U"x4e8"
            issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
              cm := rd(GCElementWidth - 1 downto 0)
              cm_valid := True
              sub_state := overall_state.states(2)
            }
          }.otherwise{
            sub_state := overall_state.states(2)
          }
        }

        is(overall_state.states(2)){
          when(!root_regions_array_valid){
            val addr = cm + U"xb0"
            issueReq(io.Mreq, addr, False, U(24), U(0), issued) { rd =>
              root_regions_array := rd(GCElementWidth - 1 downto 0)
              idx := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
              root_regions_array_valid := True
              sub_state := overall_state.states(3)
            }
          }.otherwise{
            sub_state := overall_state.states(3)
          }
        }

        is(overall_state.states(3)){
          val addr = alloc_region_cache(destAttrIdx) + U"xe8"
          issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
            start_ptr := rd(GCElementWidth - 1 downto 0)
            sub_state := overall_state.states(4)
          }
        }

        is(overall_state.states(4)){
          val addr = (root_regions_array + idx * U"x10").resize(MMUAddrWidth)
          val writeOff0 = start
          val writeOff8 = ((alloc_top_cache(destAttrIdx) - start_ptr) / U(8)).resize(GCElementWidth)
          val writeValue = Cat(writeOff8, writeOff0).asUInt
          issueReq(io.Mreq, addr, True, U(16), writeValue, issued) { _ =>
          }
          when(issued){
            issued := False
            idx := idx + 1
            sub_state := overall_state.states(5)
          }
        }

        is(overall_state.states(5)){
          io.ToNewGCAlloc.Valid := True
          io.ToNewGCAlloc.regionPtr := region_ptr_cache(destAttrIdx)
          io.ToNewGCAlloc.destAttrIdx := destAttrIdx
          when(io.ToNewGCAlloc.Valid && io.ToNewGCAlloc.Ready){
            sub_state := overall_state.states(6)
          }
        }

        is(overall_state.states(6)){
          when(io.ToNewGCAlloc.Done || new_gc_alloc_done){
            new_gc_alloc_done := False
            new_alloc_region := Mux(new_gc_alloc_done, new_alloc_region, io.ToNewGCAlloc.newAllocRegion)
            sub_state := overall_state.states(7)
          }
        }

        is(overall_state.states(7)){
          issueReq(io.Mreq, new_alloc_region, False, U(32), U(0), issued) { rd =>
            alloc_region_cache(destAttrIdx) := new_alloc_region
            alloc_bottom_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
            alloc_end_cache(destAttrIdx) := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
            alloc_top_cache(destAttrIdx) := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
            when(destAttrIdx === 1){
              next_offset_threshold := rd(GCElementWidth * 4 - 1 downto GCElementWidth * 3)
            }
            sub_state := overall_state.states(8)
          }
        }

        // 发起 parAllocate
        is(overall_state.states(8)){
          bot_updates := region_ptr_off20
          parAllocate.fire(desired_word_size_r, desired_word_size_r)
          sub_state := overall_state.states(9)
        }

        // 等待 parAllocate，并并行完成 region_ptr 写回
        is(overall_state.states(9)){
          when(!write_region_ptr_done) {
            val writeOff0 = new_alloc_region
            val writeOff10 = alloc_top_cache(destAttrIdx) - alloc_bottom_cache(destAttrIdx)
            val writeValue = Cat(writeOff10, region_ptr_off10, writeOff0).asUInt
            val addr = region_ptr_cache(destAttrIdx) + U"x8"
            issueReq(io.Mreq, addr, True, U(24), writeValue, issued) { _ =>
            }
            when(issued){
              issued := False
              write_region_ptr_done := True
            }
          }

          when(parAllocate.done){
            par_allocate_done := True
          }

          when((parAllocate.done || par_allocate_done) && write_region_ptr_done){
            when(destObjPtr =/= 0){
              actualPlabSize := desiredWordSize
            }
            finish()
          }
        }
      }
    }
  }

  switch(state){
    is(overall_state.states(0)){
      io.ToParAllocate.Ready := True
      when(io.ToParAllocate.Valid && io.ToParAllocate.Ready){
        nodeIndex := io.ToParAllocate.NodeIndex
        minWordSize := io.ToParAllocate.MinWordSize
        destAttrIdx := io.ToParAllocate.DestAttrIdx
        allocatorPtr := io.ToParAllocate.AllocatorPtr
        desiredWordSize := io.ToParAllocate.DesiredWordSize
        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      when(!region_ptr_valid(destAttrIdx)){
        when(destAttrIdx === 0){
          val addr = allocatorPtr + U"x28"
          issueReq(io.Mreq, addr, False, U(0), U(0), issued){ rd =>
            region_ptr_valid(destAttrIdx) := True
            region_ptr_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
            state := overall_state.states(2)
          }
        }.otherwise{
          region_ptr_cache(destAttrIdx) := allocatorPtr + U"x30"
          state := overall_state.states(2)
        }
      }.otherwise{
        state := overall_state.states(2)
      }
    }

    is(overall_state.states(2)){
      when(!alloc_region_valid(destAttrIdx)){
        val addr = region_ptr_cache(destAttrIdx) + U"x8"
        issueReq(io.Mreq, addr, False, U(0), U(0), issued) { rd =>
          alloc_region_valid(destAttrIdx) := True
          alloc_region_cache(destAttrIdx) := rd(GCElementWidth - 1 downto 0)
          region_ptr_off10 := rd(GCElementWidth * 2 - 1 downto GCElementWidth * 1)
          region_ptr_off18 := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          region_ptr_off20 := rd(GCElementWidth * 3)
          state := overall_state.states(3)
        }
      }.otherwise{
        state := overall_state.states(3)
      }
    }

    is(overall_state.states(3)){
      when(destAttrIdx === 0){
        parAllocateIml.fire(minWordSize, desiredWordSize)
      }.otherwise{
        bot_updates := True
        parAllocate.fire(minWordSize, desiredWordSize)
      }
      state := overall_state.states(4)
    }

    is(overall_state.states(4)){
      val useIml   = destAttrIdx === 0
      val workDone = Mux(useIml, parAllocateIml.done, parAllocate.done)

      when(workDone && destObjPtr === 0) {
        write_lock0 := False
        state := overall_state.states(5)
      }.elsewhen(workDone){
        resetState()
      }
    }

    is(overall_state.states(5)){
      attemptAlloc.fire(desiredWordSize)
      state := overall_state.states(6)
    }

    is(overall_state.states(6)){
      when(attemptAlloc.done){
        write_lock0 := True
        state := overall_state.states(7)
      }
    }

    is(overall_state.states(7)){
      val addr = io.ConfigIO.LockPtr
      val writeValue = Mux(write_lock0, U(0), io.ConfigIO.Thread)
      issueReq(io.Mreq, addr, True, U(8), writeValue, issued) { _ =>
      }
      when(issued){
        issued := False
        when(write_lock0) {
          resetState()
        }.otherwise{
          state := overall_state.states(5)
        }
      }
    }
  }
}

object GCParAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCParAllocate())
}