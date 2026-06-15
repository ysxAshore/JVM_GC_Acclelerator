package hwgc_acc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCDoAllocate extends Module with GCParameters with HWParameters {
  val io = new Bundle {
    val Mreq = master(new LocalMMUIO)
    val ToDoAllocate = slave(new GCToDoAllocate)
    val ToNewGCAlloc = master(new GCToNewGCAlloc)
    val ConfigIO = slave(new GCParAllocateConfigIO)
  }

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ToNewGCAlloc.clearIn()
  io.ToDoAllocate.clearOut()


  object overall_state extends SpinalEnum {
    val states = Array.tabulate(10)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val nodeIndex =  RegInit(U(0, 8 bits))
  val destAttrIdx = RegInit(U(0, 1 bits))
  val regionPtr = RegInit(U(0, GCElementWidth bits))
  val allocRegion = RegInit(U(0, GCElementWidth bits))
  val minWordSize = RegInit(U(0, GCElementWidth bits))
  val allocatorPtr = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val actualPlabSize = RegInit(U(0, GCElementWidth bits))
  val destObjPtr = RegInit(U(0, GCElementWidth bits))

  val parAllocateIml = new Area {
    val sub_state = RegInit(overall_state.states(0))

    val start = False
    val busy  = RegInit(False)
    val done  = RegInit(False)

    val alloc_region_r      = Reg(UInt(GCElementWidth bits)) init 0
    val min_word_size_r     = Reg(UInt(GCElementWidth bits)) init 0
    val desired_word_size_r = Reg(UInt(GCElementWidth bits)) init 0
    val want_to_allocate_r = Reg(UInt(GCElementWidth bits)) init 0

    val alloc_top = Reg(UInt(GCElementWidth bits)) init 0
    val alloc_end = Reg(UInt(GCElementWidth bits)) init 0

    done := False

    def fire(min_word_size: UInt, desired_word_size: UInt, alloc_region: UInt): Unit = {
      when(!busy) {
        start := True
        alloc_region_r      := alloc_region
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
          issueReq(io.Mreq, alloc_region_r + U"x8", False, U(16), U(0), issued) { rd =>
            alloc_end := rd(GCElementWidth - 1 downto 0)
            alloc_top := rd(GCElementWidth * 2 - 1 downto 0)
            state := overall_state.states(1)
          }
        }

        is(overall_state.states(1)){
          val available = ((alloc_end - alloc_top) >> 3).resize(GCElementWidth)
          val want_to_allocate = Mux(available > desired_word_size_r, desired_word_size_r, available)
          when(want_to_allocate >= min_word_size_r){
            want_to_allocate_r := want_to_allocate
            state := overall_state.states(2)
          }.otherwise{
            destObjPtr := U(0)
            finish()
          }
        }

        is(overall_state.states(2)){
          val new_top = (alloc_top + want_to_allocate_r << 3).resize(GCElementWidth)
          // @todo atomic cmpxchg
          issueReq(io.Mreq, alloc_region_r + U"x10", True, U(8), new_top, issued) { _ => }
          when(issued){
            issued := False
            when(alloc_top === alloc_top){
              destObjPtr := alloc_top
              actualPlabSize := want_to_allocate_r
              finish()
            }.otherwise{
              state := overall_state.states(0)
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

    val blk_start = RegInit(U(0, GCElementWidth bits))
    val blk_end = RegInit(U(0, GCElementWidth bits))
    val next_offset_threshold = RegInit(U(0, GCElementWidth bits))
    val index = RegInit(U(0, GCElementWidth bits))
    val bot_ptr = RegInit(U(0, GCElementWidth bits))
    val reserved_start = RegInit(U(0, GCElementWidth bits))
    val array_ptr = RegInit(U(0, GCElementWidth bits))
    val end_index = RegInit(U(0, GCElementWidth bits))
    val begin = RegInit(U(0, GCElementWidth bits))
    val remaining = RegInit(U(0, GCElementWidth bits))
    val iterator = RegInit(U(0, 32 bits))

    val bot_updates_r       = Reg(Bool()) init False
    val alloc_region_r      = Reg(UInt(GCElementWidth bits)) init 0
    val min_word_size_r     = Reg(UInt(GCElementWidth bits)) init 0
    val desired_word_size_r = Reg(UInt(GCElementWidth bits)) init 0

    done := False

    def fire(min_word_size: UInt, desired_word_size: UInt, bot_updates: Bool, alloc_region: UInt): Unit = {
      when(!busy) {
        start := True
        bot_updates_r       := bot_updates
        alloc_region_r      := alloc_region
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
          parAllocateIml.fire(min_word_size_r, desired_word_size_r, alloc_region_r)
          sub_state := overall_state.states(1)
        }

        // 等待 parAllocateIml 完成
        is(overall_state.states(1)){
          when(parAllocateIml.done){
            when(destObjPtr =/= 0 && bot_updates_r){
              sub_state := overall_state.states(2)
            }.otherwise{
              finish()
            }
          }
        }

        is(overall_state.states(2)){
          blk_start := destObjPtr
          blk_end := (destObjPtr + (actualPlabSize << 3)).resize(GCElementWidth)

          issueReq(io.Mreq, alloc_region_r + U"x20", False, U(24), U(0), issued) { rd =>
            next_offset_threshold := rd(GCElementWidth - 1 downto 0)
            index := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
            bot_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)

            state := overall_state.states(3)
          }
        }

        is(overall_state.states(3)){
          when(blk_end <= next_offset_threshold){
            finish()
          }.otherwise{
            issueReq(io.Mreq, bot_ptr, False, U(24), U(0), issued) { rd =>
              reserved_start := rd(GCElementWidth - 1 downto 0)
              array_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
              state := overall_state.states(4)
            }
          }
        }

        is(overall_state.states(4)){
          val writeValue = ((next_offset_threshold - blk_start) >> 3).resize(8)
          issueReq(io.Mreq, array_ptr, True, U(1), writeValue, issued) { _ => }
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
            begin := array_ptr + start_card
            iterator := 0
            sub_state := overall_state.states(7)
          }.otherwise{
            sub_state := overall_state.states(8)
          }
        }

        is(overall_state.states(7)){
          // @todo 对于 nbytes > 32 的怎么做
          when(iterator < 14 && remaining > 0) {
            val chunk = (U(15) << (iterator << 2)).resize(GCElementWidth)
            val nbytes = Mux(remaining < chunk, remaining, chunk)

            val fillByte = (U(64, 8 bits) + iterator.resize(8)).asBits
            val lanes    = MMUDataWidth / 8
            val writeBytes = Vec(Bits(8 bits), lanes)

            for (b <- 0 until lanes) {
              writeBytes(b) := Mux(U(b) < nbytes, fillByte, B(0, 8 bits))
            }

            val writeValue = writeBytes.asBits.asUInt
            issueReq(io.Mreq, begin, True, nbytes, writeValue, issued) { _ => }
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
          val write_index = end_index + 1
          val write_threshold = (reserved_start + ((end_index << 6) + 64) << 3).resize(GCElementWidth)
          val writeData = Cat(write_index, write_threshold).asUInt
          issueReq(io.Mreq, alloc_region_r + U"x20", True, U(16), writeData, issued) { _ => }
          when(issued) {
            issued := False
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
          io.ToNewGCAlloc.regionType := U(1)
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
      io.ToDoAllocate.Ready := True
      when(io.ToDoAllocate.Valid && io.ToDoAllocate.Ready){
        nodeIndex := io.ToDoAllocate.NodeIndex
        destAttrIdx := io.ToDoAllocate.DestAttrIdx
        regionPtr := io.ToDoAllocate.regionPtr
        allocRegion := io.ToDoAllocate.allocRegion
        desiredWordSize := io.ToDoAllocate.DesiredWordSize
        minWordSize := io.ToDoAllocate.MinWordSize
        allocatorPtr := io.ToDoAllocate.AllocatorPtr

        state := overall_state.states(1)
      }
    }

    is(overall_state.states(1)){
      when(destAttrIdx === U(0)){
        // run par_allocate_iml
      }.otherwise{
        //atomic cmpxchg
      }
    }

    is(overall_state.states(2)){
      // run par_allocate
    }

    is(overall_state.states(3)){
      // get lockptr+ 8
    }

    is(overall_state.states(4)){

    }
  }


}
