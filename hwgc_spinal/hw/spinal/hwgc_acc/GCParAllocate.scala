package hwgc_acc

import hwgc_top.{Config, GCAllocCacheUpdate, GCInterruptsIO, GCTopParameters, HWParameters, LocalMMUIO, MyStateMachine}
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCParAllocate extends Module with GCTopParameters with HWParameters {
  val io = new Bundle {
    val MreqMainIml = master(new LocalMMUIO)
    val MreqPar = master(new LocalMMUIO)
    val MreqAttempt = master(new LocalMMUIO)

    val ToParAllocate = slave(new GCToParAllocate)
    val ToNewGCAlloc = master(new GCToNewGCAlloc)

    val CacheUpdateOut = master(Stream(new GCAllocCacheUpdate))
    val CacheUpdateIn = slave(Flow(new GCAllocCacheUpdate))

    val Irq = master(new GCInterruptsIO)

    val ConfigIO = slave(new GCDoAllocateConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCParAllocate<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def clearMreq(mreq: LocalMMUIO): Unit = {
    mreq.Request.valid := False
    mreq.Request.payload.clearAll()
    mreq.Response.ready := True
  }

  clearMreq(io.MreqMainIml)
  clearMreq(io.MreqPar)
  clearMreq(io.MreqAttempt)

  io.ToNewGCAlloc.clearOut()
  io.ToParAllocate.clearIn()
  io.Irq.clearOut()

  val destAttrIdx = RegInit(U(0, 1 bits))
  val regionPtr = RegInit(U(0, GCElementWidth bits))
  val allocRegion = RegInit(U(0, GCElementWidth bits))
  val minWordSize = RegInit(U(0, GCElementWidth bits))
  val allocatorPtr = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val destObjPtr = RegInit(U(0, GCElementWidth bits))
  val actualWordSize = RegInit(U(0, GCElementWidth bits))

  val attemptNewRegionDone = RegInit(False)

  val updateValid = RegInit(False)
  val updateRegionPtr = RegInit(U(0, GCElementWidth bits))
  val updateRegion = RegInit(U(0, GCElementWidth bits))

  val region_ptr_valid = Vec(RegInit(False), 2)
  val region_ptr_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)
  val alloc_region_valid = Vec(RegInit(False), 2)
  val alloc_region_cache = Vec(RegInit(U(0, GCElementWidth bits)), 2)

  val new_gc_alloc_done_reg = RegInit(False)
  val new_alloc_region_reg = RegInit(U(0, GCElementWidth bits))

  when(io.ToNewGCAlloc.done.valid) {
    new_gc_alloc_done_reg := io.ToNewGCAlloc.done.valid
    new_alloc_region_reg := io.ToNewGCAlloc.done.payload.NewAllocRegion
  }

  io.CacheUpdateOut.valid := updateValid
  io.CacheUpdateOut.regionPtr := updateRegionPtr
  io.CacheUpdateOut.region := updateRegion

  when(io.CacheUpdateOut.fire){
    updateValid := False
  }

  def updateLocalAllocRegionCache(regionPtrValue: UInt, regionValue: UInt): Unit = {
    when(region_ptr_valid(0) && alloc_region_valid(0) && regionPtrValue === region_ptr_cache(0)) {
      alloc_region_cache(0) := regionValue
    }

    when(region_ptr_valid(1) && alloc_region_valid(1) && regionPtrValue === region_ptr_cache(1)) {
      alloc_region_cache(1) := regionValue
    }
  }

  def emitCacheUpdate(regionPtrValue: UInt, regionValue: UInt): Unit = {
    updateValid := True
    updateRegionPtr := regionPtrValue
    updateRegion := regionValue

    // 本 accelerator 自己也立即更新一次。这样就算顶层广播晚一拍回来，本地 cache 也已经是新的。
    updateLocalAllocRegionCache(regionPtrValue, regionValue)
  }

  when(io.CacheUpdateIn.valid) {
    updateLocalAllocRegionCache(io.CacheUpdateIn.payload.regionPtr, io.CacheUpdateIn.payload.region)
  }

  val parAllocateIml = new Area {
    val start = False
    val busy = RegInit(False)
    val done = RegInit(False)

    val alloc_region_r = Reg(UInt(GCElementWidth bits)) init 0
    val min_word_size_r = Reg(UInt(GCElementWidth bits)) init 0
    val desired_word_size_r = Reg(UInt(GCElementWidth bits)) init 0
    val want_to_allocate_r = Reg(UInt(GCElementWidth bits)) init 0

    val alloc_top = Reg(UInt(GCElementWidth bits)) init 0
    val alloc_end = Reg(UInt(GCElementWidth bits)) init 0

    done := False

    def fire(min_word_size: UInt, desired_word_size: UInt, alloc_region: UInt): Unit = {
      when(!busy) {
        start := True
        alloc_region_r := alloc_region
        min_word_size_r := min_word_size
        desired_word_size_r := desired_word_size
      }
    }

    val fsm = new MyStateMachine {
      val IDLE = new State with EntryPoint
      val READ_TOP_END = new State
      val WRITE_TOP = new State

      IDLE.whenIsActive {
        when(start && !busy) {
          busy := True
          goto(READ_TOP_END)
        }
      }

      READ_TOP_END.whenIsActive {
        issueReq(io.MreqMainIml, alloc_region_r + U"x8", False, U(16), U(0), True, False, issued) { rd =>
          val rd_alloc_end = rd(GCElementWidth - 1 downto 0)
          val rd_alloc_top = rd(GCElementWidth * 2 - 1 downto GCElementWidth)

          val available = ((rd_alloc_end - rd_alloc_top) >> 3).resize(GCElementWidth)
          val want_to_allocate = Mux(available > desired_word_size_r, desired_word_size_r, available)

          alloc_end := rd_alloc_end
          alloc_top := rd_alloc_top

          when(want_to_allocate >= min_word_size_r) {
            want_to_allocate_r := want_to_allocate
            goto(WRITE_TOP)
          }.otherwise {
            destObjPtr := U(0)
            actualWordSize := U(0)
            busy := False
            done := True
            goto(IDLE)
          }
        }
      }

      WRITE_TOP.whenIsActive {
        // @notice: atomic-cas
        val addr = alloc_region_r + U"x10"
        val desired = (alloc_top + (want_to_allocate_r << 3)).resize(GCElementWidth)
        val expected = alloc_top

        // 128bits 对齐的访问 64bits
        // val casData = Mux(
        //   addr(3),
        //   Cat(desired.resize(128) << 64, expected.resize(128) << 64),
        //   Cat(desired.resize(128), expected.resize(128))
        // ).asUInt.resize(MMUDataWidth)
        val casData = desired

        issueReq(io.MreqMainIml, addr, True, U(8), casData, True, True, issued) { rd =>
          // val observerd = rd(GCElementWidth - 1 downto 0)
          // when(observed === expected) {
          when(expected === expected) {
            destObjPtr := alloc_top
            actualWordSize := want_to_allocate_r
            busy := False
            done := True
            goto(IDLE)
          }.otherwise {
            goto(READ_TOP_END)
          }
        }
      }
    }
  }

  val parAllocate = new Area {
    val start = False
    val busy = RegInit(False)
    val done = RegInit(False)

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
    val iterator = RegInit(U(0, 4 bits))

    val bot_updates_r = Reg(Bool()) init False
    val alloc_region_r = Reg(UInt(GCElementWidth bits)) init 0
    val min_word_size_r = Reg(UInt(GCElementWidth bits)) init 0
    val desired_word_size_r = Reg(UInt(GCElementWidth bits)) init 0

    val cardBytesLeft = Reg(UInt(GCElementWidth bits)) init 0
    val cardTotalBytes = Reg(UInt(GCElementWidth bits)) init 0

    done := False

    def fire(min_word_size: UInt, desired_word_size: UInt, bot_updates: Bool, alloc_region: UInt): Unit = {
      when(!busy) {
        start := True
        bot_updates_r := bot_updates
        alloc_region_r := alloc_region
        min_word_size_r := min_word_size
        desired_word_size_r := desired_word_size
      }
    }

    val par_allocate_iml_done_reg = RegInit(False)

    when(parAllocateIml.done) {
      par_allocate_iml_done_reg := True
    }

    val fsm = new MyStateMachine {
      val IDLE = new State with EntryPoint
      val START_IML_AND_READ_REGION = new State
      val READ_BOT = new State
      val WAIT_IML_DONE = new State
      val WRITE_FIRST_CARD_AND_PREPARE_FILL = new State
      val FILL_CARDS = new State
      val FILL_CARDS_DATA = new State
      val UPDATE_BOT = new State

      IDLE.whenIsActive {
        when(start && !busy) {
          busy := True
          goto(START_IML_AND_READ_REGION)
        }
      }

      START_IML_AND_READ_REGION.whenIsActive {
        parAllocateIml.fire(min_word_size_r, desired_word_size_r, alloc_region_r)

        issueDirectRead(io.MreqPar, alloc_region_r + U"x20", U(24), READ_BOT) { rd =>
          next_offset_threshold := rd(GCElementWidth - 1 downto 0)
          index := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          bot_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        }
      }

      READ_BOT.whenIsActive {
        issueDirectRead(io.MreqPar, bot_ptr, U(24), WAIT_IML_DONE) { rd =>
          reserved_start := rd(GCElementWidth - 1 downto 0)
          array_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        }
      }

      WAIT_IML_DONE.whenIsActive {
        when(par_allocate_iml_done_reg) {
          par_allocate_iml_done_reg := False

          val blk_end_value = (destObjPtr + (actualWordSize << 3)).resize(GCElementWidth)
          blk_start := destObjPtr
          blk_end := blk_end_value

          when(destObjPtr =/= 0 && bot_updates_r && blk_end_value > next_offset_threshold) {
            goto(WRITE_FIRST_CARD_AND_PREPARE_FILL)
          }.otherwise {
            busy := False
            done := True
            goto(IDLE)
          }
        }
      }

      WRITE_FIRST_CARD_AND_PREPARE_FILL.whenIsActive {
        val addr = array_ptr + index
        val writeValue = ((next_offset_threshold - blk_start) >> 3).resize(8)

        issueReq(io.MreqPar, addr, True, U(1), writeValue, False, False, issued) { _ => }

        when(issued) {
          issued := False

          val end_index_value = ((blk_end - 8 - reserved_start) >> 9).resize(GCElementWidth)
          val rem_st = (reserved_start + ((index + 1) << 6) << 3).resize(GCElementWidth)
          val rem_end = (reserved_start + ((end_index_value << 6) + 64) << 3).resize(GCElementWidth)
          val start_card = ((((index + 1) << 6) << 3) >> 9).resize(GCElementWidth)
          val end_card = ((((end_index_value << 6) + 63) << 3) >> 9).resize(GCElementWidth)

          end_index := end_index_value

          when(index + 1 <= end_index_value && rem_st < rem_end && start_card <= end_card) {
            remaining := end_card - start_card + 1
            begin := array_ptr + start_card
            iterator := 0
            goto(FILL_CARDS)
          }.otherwise {
            goto(UPDATE_BOT)
          }
        }
      }

      FILL_CARDS.whenIsActive {
        when(iterator < 14 && remaining > 0) {
          val chunk = (U(15) << (iterator << 2)).resize(GCElementWidth)
          val nbytes = Mux(remaining < chunk, remaining, chunk)

          cardBytesLeft := nbytes
          cardTotalBytes := nbytes

          goto(FILL_CARDS_DATA)
        }.otherwise {
          goto(UPDATE_BOT)
        }
      }

      FILL_CARDS_DATA.whenIsActive {
        val lanes = LineBytesNum

        val beatBytes = Mux(
          cardBytesLeft < U(lanes, GCElementWidth bits),
          cardBytesLeft,
          U(lanes, GCElementWidth bits)
        )

        val fillByte = (U(64, 8 bits) + iterator.resize(8)).asBits
        val writeBytes = Vec(Bits(8 bits), lanes)

        for (b <- 0 until lanes) {
          writeBytes(b) := Mux(U(b, GCElementWidth bits) < beatBytes, fillByte, B(0, 8 bits))
        }

        val writeValue = writeBytes.asBits.asUInt

        issueReq(io.MreqPar, begin, True, beatBytes, writeValue, False, False, issued) { _ => }

        when(issued) {
          issued := False
          begin := begin + beatBytes

          when(cardBytesLeft === beatBytes) {
            cardBytesLeft := 0
            remaining := remaining - cardTotalBytes
            iterator := iterator + 1
            goto(FILL_CARDS)
          }.otherwise {
            cardBytesLeft := cardBytesLeft - beatBytes
            goto(FILL_CARDS_DATA)
          }
        }
      }

      UPDATE_BOT.whenIsActive {
        val write_index = end_index + 1
        val write_threshold = (reserved_start + ((end_index << 6) + 64) << 3).resize(GCElementWidth)
        val writeData = Cat(write_index, write_threshold).asUInt

        issueReq(io.MreqPar, alloc_region_r + U"x20", True, U(16), writeData, False, False, issued) { _ => }

        when(issued) {
          issued := False
          busy := False
          done := True
          goto(IDLE)
        }
      }
    }
  }

  val attemptAlloc = new Area {
    val start = False

    val busy = RegInit(False)
    val done = RegInit(False)

    val region_ptr_r = RegInit(U(0, GCElementWidth bits))
    val alloc_region_r = RegInit(U(0, GCElementWidth bits))
    val desired_word_size_r = RegInit(U(0, GCElementWidth bits))

    val alloc_top = RegInit(U(0, GCElementWidth bits))
    val alloc_bottom = RegInit(U(0, GCElementWidth bits))

    val region_ptr_type = RegInit(U(0, 8 bits))
    val region_ptr_off10 = RegInit(U(0, GCElementWidth bits))
    val allocated_bytes = RegInit(U(0, GCElementWidth bits))

    val new_alloc_region_r = RegInit(U(0, GCElementWidth bits))

    val bot_updates_r = RegInit(False)

    val old_set_cnt_r = RegInit(U(0, 32 bits))
    val survivor_bytes_r = RegInit(U(0, GCElementWidth bits))

    val cm_r = Reg(UInt(GCElementWidth bits)) init 0
    val cm_valid = RegInit(False)
    val during_im_r = RegInit(False)
    val during_im_valid = RegInit(False)
    val root_start_r = Reg(UInt(GCElementWidth bits)) init 0
    val root_regions_array_r = Reg(UInt(GCElementWidth bits)) init 0
    val root_regions_idx_r = Reg(UInt(GCElementWidth bits)) init 0

    done := False

    def fire(region_ptr: UInt, alloc_region: UInt, desired_word_size: UInt): Unit = {
      when(!busy) {
        start := True
        region_ptr_r := region_ptr
        alloc_region_r := alloc_region
        desired_word_size_r := desired_word_size
      }
    }

    val par_allocate_done_reg = RegInit(False)

    when(parAllocate.done) {
      par_allocate_done_reg := True
    }

    val fsm = new MyStateMachine {
      val IDLE = new State with EntryPoint
      val READ_REGION_TYPE = new State
      val NEW_ALLOC_REQ = new State
      val READ_ALLOC_REGION = new State
      val READ_REGION_INFO = new State
      val READ_OLD_SET = new State
      val WRITE_OLD_SET = new State
      val READ_SURVIVOR = new State
      val WRITE_SURVIVOR = new State
      val READ_DURING_IM = new State
      val READ_CM = new State
      val READ_ROOT_START = new State
      val READ_ROOT_REGIONS = new State
      val WRITE_ROOT_REGION = new State
      val WRITE_ROOT_INDEX = new State
      val WRITE_REGION_TO_DUMMY = new State
      val WAIT_NEW_ALLOC_REGION = new State
      val READ_BOT_UPDATES = new State
      val START_PAR_AND_CLEAR_NEW_REGION = new State
      val READ_NEW_ALLOC_REGION = new State
      val WRITE_REGION_TO_NEW = new State
      val WAIT_PAR_DONE = new State

      IDLE.whenIsActive {
        when(start && !busy) {
          busy := True
          goto(READ_REGION_TYPE)
        }
      }

      READ_REGION_TYPE.whenIsActive {
        issueDirectRead(io.MreqAttempt, region_ptr_r + U"x40", U(1), NEW_ALLOC_REQ) { rd =>
          region_ptr_type := rd(7 downto 0)
        }
      }

      NEW_ALLOC_REQ.whenIsActive {
        io.ToNewGCAlloc.cmd.valid := True
        io.ToNewGCAlloc.cmd.payload.RegionPtr := region_ptr_r
        io.ToNewGCAlloc.cmd.payload.RegionType := region_ptr_type

        when(io.ToNewGCAlloc.cmd.fire) {
          goto(READ_ALLOC_REGION)
        }
      }

      READ_ALLOC_REGION.whenIsActive {
        when(alloc_region_r =/= io.ConfigIO.DummyRegion) {
          issueDirectRead(io.MreqAttempt, alloc_region_r, U(24), READ_REGION_INFO) { rd =>
            alloc_bottom := rd(GCElementWidth - 1 downto 0)
            alloc_top := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          }
        }.otherwise {
          goto(WAIT_NEW_ALLOC_REGION)
        }
      }

      READ_REGION_INFO.whenIsActive {
        issueReq(io.MreqAttempt, region_ptr_r + U"x10", False, U(24), U(0), True, False, issued) { rd =>
          region_ptr_off10 := rd(GCElementWidth - 1 downto 0)
          bot_updates_r := rd(GCElementWidth * 2)
          allocated_bytes := alloc_top - alloc_bottom - rd(GCElementWidth * 2 - 1 downto GCElementWidth)

          when(region_ptr_type === U(1)){
            goto(READ_OLD_SET)
          }.otherwise{
            goto(READ_SURVIVOR)
          }
        }
      }

      READ_OLD_SET.whenIsActive {
        val addr = io.ConfigIO.G1h + U"xb0"
        issueDirectRead(io.MreqAttempt, addr, U(4), WRITE_OLD_SET) { rd =>
          old_set_cnt_r := rd(31 downto 0)
        }
      }

      WRITE_OLD_SET.whenIsActive {
        val addr = io.ConfigIO.G1h + U"xb0"
        issueDirectWriteWithoutResp(io.MreqAttempt, addr, U(4), old_set_cnt_r + U(1), READ_DURING_IM)()
      }

      READ_SURVIVOR.whenIsActive {
        val addr = io.ConfigIO.G1h + U"x408"
        issueDirectRead(io.MreqAttempt, addr, U(8), WRITE_SURVIVOR) { rd =>
          survivor_bytes_r := rd(GCElementWidth - 1 downto 0)
        }
      }

      WRITE_SURVIVOR.whenIsActive {
        val addr = io.ConfigIO.G1h + U"x408"
        issueDirectWriteWithoutResp(io.MreqAttempt, addr, U(8), survivor_bytes_r + allocated_bytes, READ_DURING_IM)()
      }

      READ_DURING_IM.whenIsActive {
        when(!during_im_valid) {
          issueReq(io.MreqAttempt, io.ConfigIO.G1h + U"x3c1", False, U(1), U(0), True, False, issued) { rd =>
            during_im_r := rd(0)
            during_im_valid := True

            when(rd(0) && allocated_bytes > 0) {
              goto(READ_CM)
            }.otherwise {
              goto(WRITE_REGION_TO_DUMMY)
            }
          }
        }.otherwise {
          when(during_im_r && allocated_bytes > 0) {
            goto(READ_CM)
          }.otherwise {
            goto(WRITE_REGION_TO_DUMMY)
          }
        }
      }

      READ_CM.whenIsActive {
        when(!cm_valid) {
          issueDirectRead(io.MreqAttempt, io.ConfigIO.G1h + U"x4e8",  U(8), READ_ROOT_START) { rd =>
            cm_r := rd(GCElementWidth - 1 downto 0)
            cm_valid := True
          }
        }.otherwise {
          goto(READ_ROOT_START)
        }
      }

      READ_ROOT_START.whenIsActive {
        issueDirectRead(io.MreqAttempt, alloc_region_r + U"xe8", U(8), READ_ROOT_REGIONS) { rd =>
          root_start_r := rd(GCElementWidth - 1 downto 0)
        }
      }

      READ_ROOT_REGIONS.whenIsActive {
        val root_regions_ptr = cm_r + U"xb0"

        issueDirectRead(io.MreqAttempt, root_regions_ptr, U(24), WRITE_ROOT_REGION) { rd =>
          root_regions_array_r := rd(GCElementWidth - 1 downto 0)
          root_regions_idx_r := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        }
      }

      WRITE_ROOT_REGION.whenIsActive {
        val mem_region = (root_regions_array_r + (root_regions_idx_r << 4)).resize(MMUAddrWidth)
        val word_len = (alloc_top - root_start_r) >> 3
        val write_data = Cat(word_len.asBits, root_start_r.asBits).asUInt

        issueDirectWriteWithoutResp(io.MreqAttempt, mem_region, U(16), write_data, WRITE_ROOT_INDEX) ()
      }

      WRITE_ROOT_INDEX.whenIsActive {
        val addr = cm_r + U"xc0"
        issueDirectWriteWithoutResp(io.MreqAttempt, addr, U(8), root_regions_idx_r + 1, WRITE_REGION_TO_DUMMY)()
      }

      WRITE_REGION_TO_DUMMY.whenIsActive {
        val writeData = Cat(U(0), region_ptr_off10, io.ConfigIO.DummyRegion).asUInt

        issueDirectWriteWithoutResp(io.MreqAttempt, region_ptr_r + U"x8", U(24), writeData, WAIT_NEW_ALLOC_REGION) {
          emitCacheUpdate(region_ptr_r, io.ConfigIO.DummyRegion)
        }
      }

      WAIT_NEW_ALLOC_REGION.whenIsActive {
        when(new_gc_alloc_done_reg) {
          new_gc_alloc_done_reg := False
          new_alloc_region_r := new_alloc_region_reg

          when(new_alloc_region_reg =/= 0) {
            goto(READ_BOT_UPDATES)
          }.otherwise {
            destObjPtr := 0
            actualWordSize := 0
            busy := False
            done := True
            par_allocate_done_reg := False
            goto(IDLE)
          }
        }
      }

      READ_BOT_UPDATES.whenIsActive {
        when(alloc_region_r === io.ConfigIO.DummyRegion) {
          goto(START_PAR_AND_CLEAR_NEW_REGION)
        }.otherwise {
          issueDirectRead(io.MreqAttempt, region_ptr_r + U"x10", U(24), START_PAR_AND_CLEAR_NEW_REGION) { rd =>
            region_ptr_off10 := rd(GCElementWidth - 1 downto 0)
            bot_updates_r := rd(GCElementWidth * 3)
            goto(START_PAR_AND_CLEAR_NEW_REGION)
          }
        }
      }

      START_PAR_AND_CLEAR_NEW_REGION.whenIsActive {
        parAllocate.fire(desired_word_size_r, desired_word_size_r, bot_updates_r, new_alloc_region_r)
        issueDirectWriteWithoutResp(io.MreqAttempt, new_alloc_region_r + U"xa8", U(8), U(0), READ_NEW_ALLOC_REGION)()
      }

      READ_NEW_ALLOC_REGION.whenIsActive {
        issueDirectRead(io.MreqAttempt, new_alloc_region_r, U(24), WRITE_REGION_TO_NEW) { rd =>
          alloc_bottom := rd(GCElementWidth - 1 downto 0)
          alloc_top := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
        }
      }

      WRITE_REGION_TO_NEW.whenIsActive {
        val writeOff18 = alloc_top - alloc_bottom
        val writeOff10 = region_ptr_off10 + 1
        val writeOff8 = new_alloc_region_r
        val writeData = Cat(writeOff18, writeOff10, writeOff8).asUInt

        issueDirectWriteWithoutResp(io.MreqAttempt, region_ptr_r + U"x8", U(24), writeData, WAIT_PAR_DONE){
          emitCacheUpdate(region_ptr_r, new_alloc_region_r)
        }
      }

      WAIT_PAR_DONE.whenIsActive {
        when(par_allocate_done_reg) {
          par_allocate_done_reg := False

          when(destObjPtr =/= 0) {
            actualWordSize := desired_word_size_r
          }

          busy := False
          done := True
          goto(IDLE)
        }
      }
    }
  }

  val selectAllocate = RegInit(False)

  val lockPtr = RegInit(U(0, GCElementWidth bits))
  val wakeUpSel = RegInit(U(0, 2 bits))
  val mainFsm = new MyStateMachine {
    val IDLE = new State with EntryPoint
    val LOAD_REGION_PTR = new State
    val LOAD_ALLOC_REGION = new State
    val FIRST_ALLOC = new State
    val WAIT_IML_DONE = new State
    val START_PAR_ALLOC = new State
    val WAIT_PAR_ALLOC = new State
    val UNLOCK_ALLOC_REGION = new State
    val WAKE_LOCK_PTR_IRQ = new State
    val WAKE_IRQ_DONE = new State
    val CHECK_ALLOC_RESULT = new State
    val LOCK_FREELIST = new State
    val WRITE_FREELIST_OWNER = new State
    val START_ATTEMPT = new State
    val WAIT_ATTEMPT = new State
    val MARK_ALLOCATOR_FULL = new State
    val UNLOCK_FREELIST = new State

    def U32(v: Int): UInt = U(v, 32 bits)

    def allocRegionLockAddr: UInt = allocRegion + U"x48"
    def freeListLockAddr: UInt = io.ConfigIO.LockPtr + U"x8"
    def allocatorFullFlagAddr: UInt = allocatorPtr + Mux(destAttrIdx === U(0), U"x10", U"x11")
    def allocatorFullFlagBit(rd: UInt): Bool = Mux(destAttrIdx === U(0), rd(0), rd(1))
    def resetState(): Unit = {
      io.ToParAllocate.done.valid := True
      io.ToParAllocate.done.payload.DestObjPtr := destObjPtr
      io.ToParAllocate.done.payload.ActualPlabSize := actualWordSize
      goto(IDLE)
    }
    def casData32(addr: UInt, expected: UInt, desired: UInt): UInt = {
      val laneShift = addr(3 downto 2) << 5
      val desiredAligned = (desired.resize(128) << laneShift).resize(128)
      val expectedAligned = (expected.resize(128) << laneShift).resize(128)
      Cat(desiredAligned, expectedAligned).asUInt.resize(MMUDataWidth)
    }
    def tryLock32(addr: UInt, successState: State) : Unit = {
      val casData = casData32(addr, U32(0), U32(1))
      issueReq(io.MreqMainIml, addr, True, U(4), casData, True, True, issued) { rd =>
        when(rd(31 downto 0) === U32(0)) {
          goto(successState)
        }
      }
    }
    def wakeLockPtr(ptr: UInt, sel: UInt): Unit = {
      lockPtr := ptr
      wakeUpSel := sel
      goto(WAKE_LOCK_PTR_IRQ)
    }
    def tryUnlock32(addr: UInt, successState: State, wakeSel: UInt): Unit = {
      val casData = casData32(addr, U32(1), U32(0))

      issueReq(io.MreqMainIml, addr, True, U(4), casData, True, True, issued) { rd =>
        when(rd(31 downto 0) === U32(1)) {
          when(wakeSel === U(1)){
            resetState()
          }.otherwise{
            goto(successState)
          }
        }.otherwise{
          wakeLockPtr(addr - U"x8", wakeSel)
        }
      }
    }

    IDLE.whenIsActive {
      io.ToParAllocate.cmd.ready := True

      when(io.ToParAllocate.cmd.fire) {
        destAttrIdx := io.ToParAllocate.cmd.payload.DestAttrIdx
        minWordSize := io.ToParAllocate.cmd.payload.MinWordSize
        allocatorPtr := io.ToParAllocate.cmd.payload.AllocatorPtr
        desiredWordSize := io.ToParAllocate.cmd.payload.DesiredWordSize

        regionPtr := 0
        allocRegion := 0
        destObjPtr := 0
        actualWordSize := 0
        attemptNewRegionDone := False
        selectAllocate := False
        lockPtr := U(0)
        wakeUpSel := U(0)

        goto(LOAD_REGION_PTR)
      }
    }

    LOAD_REGION_PTR.whenIsActive {
      when(!region_ptr_valid(destAttrIdx)) {
        when(destAttrIdx === U(0)) {
          val addr = allocatorPtr + U"x28"

          issueDirectRead(io.MreqMainIml, addr, U(8), LOAD_ALLOC_REGION) { rd =>
            val rp = rd(GCElementWidth - 1 downto 0)

            region_ptr_valid(destAttrIdx) := True
            region_ptr_cache(destAttrIdx) := rp
            regionPtr := rp
          }
        }.otherwise {
          val rp = allocatorPtr + U"x30"

          region_ptr_valid(destAttrIdx) := True
          region_ptr_cache(destAttrIdx) := rp
          regionPtr := rp

          goto(LOAD_ALLOC_REGION)
        }
      }.otherwise {
        regionPtr := region_ptr_cache(destAttrIdx)
        goto(LOAD_ALLOC_REGION)
      }
    }

    LOAD_ALLOC_REGION.whenIsActive {
      when(!alloc_region_valid(destAttrIdx)) {
        val addr = region_ptr_cache(destAttrIdx) + U"x8"

        issueDirectRead(io.MreqMainIml, addr, U(8), FIRST_ALLOC) { rd =>
          val ar = rd(GCElementWidth - 1 downto 0)

          alloc_region_valid(destAttrIdx) := True
          alloc_region_cache(destAttrIdx) := ar
          allocRegion := ar
        }
      }.otherwise {
        allocRegion := alloc_region_cache(destAttrIdx)
        goto(FIRST_ALLOC)
      }
    }

    FIRST_ALLOC.whenIsActive {
      when(destAttrIdx === U(0)) {
        parAllocateIml.fire(minWordSize, desiredWordSize, allocRegion)
        goto(WAIT_IML_DONE)
      }.otherwise {
        // tryLock32(allocRegionLockAddr, START_PAR_ALLOC)
        issueReq(io.MreqMainIml, allocRegionLockAddr, True, U(4), U(1), True, True, issued) { _ =>
          goto(START_PAR_ALLOC)
        }
      }
    }

    WAIT_IML_DONE.whenIsActive {
      when(parAllocateIml.done) {
        goto(CHECK_ALLOC_RESULT)
      }
    }

    START_PAR_ALLOC.whenIsActive {
      parAllocate.fire(minWordSize, desiredWordSize, True, allocRegion)
      goto(WAIT_PAR_ALLOC)
    }

    WAIT_PAR_ALLOC.whenIsActive {
      when(parAllocate.done) {
        goto(UNLOCK_ALLOC_REGION)
      }
    }

    UNLOCK_ALLOC_REGION.whenIsActive {
      // tryUnlock32(allocRegionLockAddr, CHECK_ALLOC_RESULT, U(0))
      issueDirectWriteWithoutResp(io.MreqMainIml, allocRegionLockAddr, U(4), U(0), CHECK_ALLOC_RESULT) {}
    }

    WAKE_LOCK_PTR_IRQ.whenIsActive {
      io.Irq.req.valid := True
      io.Irq.req.payload.par0 := lockPtr
      io.Irq.req.payload.cmd := IRQ_WAKE
      when(io.Irq.req.fire){
        goto(WAKE_IRQ_DONE)
      }
    }

    WAKE_IRQ_DONE.whenIsActive {
      when(io.Irq.resp.valid){
        when(wakeUpSel === U(0)){
          goto(CHECK_ALLOC_RESULT)
        }.otherwise{
          resetState()
        }
      }
    }

    CHECK_ALLOC_RESULT.whenIsActive {
      when(destObjPtr === 0) {
        when(selectAllocate) {
          selectAllocate := False
          goto(LOCK_FREELIST)
        }.otherwise {
          issueReq(io.MreqMainIml, allocatorPtr + U"x10", False, U(1), U(0), True, False, issued) { rd =>
            val isFull = allocatorFullFlagBit(rd)

            when(!isFull) {
              selectAllocate := True
              goto(FIRST_ALLOC)
            }.otherwise {
              resetState()
            }
          }
        }
      }.otherwise {
        resetState()
      }
    }

    LOCK_FREELIST.whenIsActive {
      // tryLock32(freeListLockAddr, WRITE_FREELIST_OWNER)
      issueReq(io.MreqMainIml, freeListLockAddr, True, U(4), U(1), True, True, issued) { rd =>
        goto(WRITE_FREELIST_OWNER)
      }
    }

    WRITE_FREELIST_OWNER.whenIsActive {
      issueDirectWriteWithoutResp(io.MreqMainIml, io.ConfigIO.G1h, U(8), io.ConfigIO.Thread, START_ATTEMPT)()
    }

    START_ATTEMPT.whenIsActive {
      attemptAlloc.fire(regionPtr, allocRegion, desiredWordSize)
      goto(WAIT_ATTEMPT)
    }

    WAIT_ATTEMPT.whenIsActive {
      when(attemptAlloc.done) {
        attemptNewRegionDone := True

        when(destObjPtr === 0) {
          goto(MARK_ALLOCATOR_FULL)
        }.otherwise {
          goto(UNLOCK_FREELIST)
        }
      }
    }

    MARK_ALLOCATOR_FULL.whenIsActive {
      issueDirectWriteWithoutResp(io.MreqMainIml, allocatorFullFlagAddr, U(1), U(1), UNLOCK_FREELIST)()
    }

    UNLOCK_FREELIST.whenIsActive {
      // tryUnlock32(freeListLockAddr, IDLE, U(1))
      issueReq(io.MreqMainIml, freeListLockAddr, True, U(4), U(0, 32 bits), False, False, issued) { _ => }
      when(issued) {
        issued := False
        resetState()
      }
    }
  }
}

object GCParAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCParAllocate())
}