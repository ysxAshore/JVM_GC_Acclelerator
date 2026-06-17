package hwgc_allocate

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

class GCDoAllocate extends Module with GCTopParameters with HWParameters {
  val io = new Bundle {
    val MreqMainIml = master(new LocalMMUIO)
    val MreqPar = master(new LocalMMUIO)
    val MreqAttempt = master(new LocalMMUIO)

    val ToDoAllocate = slave(new GCToDoAllocate)
    val ToNewGCAlloc = master(new GCToNewGCAlloc)
    val ConfigIO = slave(new GCDoAllocateConfigIO)
  }

  def clearMreq(mreq: LocalMMUIO): Unit = {
    mreq.Request.valid := False
    mreq.Request.payload.clearAll()
    mreq.RequestSize.valid := False
    mreq.RequestSize.payload.clearAll()
    mreq.Response.ready := True
  }

  clearMreq(io.MreqMainIml)
  clearMreq(io.MreqPar)
  clearMreq(io.MreqAttempt)

  io.ToNewGCAlloc.clearIn()
  io.ToDoAllocate.clearOut()

  val issuedMainIml = RegInit(False)
  val issuedPar = RegInit(False)
  val issuedAttempt = RegInit(False)

  val nodeIndex = RegInit(U(0, 8 bits))
  val destAttrIdx = RegInit(U(0, 1 bits))
  val regionPtr = RegInit(U(0, GCElementWidth bits))
  val allocRegion = RegInit(U(0, GCElementWidth bits))
  val minWordSize = RegInit(U(0, GCElementWidth bits))
  val allocatorPtr = RegInit(U(0, GCElementWidth bits))
  val desiredWordSize = RegInit(U(0, GCElementWidth bits))

  val destObjPtr = RegInit(U(0, GCElementWidth bits))
  val actualWordSize = RegInit(U(0, GCElementWidth bits))

  val allocRegionLockPtr = RegInit(U(0, GCElementWidth bits))
  val freelistLockPtr = RegInit(U(0, GCElementWidth bits))

  val lockValue = RegInit(U(0, 32 bits))

  val firstTryDone = RegInit(False)
  val retryTryDone = RegInit(False)
  val attemptNewRegionDone = RegInit(False)

  val updateValid = RegInit(False)
  val updateRegionPtr = RegInit(U(0, GCElementWidth bits))
  val updateRegion = RegInit(U(0, GCElementWidth bits))

  val new_gc_alloc_done_reg = RegInit(False)
  val new_alloc_region_reg = RegInit(U(0, GCElementWidth bits))
  when(io.ToNewGCAlloc.Done) {
    new_gc_alloc_done_reg := io.ToNewGCAlloc.Done
    new_alloc_region_reg := io.ToNewGCAlloc.newAllocRegion
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

    def fire(
        min_word_size: UInt,
        desired_word_size: UInt,
        alloc_region: UInt
    ): Unit = {
      when(!busy) {
        start := True
        alloc_region_r := alloc_region
        min_word_size_r := min_word_size
        desired_word_size_r := desired_word_size
      }
    }

    val fsm = new StateMachine {
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
        issueReq(
          io.MreqMainIml,
          alloc_region_r + U"x8",
          False,
          U(16),
          U(0),
          issuedMainIml
        ) { rd =>
          val rd_alloc_end = rd(GCElementWidth - 1 downto 0)
          val rd_alloc_top = rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          val available =
            ((rd_alloc_end - rd_alloc_top) >> 3).resize(GCElementWidth)
          val want_to_allocate =
            Mux(available > desired_word_size_r, desired_word_size_r, available)

          alloc_end := rd_alloc_end
          alloc_top := rd_alloc_top

          when(want_to_allocate >= min_word_size_r) {
            want_to_allocate_r := want_to_allocate
            goto(WRITE_TOP)
          }.otherwise {
            destObjPtr := U(0)
            busy := False
            done := True
            goto(IDLE)
          }
        }
      }

      WRITE_TOP.whenIsActive {
        val new_top =
          (alloc_top + (want_to_allocate_r << 3)).resize(GCElementWidth)

        // @todo atomic cmpxchg
        issueReq(
          io.MreqMainIml,
          alloc_region_r + U"x10",
          True,
          U(8),
          new_top,
          issuedMainIml
        ) { _ => }

        when(issuedMainIml) {
          issuedMainIml := False

          when(alloc_top === alloc_top) {
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

    done := False

    def fire(
        min_word_size: UInt,
        desired_word_size: UInt,
        bot_updates: Bool,
        alloc_region: UInt
    ): Unit = {
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

    val fsm = new StateMachine {
      val IDLE = new State with EntryPoint
      val START_IML_AND_READ_REGION = new State
      val READ_BOT = new State
      val WAIT_IML_DONE = new State
      val WRITE_FIRST_CARD_AND_PREPARE_FILL = new State
      val FILL_CARDS = new State
      val UPDATE_BOT = new State

      IDLE.whenIsActive {
        when(start && !busy) {
          busy := True
          goto(START_IML_AND_READ_REGION)
        }
      }

      START_IML_AND_READ_REGION.whenIsActive {
        parAllocateIml.fire(
          min_word_size_r,
          desired_word_size_r,
          alloc_region_r
        )

        issueReq(
          io.MreqPar,
          alloc_region_r + U"x20",
          False,
          U(24),
          U(0),
          issuedPar
        ) { rd =>
          next_offset_threshold := rd(GCElementWidth - 1 downto 0)
          index := rd(GCElementWidth * 2 - 1 downto GCElementWidth)
          bot_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          goto(READ_BOT)
        }
      }

      READ_BOT.whenIsActive {
        issueReq(io.MreqPar, bot_ptr, False, U(24), U(0), issuedPar) { rd =>
          reserved_start := rd(GCElementWidth - 1 downto 0)
          array_ptr := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          goto(WAIT_IML_DONE)
        }
      }

      WAIT_IML_DONE.whenIsActive {
        when(par_allocate_iml_done_reg) {
          par_allocate_iml_done_reg := False

          val blk_end_value =
            (destObjPtr + (actualWordSize << 3)).resize(GCElementWidth)
          blk_start := destObjPtr
          blk_end := blk_end_value

          when(
            destObjPtr =/= 0 && bot_updates_r && blk_end_value > next_offset_threshold
          ) {
            goto(WRITE_FIRST_CARD_AND_PREPARE_FILL)
          }.otherwise {
            busy := False
            done := True
            goto(IDLE)
          }
        }
      }

      WRITE_FIRST_CARD_AND_PREPARE_FILL.whenIsActive {
        val writeValue = ((next_offset_threshold - blk_start) >> 3).resize(8)

        issueReq(io.MreqPar, array_ptr, True, U(1), writeValue, issuedPar) {
          _ =>
        }

        when(issuedPar) {
          issuedPar := False

          val end_index_value =
            ((blk_end - 8 - reserved_start) >> 9).resize(GCElementWidth)
          val rem_st =
            (reserved_start + ((index + 1) << 6) << 3).resize(GCElementWidth)
          val rem_end = (reserved_start + ((end_index_value << 6) + 64) << 3)
            .resize(GCElementWidth)
          val start_card =
            ((((index + 1) << 6) << 3) >> 9).resize(GCElementWidth)
          val end_card =
            ((((end_index_value << 6) + 63) << 3) >> 9).resize(GCElementWidth)

          end_index := end_index_value

          when(
            index + 1 <= end_index_value && rem_st < rem_end && start_card <= end_card
          ) {
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

          val fillByte = (U(64, 8 bits) + iterator.resize(8)).asBits
          val lanes = MMUDataWidth / 8
          val writeBytes = Vec(Bits(8 bits), lanes)

          for (b <- 0 until lanes) {
            writeBytes(b) := Mux(U(b) < nbytes, fillByte, B(0, 8 bits))
          }

          val writeValue = writeBytes.asBits.asUInt

          issueReq(io.MreqPar, begin, True, nbytes, writeValue, issuedPar) {
            _ =>
          }

          when(issuedPar) {
            issuedPar := False
            begin := begin + nbytes
            remaining := remaining - nbytes
            iterator := iterator + 1
            goto(FILL_CARDS)
          }
        }.otherwise {
          goto(UPDATE_BOT)
        }
      }

      UPDATE_BOT.whenIsActive {
        val write_index = end_index + 1
        val write_threshold =
          (reserved_start + ((end_index << 6) + 64) << 3).resize(GCElementWidth)
        val writeData = Cat(write_index, write_threshold).asUInt

        issueReq(
          io.MreqPar,
          alloc_region_r + U"x20",
          True,
          U(16),
          writeData,
          issuedPar
        ) { _ => }

        when(issuedPar) {
          issuedPar := False
          busy := False
          done := True
          goto(IDLE)
        }
      }
    }
  }

  val attemptAlloc = new Area {
    val start = Bool()
    start := False

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

    def fire(
        region_ptr: UInt,
        alloc_region: UInt,
        desired_word_size: UInt
    ): Unit = {
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

    val fsm = new StateMachine {
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
        issueReq(
          io.MreqAttempt,
          region_ptr_r + U"x40",
          False,
          U(1),
          U(0),
          issuedAttempt
        ) { rd =>
          region_ptr_type := rd(7 downto 0)
          goto(NEW_ALLOC_REQ)
        }
      }

      NEW_ALLOC_REQ.whenIsActive {
        io.ToNewGCAlloc.Valid := True
        io.ToNewGCAlloc.regionPtr := region_ptr_r
        io.ToNewGCAlloc.regionType := region_ptr_type

        when(io.ToNewGCAlloc.Valid && io.ToNewGCAlloc.Ready) {
          goto(READ_ALLOC_REGION)
        }
      }

      READ_ALLOC_REGION.whenIsActive {
        when(alloc_region_r =/= io.ConfigIO.DummyRegion) {
          issueReq(
            io.MreqAttempt,
            alloc_region_r,
            False,
            U(24),
            U(0),
            issuedAttempt
          ) { rd =>
            alloc_bottom := rd(GCElementWidth - 1 downto 0)
            alloc_top := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
            goto(READ_REGION_INFO)
          }
        }.otherwise {
          goto(WAIT_NEW_ALLOC_REGION)
        }
      }

      READ_REGION_INFO.whenIsActive {
        issueReq(
          io.MreqAttempt,
          region_ptr_r + U"x10",
          False,
          U(24),
          U(0),
          issuedAttempt
        ) { rd =>
          region_ptr_off10 := rd(GCElementWidth - 1 downto 0)
          bot_updates_r := rd(GCElementWidth * 2)
          allocated_bytes := alloc_top - alloc_bottom - rd(
            GCElementWidth - 1 downto 0
          )

          when(region_ptr_type === U(1, 8 bits)) {
            goto(READ_OLD_SET)
          }.otherwise {
            goto(READ_SURVIVOR)
          }
        }
      }

      READ_OLD_SET.whenIsActive {
        val old_set = io.ConfigIO.G1h + U"xa0"

        issueReq(
          io.MreqAttempt,
          old_set + U"x10",
          False,
          U(4),
          U(0),
          issuedAttempt
        ) { rd =>
          old_set_cnt_r := rd(31 downto 0)
          goto(WRITE_OLD_SET)
        }
      }

      WRITE_OLD_SET.whenIsActive {
        val old_set = io.ConfigIO.G1h + U"xa0"

        issueReq(
          io.MreqAttempt,
          old_set + U"x10",
          True,
          U(4),
          old_set_cnt_r + 1,
          issuedAttempt
        ) { _ => }

        when(issuedAttempt) {
          issuedAttempt := False
          goto(READ_DURING_IM)
        }
      }

      READ_SURVIVOR.whenIsActive {
        val survivor_ptr = io.ConfigIO.G1h + U"x3f8"

        issueReq(
          io.MreqAttempt,
          survivor_ptr + U"x10",
          False,
          U(8),
          U(0),
          issuedAttempt
        ) { rd =>
          survivor_bytes_r := rd(GCElementWidth - 1 downto 0)
          goto(WRITE_SURVIVOR)
        }
      }

      WRITE_SURVIVOR.whenIsActive {
        val survivor_ptr = io.ConfigIO.G1h + U"x3f8"

        issueReq(
          io.MreqAttempt,
          survivor_ptr + U"x10",
          True,
          U(8),
          survivor_bytes_r + allocated_bytes,
          issuedAttempt
        ) { _ => }

        when(issuedAttempt) {
          issuedAttempt := False
          goto(READ_DURING_IM)
        }
      }

      READ_DURING_IM.whenIsActive {
        when(!during_im_valid) {
          issueReq(
            io.MreqAttempt,
            io.ConfigIO.G1h + U"x3c1",
            False,
            U(1),
            U(0),
            issuedAttempt
          ) { rd =>
            during_im_r := rd(0)
            during_im_valid := True

            when(rd(0) && allocated_bytes =/= 0) {
              goto(READ_CM)
            }.otherwise {
              goto(WRITE_REGION_TO_DUMMY)
            }
          }
        }.otherwise {
          when(during_im_r && allocated_bytes =/= 0) {
            goto(READ_CM)
          }.otherwise {
            goto(WRITE_REGION_TO_DUMMY)
          }
        }
      }

      READ_CM.whenIsActive {
        when(!cm_valid) {
          issueReq(
            io.MreqAttempt,
            io.ConfigIO.G1h + U"x4e8",
            False,
            U(8),
            U(0),
            issuedAttempt
          ) { rd =>
            cm_r := rd(GCElementWidth - 1 downto 0)
            cm_valid := True
            goto(READ_ROOT_START)
          }
        }.otherwise {
          goto(READ_ROOT_START)
        }
      }

      READ_ROOT_START.whenIsActive {
        issueReq(
          io.MreqAttempt,
          alloc_region_r + U"xe8",
          False,
          U(8),
          U(0),
          issuedAttempt
        ) { rd =>
          root_start_r := rd(GCElementWidth - 1 downto 0)
          goto(READ_ROOT_REGIONS)
        }
      }

      READ_ROOT_REGIONS.whenIsActive {
        val root_regions_ptr = cm_r + U"xb0"

        issueReq(
          io.MreqAttempt,
          root_regions_ptr,
          False,
          U(24),
          U(0),
          issuedAttempt
        ) { rd =>
          root_regions_array_r := rd(GCElementWidth - 1 downto 0)
          root_regions_idx_r := rd(
            GCElementWidth * 3 - 1 downto GCElementWidth * 2
          )
          goto(WRITE_ROOT_REGION)
        }
      }

      WRITE_ROOT_REGION.whenIsActive {
        val mem_region = (root_regions_array_r + (root_regions_idx_r << 4))
          .resize(MMUAddrWidth)
        val word_len = (alloc_top - root_start_r) >> 3

        val write_data = Cat(
          word_len.asBits,
          root_start_r.asBits
        ).asUInt

        issueReq(
          io.MreqAttempt,
          mem_region,
          True,
          U(16),
          write_data,
          issuedAttempt
        ) { _ => }

        when(issuedAttempt) {
          issuedAttempt := False
          goto(WRITE_ROOT_INDEX)
        }
      }

      WRITE_ROOT_INDEX.whenIsActive {
        val root_regions_ptr = cm_r + U"xb0"

        issueReq(
          io.MreqAttempt,
          root_regions_ptr + U"x10",
          True,
          U(8),
          root_regions_idx_r + 1,
          issuedAttempt
        ) { _ => }

        when(issuedAttempt) {
          issuedAttempt := False
          goto(WRITE_REGION_TO_DUMMY)
        }
      }

      WRITE_REGION_TO_DUMMY.whenIsActive {
        val writeData =
          Cat(U(0), region_ptr_off10, io.ConfigIO.DummyRegion).asUInt

        issueReq(
          io.MreqAttempt,
          region_ptr_r + U"x8",
          True,
          U(24),
          writeData,
          issuedAttempt
        ) { _ => }

        when(issuedAttempt) {
          issuedAttempt := False

          updateValid := True
          updateRegionPtr := region_ptr_r
          updateRegion := io.ConfigIO.DummyRegion

          goto(WAIT_NEW_ALLOC_REGION)
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
          issueReq(
            io.MreqAttempt,
            region_ptr_r + U"x10",
            False,
            U(24),
            U(0),
            issuedAttempt
          ) { rd =>
            region_ptr_off10 := rd(GCElementWidth - 1 downto 0)
            bot_updates_r := rd(GCElementWidth * 3)
            goto(START_PAR_AND_CLEAR_NEW_REGION)
          }
        }
      }

      START_PAR_AND_CLEAR_NEW_REGION.whenIsActive {
        parAllocate.fire(
          desired_word_size_r,
          desired_word_size_r,
          bot_updates_r,
          new_alloc_region_r
        )

        issueReq(
          io.MreqAttempt,
          new_alloc_region_r + U"xa8",
          True,
          U(8),
          U(0),
          issuedAttempt
        ) { _ => }

        when(issuedAttempt) {
          issuedAttempt := False
          goto(READ_NEW_ALLOC_REGION)
        }
      }

      READ_NEW_ALLOC_REGION.whenIsActive {
        issueReq(
          io.MreqAttempt,
          new_alloc_region_r,
          False,
          U(24),
          U(0),
          issuedAttempt
        ) { rd =>
          alloc_bottom := rd(GCElementWidth - 1 downto 0)
          alloc_top := rd(GCElementWidth * 3 - 1 downto GCElementWidth * 2)
          goto(WRITE_REGION_TO_NEW)
        }
      }

      WRITE_REGION_TO_NEW.whenIsActive {
        val writeOff18 = alloc_top - alloc_bottom
        val writeOff10 = region_ptr_off10 + 1
        val writeOff8 = new_alloc_region_r
        val writeData = Cat(writeOff18, writeOff10, writeOff8).asUInt

        issueReq(
          io.MreqAttempt,
          region_ptr_r + U"x8",
          True,
          U(24),
          writeData,
          issuedAttempt
        ) { _ => }

        when(issuedAttempt) {
          issuedAttempt := False

          updateValid := True
          updateRegionPtr := region_ptr_r
          updateRegion := new_alloc_region_r

          goto(WAIT_PAR_DONE)
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

  val mainFsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val FIRST_ALLOC = new State
    val WAIT_IML_DONE = new State
    val START_PAR_ALLOC = new State
    val WAIT_PAR_ALLOC = new State
    val READ_ALLOC_REGION_LOCK = new State
    val UNLOCK_ALLOC_REGION = new State
    val CHECK_ALLOC_RESULT = new State
    val LOCK_FREELIST = new State
    val WRITE_FREELIST_OWNER = new State
    val START_ATTEMPT = new State
    val WAIT_ATTEMPT = new State
    val MARK_ALLOCATOR_FULL = new State
    val READ_FREELIST_LOCK = new State
    val UNLOCK_FREELIST = new State

    def resetState(): Unit = {
      io.ToDoAllocate.Done := True
      io.ToDoAllocate.DestObjPtr := destObjPtr
      io.ToDoAllocate.ActualPlabSize := actualWordSize
      io.ToDoAllocate.updateCacheValid := updateValid
      io.ToDoAllocate.updateRegionPtr := updateRegionPtr
      io.ToDoAllocate.updateRegion := updateRegion
      goto(IDLE)
    }

    IDLE.whenIsActive {
      io.ToDoAllocate.Ready := True

      when(io.ToDoAllocate.Valid && io.ToDoAllocate.Ready) {
        nodeIndex := io.ToDoAllocate.NodeIndex
        destAttrIdx := io.ToDoAllocate.DestAttrIdx
        regionPtr := io.ToDoAllocate.regionPtr
        allocRegion := io.ToDoAllocate.allocRegion
        desiredWordSize := io.ToDoAllocate.DesiredWordSize
        minWordSize := io.ToDoAllocate.MinWordSize
        allocatorPtr := io.ToDoAllocate.AllocatorPtr

        actualWordSize := 0

        firstTryDone := False
        retryTryDone := False
        attemptNewRegionDone := False

        selectAllocate := False

        goto(FIRST_ALLOC)
      }
    }

    FIRST_ALLOC.whenIsActive {
      when(destAttrIdx === U(0)) {
        parAllocateIml.fire(minWordSize, desiredWordSize, allocRegion)
        goto(WAIT_IML_DONE)
      }.otherwise {
        allocRegionLockPtr := allocRegion + U"x40"
        val addr = allocRegion + U"x48"

        issueReq(
          io.MreqMainIml,
          addr,
          True,
          U(4),
          U(1, 32 bits),
          issuedMainIml
        ) { _ => }

        when(issuedMainIml) {
          issuedMainIml := False

          when(lockValue === U(0, 32 bits)) {
            goto(START_PAR_ALLOC)
          }.otherwise {
            goto(FIRST_ALLOC)
          }
        }
      }
    }

    WAIT_IML_DONE.whenIsActive {
      when(parAllocateIml.done) {
        when(selectAllocate) {
          retryTryDone := True
        }.otherwise {
          firstTryDone := True
        }

        goto(CHECK_ALLOC_RESULT)
      }
    }

    START_PAR_ALLOC.whenIsActive {
      parAllocate.fire(minWordSize, desiredWordSize, True, allocRegion)
      goto(WAIT_PAR_ALLOC)
    }

    WAIT_PAR_ALLOC.whenIsActive {
      when(parAllocate.done) {
        when(selectAllocate) {
          retryTryDone := True
        }.otherwise {
          firstTryDone := True
        }

        goto(READ_ALLOC_REGION_LOCK)
      }
    }

    READ_ALLOC_REGION_LOCK.whenIsActive {
      issueReq(
        io.MreqMainIml,
        allocRegionLockPtr + U"x8",
        False,
        U(4),
        U(0),
        issuedMainIml
      ) { rd =>
        lockValue := rd(31 downto 0)
        goto(UNLOCK_ALLOC_REGION)
      }
    }

    UNLOCK_ALLOC_REGION.whenIsActive {
      issueReq(
        io.MreqMainIml,
        allocRegionLockPtr + U"x8",
        True,
        U(4),
        U(0, 32 bits),
        issuedMainIml
      ) { _ => }

      when(issuedMainIml) {
        issuedMainIml := False
        lockValue := 0
        firstTryDone := True
        goto(CHECK_ALLOC_RESULT)
      }
    }

    CHECK_ALLOC_RESULT.whenIsActive {
      when(destObjPtr === 0) {
        when(selectAllocate) {
          freelistLockPtr := io.ConfigIO.LockPtr
          goto(LOCK_FREELIST)
        }.otherwise {
          issueReq(
            io.MreqMainIml,
            allocatorPtr + U"x10",
            False,
            U(1),
            U(0),
            issuedMainIml
          ) { rd =>
            val isFull = Mux(destAttrIdx === U(0), rd(0), rd(1))

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
      issueReq(
        io.MreqMainIml,
        freelistLockPtr + U"x8",
        True,
        U(4),
        U(1, 32 bits),
        issuedMainIml
      ) { _ => }

      when(issuedMainIml) {
        issuedMainIml := False

        when(lockValue === U(0, 32 bits)) {
          goto(WRITE_FREELIST_OWNER)
        }.otherwise {
          goto(LOCK_FREELIST)
        }
      }
    }

    WRITE_FREELIST_OWNER.whenIsActive {
      issueReq(
        io.MreqMainIml,
        freelistLockPtr,
        True,
        U(8),
        io.ConfigIO.Thread,
        issuedMainIml
      ) { _ => }

      when(issuedMainIml) {
        issuedMainIml := False
        goto(START_ATTEMPT)
      }
    }

    START_ATTEMPT.whenIsActive {
      attemptAlloc.fire(
        regionPtr,
        allocRegion,
        desiredWordSize
      )

      goto(WAIT_ATTEMPT)
    }

    WAIT_ATTEMPT.whenIsActive {
      when(attemptAlloc.done) {
        attemptNewRegionDone := True

        when(destObjPtr === 0) {
          goto(MARK_ALLOCATOR_FULL)
        }.otherwise {
          goto(READ_FREELIST_LOCK)
        }
      }
    }

    MARK_ALLOCATOR_FULL.whenIsActive {
      when(destAttrIdx === U(0)) {
        issueReq(
          io.MreqMainIml,
          allocatorPtr + U"x10",
          True,
          U(1),
          U(1, 8 bits),
          issuedMainIml
        ) { _ => }
      }.otherwise {
        issueReq(
          io.MreqMainIml,
          allocatorPtr + U"x11",
          True,
          U(1),
          U(1, 8 bits),
          issuedMainIml
        ) { _ => }
      }

      when(issuedMainIml) {
        issuedMainIml := False
        goto(READ_FREELIST_LOCK)
      }
    }

    READ_FREELIST_LOCK.whenIsActive {
      issueReq(
        io.MreqMainIml,
        freelistLockPtr + U"x8",
        False,
        U(4),
        U(0),
        issuedMainIml
      ) { rd =>
        lockValue := rd(31 downto 0)
        goto(UNLOCK_FREELIST)
      }
    }

    UNLOCK_FREELIST.whenIsActive {
      issueReq(
        io.MreqMainIml,
        freelistLockPtr + U"x8",
        True,
        U(4),
        U(0, 32 bits),
        issuedMainIml
      ) { _ => }

      when(issuedMainIml) {
        issuedMainIml := False
        lockValue := U(0)
        resetState()
      }
    }
  }
}

object GCDoAllocateVerilog extends App {
  Config.spinal.generateVerilog(new GCDoAllocate())
}
