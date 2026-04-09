package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCTrace extends Module with GCParameters with HWParameters{
  val io = new Bundle {
    val Mreq = master(new LocalMMUIO)
    val ToAop = master(new GCToAop)
    val ToTrace = slave(new GCToTrace)
    val ToStack = master(new GCToStack)
    val Trace2Fetch = master Stream UInt(GCElementWidth bits)
    val TaskDone = in(Bool())
    val ConfigIO = slave(new GCTraceConfigIO)
    val DebugTimeStamp = in UInt(64 bits)
  }

  // default value
  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.RequestSize.valid := False
  io.Mreq.RequestSize.payload.clearAll()
  io.Mreq.Response.ready := True

  io.ToAop.clearIn()

  io.ToTrace.clearOut()

  io.ToStack.Push.valid := False
  io.ToStack.Push.payload.clearAll()
  io.ToStack.LastPush := io.Trace2Fetch.fire

  io.Trace2Fetch.valid := False
  io.Trace2Fetch.payload.clearAll()

  // State Machine
  object overall_state extends SpinalEnum {
    val states = Array.tabulate(16)(_ => newElement())
    for((state, i) <- states.zipWithIndex){
      state.setName(s"s$i")
    }
  }

  val state = RegInit(overall_state.states(0))
  val issued = RegInit(False)

  val Kid = RegInit(U(0, 32 bits))
  val OopType = RegInit(U(0, GCOopTypeWidth bits))
  val KlassPtr = RegInit(U(0, GCElementWidth bits))
  val SrcOopPtr = RegInit(U(0, GCElementWidth bits))
  val DestOopPtr = RegInit(U(0, GCElementWidth bits))
  val ScanningInYoung = RegInit(False)
  val StepIndex = RegInit(U(0, 32 bits))
  val StepNCreate = RegInit(U(0, 32 bits))
  val ArrayLength = RegInit(U(0, 32 bits))
  val PartialArrayStart = RegInit(U(0, 32 bits))

  val p = RegInit(U(0, GCElementWidth bits))
  val q = RegInit(U(0, GCElementWidth bits))
  val vtable_len = RegInit(U(0, 32 bits))
  val start_map = RegInit(U(0, GCElementWidth bits))
  val end_map = RegInit(U(0, GCElementWidth bits))
  val src = RegInit(U(0, GCElementWidth bits))
  val dest = RegInit(U(0, GCElementWidth bits))
  val heap_oop = RegInit(U(0, GCElementWidth bits))
  val regionAttr = RegInit(U(0, 16 bits))
  val region = RegInit(U(0, 32 bits))
  val bool_base_value = RegInit(U(0, 8 bits))

  val previousState = RegInit(overall_state.states(0))
  val for_counter = RegInit(U(0, 32 bits))

  val pendingPushValid   = RegInit(False)
  val pendingPushPayload = RegInit(U(0, GCElementWidth bits))

  // klass meta data cache
  val KlassCacheEntries = 16
  val klassCacheValid  = Vec(RegInit(False), KlassCacheEntries)
  val klassCachePtr    = Vec(Reg(UInt(GCElementWidth bits)) init(0), KlassCacheEntries)
  val klassCacheVtable = Vec(Reg(UInt(32 bits)) init(0), KlassCacheEntries)
  val klassCacheStartMap  = Vec(Reg(UInt(GCElementWidth bits)) init(0), KlassCacheEntries)
  val klassCacheEndMap  = Vec(Reg(UInt(GCElementWidth bits)) init(0), KlassCacheEntries)

  val klassCacheHit      = Bool()
  val klassCacheHitIndex = UInt(log2Up(KlassCacheEntries) bits)
  val klassCacheAllocIndex = UInt(log2Up(KlassCacheEntries) bits)

  val klassCacheHitVec = Vec(Bool(), KlassCacheEntries)
  for(i <- 0 until KlassCacheEntries){
    klassCacheHitVec(i) := klassCacheValid(i) && (klassCachePtr(i) === KlassPtr)
  }

  // Hit only -> can use OHToUInt
  klassCacheHit := klassCacheHitVec.orR
  klassCacheHitIndex := OHToUInt(klassCacheHitVec.asBits)
  val klassCacheReplacePtr = RegInit(U(0, log2Up(KlassCacheEntries) bits))

  // region cache
  val humRegionCacheEntries = 8
  val humRegionCacheValid = Vec(RegInit(False), humRegionCacheEntries)
  val humRegionCacheTag   = Vec(Reg(UInt(32 bits)) init(0), humRegionCacheEntries)
  val regionLookup = ((heap_oop - (io.ConfigIO.HeapRegionBias << io.ConfigIO.HeapRegionShiftBy(4 downto 0))) >> io.ConfigIO.LogOfHRGrainBytes).resize(32)

  val humRegionHitVec = Vec(Bool(), humRegionCacheEntries)
  for(i <- 0 until humRegionCacheEntries){
    humRegionHitVec(i) := humRegionCacheValid(i) && (humRegionCacheTag(i) === regionLookup)
  }
  val humRegionHit = humRegionHitVec.orR
  val humRegionCacheReplacePtr = RegInit(U(0, log2Up(humRegionCacheEntries) bits))

  // region attr cache
  val regionAttrCacheEntries = 8
  val regionAttrCacheValid = Vec(RegInit(False), regionAttrCacheEntries)
  val regionAttrCacheTag = Vec(RegInit(U(0, GCElementWidth bits)), regionAttrCacheEntries)
  val regionAttrCache = Vec(RegInit(U(0, 16 bits)), regionAttrCacheEntries)
  val compressed_oop = (io.ConfigIO.CompressedOopBase + (heap_oop << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth)
  val current_oop = Mux(io.ConfigIO.UseCompressedOops, compressed_oop, heap_oop)
  val regionAttrAddrLookup = (io.ConfigIO.RegionAttrBiasedBase + (current_oop >> io.ConfigIO.RegionAttrShiftBy) * U(2)).resize(MMUAddrWidth)

  val regionAttrHitVec = Vec(Bool(), regionAttrCacheEntries)
  for(i <- 0 until regionAttrCacheEntries){
    regionAttrHitVec(i) := regionAttrCacheValid(i) && (regionAttrCacheTag(i) === regionAttrAddrLookup)
  }
  val regionAttrHit = regionAttrHitVec.orR
  val regionAttrHitIndex = OHToUInt(regionAttrHitVec.asBits)
  val regionAttrCacheReplacePtr = RegInit(U(0, log2Up(regionAttrCacheEntries) bits))

  val plus_or_dec = RegInit(False)
  val heap_oop_valid = RegInit(False)
  val remainCount = RegInit(U(0, 32 bits))
  val heap_oop_cache = RegInit(U(0, MMUDataWidth bits))
  val heap_oop_addr = RegInit(U(0, MMUAddrWidth bits))

  // some const value
  val oopStride = Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))
  val objArrayHeaderSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(16), U(24))
  val objArrayMarkKlassLenSize = Mux(io.ConfigIO.UseCompressedKlassPointers, U(8), U(12))

  val counter = RegInit(U(0, GCElementWidth bits))
  when(state =/= overall_state.states(0)){
    counter := counter + 1
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCTrace<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def resetState(): Unit = {
    io.ToTrace.Done := True
    state := overall_state.states(0)
  }

  def pushTask(task: UInt)(afterAccept: => Unit): Unit = {
    when(!pendingPushValid) {
      pendingPushValid := True
      pendingPushPayload := task
      afterAccept
    }.otherwise {
      io.ToStack.Push.valid := True
      io.ToStack.Push.payload := pendingPushPayload
      when(io.ToStack.Push.fire) {
        pendingPushPayload := task
        pendingPushValid := True
        afterAccept
      }
    }
  }

  def readFromWindow(base: UInt, data: UInt): Unit = {
    when(io.ConfigIO.UseCompressedOops) {
      val idx32 = ((src - base) >> 2).resize(3 bits)   // 32B / 4B = 8 entries
      val split32 = data.subdivideIn(32 bits)
      heap_oop := Cat(U(0, 32 bits), split32(idx32)).asUInt
    } otherwise {
      val idx64 = ((src - base) >> 3).resize(2 bits)   // 32B / 8B = 4 entries
      val split64 = data.subdivideIn(GCElementWidth bits)
      heap_oop := split64(idx64)
    }
    state := overall_state.states(9)
  }

  def issueElemRead(addr: UInt): Unit = {
    val elemBytes = Mux(io.ConfigIO.UseCompressedOops, U(4), U(8))
    issueReq(io.Mreq, addr, False, elemBytes, U(0), issued) { rd =>
      when(io.ConfigIO.UseCompressedOops) {
        heap_oop := Cat(U(0, 32 bits), rd(31 downto 0)).asUInt
      } otherwise {
        heap_oop := rd(GCElementWidth - 1 downto 0)
      }
      state := overall_state.states(9)
    }
  }

  def issueWindowRead(addr: UInt): Unit = {
    issueReq(io.Mreq, addr, False, U(32), U(0), issued) { rd =>
      heap_oop_addr  := addr
      heap_oop_cache := rd
      heap_oop_valid := True
      readFromWindow(addr, rd)
    }
  }

  when(io.TaskDone){
    for(i <- 0 until KlassCacheEntries){
      klassCacheValid(i) := False
    }
    for(i <- 0 until humRegionCacheEntries){
      humRegionCacheValid(i) := False
    }
    for(i <- 0 until regionAttrCacheEntries){
      regionAttrCacheValid(i) := False
    }
  }

  switch(state){
    is(overall_state.states(0)){
      io.ToTrace.Ready := True
      when(io.ToTrace.Valid && io.ToTrace.Ready){
        Kid := io.ToTrace.Kid
        OopType := io.ToTrace.OopType
        KlassPtr := io.ToTrace.KlassPtr
        SrcOopPtr := io.ToTrace.SrcOopPtr
        DestOopPtr := io.ToTrace.DestOopPtr
        ScanningInYoung := io.ToTrace.ScanningInYoung
        StepIndex := io.ToTrace.StepIndex
        StepNCreate := io.ToTrace.StepNCreate
        ArrayLength := io.ToTrace.ArrayLength
        PartialArrayStart := Mux(io.ToTrace.OopType === U(PartialArrayOop), io.ToTrace.PartialArrayStart, U(0))

        for_counter := U(0)
        state := overall_state.states(1)

        heap_oop_addr := U(0)

        dbg(Seq(
          "Receive GCTrace Task", ", RegionAttrBase = ", io.ConfigIO.RegionAttrBase, ", RegionAttrShiftBy = ", io.ConfigIO.RegionAttrShiftBy,
          ", RegionAttrBiasedBase = ", io.ConfigIO.RegionAttrBiasedBase, ", HeapRegionBias = ", io.ConfigIO.HeapRegionBias,
          ", HeapRegionShiftBy = ", io.ConfigIO.HeapRegionShiftBy, ", LogOfHRGrainBytes = ", io.ConfigIO.LogOfHRGrainBytes,
          ", HumongousReclaimCandidatesBoolBase = ", io.ConfigIO.HumongousReclaimCandidatesBoolBase, ", OopType = ", io.ToTrace.OopType,
          ", KlassPtr = ", io.ToTrace.KlassPtr, ", SrcOopPtr = ", io.ToTrace.SrcOopPtr, ", DestOopPtr = ", io.ToTrace.DestOopPtr,
          ", Kid = ", io.ToTrace.Kid, ", ArrayLength = ", io.ToTrace.ArrayLength, ", PartialArrayStart = ", io.ToTrace.PartialArrayStart,
          ", StepIndex = ", io.ToTrace.StepIndex, ", StepNCreate = ", io.ToTrace.StepNCreate,
          "\n"
        ))
      }
    }

    is(overall_state.states(1)) {
      when(OopType === U(PartialArrayOop)){
        val addr = DestOopPtr + Mux(io.ConfigIO.UseCompressedKlassPointers, U"xc", U"x10")
        val writeValue = ArrayLength
        issueReq(io.Mreq, addr, True, 4, writeValue, issued){ _ =>
        }
        when(issued){
          state := overall_state.states(2)
          issued := False
        }
      }.elsewhen(Kid === U(ObjectArrayKlassID)){
        val addr = DestOopPtr + U(8)
        val writeLen = StepIndex
        val writeValue = Mux(io.ConfigIO.UseCompressedKlassPointers, Cat(writeLen, KlassPtr(31 downto 0)).resize(96), Cat(writeLen, KlassPtr)).asUInt
        issueReq(io.Mreq, addr, True, objArrayMarkKlassLenSize, writeValue, issued) { _ =>
        }
        when(issued){
          state := overall_state.states(2)
          issued := False
        }
      }.otherwise{
        when(klassCacheHit){
          vtable_len := klassCacheVtable(klassCacheHitIndex)
          start_map := klassCacheStartMap(klassCacheHitIndex)
          end_map := klassCacheEndMap(klassCacheHitIndex)
          state := overall_state.states(4)
        }.otherwise {
          val addr = KlassPtr + U(160)
          issueReq(io.Mreq, addr, False, U(4), U(0), issued) { rd =>
            vtable_len := rd(31 downto 0)
            state := overall_state.states(3)
          }
        }
      }
    }

    is(overall_state.states(2)){
      val cond = Mux(OopType === U(PartialArrayOop), for_counter < StepNCreate, ArrayLength > StepIndex)
      val low = (DestOopPtr + objArrayHeaderSize + PartialArrayStart * oopStride).resize(GCElementWidth)
      val high = (DestOopPtr + objArrayHeaderSize + StepIndex * oopStride).resize(GCElementWidth)
      val temp_p = DestOopPtr + objArrayHeaderSize
      val temp_q = (temp_p + ArrayLength * oopStride).resize(GCElementWidth)
      val q_value = Mux(temp_q > high, high, temp_q)
      val p_value = Mux(temp_p < low, low, temp_p)

      p := p_value
      q := q_value
      remainCount := ((q_value - p_value) >> Mux(io.ConfigIO.UseCompressedOops, U(2), U(3))).resize(32)

      when(cond){
        val task = SrcOopPtr + U(2)
        pushTask(task) {
          when(OopType === U(PartialArrayOop)){
            for_counter := for_counter + U(1)
            state := overall_state.states(2)
          }.otherwise{
            state := overall_state.states(6)
          }
        }
      }.otherwise{
        state := overall_state.states(6)
      }
    }

    is(overall_state.states(3)){
      val addr = KlassPtr + U(296)
      issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
        val new_startMap = (KlassPtr + U(464) + (vtable_len + rd(63 downto 32)) * U(8)).resize(GCElementWidth)
        val new_endMap = (KlassPtr + U(464) + (vtable_len + rd(63 downto 32) + rd(31 downto 0)) * U(8)).resize(GCElementWidth)

        klassCacheValid(klassCacheReplacePtr) := True
        klassCachePtr(klassCacheReplacePtr) := KlassPtr
        klassCacheVtable(klassCacheReplacePtr) := vtable_len
        klassCacheStartMap(klassCacheReplacePtr) := new_startMap
        klassCacheEndMap(klassCacheReplacePtr)   := new_endMap

        klassCacheReplacePtr := klassCacheReplacePtr + 1

        start_map := new_startMap
        end_map := new_endMap
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(4)){
      when(start_map < end_map){
        val addr = end_map - U(8)
        issueReq(io.Mreq, addr, False, U(8), U(0), issued) { rd =>
          p := (DestOopPtr + rd(31 downto 0)).resize(GCElementWidth)
          q := (DestOopPtr + rd(31 downto 0) + rd(63 downto 32) * oopStride).resize(GCElementWidth)
          remainCount := rd(63 downto 32)
          end_map := addr

          // TRACE_DEC -> done to the state
          state := overall_state.states(7)
        }
      }.otherwise{
        for_counter := U(0)
        state := overall_state.states(5)
      }
    }

    is(overall_state.states(5)){
      when(Kid === U(InstanceMirrorKlassID)){
        val addr = SrcOopPtr + U(40)
        issueReq(io.Mreq, addr, False, U(4), U(0), issued) { rd =>
          p := DestOopPtr + U(184)
          q := (DestOopPtr + U(184) + rd(31 downto 0) * oopStride).resize(GCElementWidth)
          remainCount := rd(31 downto 0)

          // TRACE_PLUS -> done to end
          state := overall_state.states(6)
        }
      }.elsewhen(Kid === U(InstanceRefKlassID)){
        when(for_counter === U(3)){
          state := overall_state.states(15)
        }.otherwise {
          val discovered_offset = Mux(io.ConfigIO.UseCompressedOops && io.ConfigIO.UseCompressedKlassPointers, U"x18", Mux(io.ConfigIO.UseCompressedOops, U"x1c", U"x28"))
          val referent_offset = Mux(io.ConfigIO.UseCompressedOops && io.ConfigIO.UseCompressedKlassPointers, U"xc", U"x10")

          src := Mux(for_counter === U(1), SrcOopPtr + referent_offset, SrcOopPtr + discovered_offset)
          dest := Mux(for_counter === U(1), DestOopPtr + referent_offset, DestOopPtr + discovered_offset)

          for_counter := for_counter + U(1)
          previousState := overall_state.states(5)
          state := overall_state.states(8)
        }
      }.otherwise{
        state := overall_state.states(15)
      }
    }

    // TRACE_PLUS
    is(overall_state.states(6)){
      when(p < q){
        src := p - DestOopPtr + SrcOopPtr
        dest := p
        p := p + oopStride

        previousState := overall_state.states(6)
        state := overall_state.states(8)

        plus_or_dec := False
      }.otherwise{
        heap_oop_valid := False
        state := overall_state.states(15)
      }
    }

    // TRACE_DEC
    is(overall_state.states(7)){
      when(p < q){
        val current_q = q - oopStride
        src := current_q - DestOopPtr + SrcOopPtr
        dest := current_q
        q:= current_q

        previousState := overall_state.states(7)
        state := overall_state.states(8)

        plus_or_dec := True
      }.otherwise{
        heap_oop_valid := False
        state := overall_state.states(4)
      }
    }

    is(overall_state.states(8)){
      val alignedBase  = src & ~U(31, src.getWidth bits)
      val cacheHit     = heap_oop_valid && (src >= heap_oop_addr) && (src < heap_oop_addr + LineBytesNum)

      val offAligned = src - alignedBase
      val idxAligned32 = (offAligned >> 2).resize(4 bits) // 0..7
      val idxAligned64 = (offAligned >> 3).resize(3 bits) // 0..3

      // 当前对齐 line 对后续遍历最多还能覆盖多少个元素
      val usableAligned = UInt(4 bits)
      usableAligned := 0
      when(io.ConfigIO.UseCompressedOops) {
        when(plus_or_dec) {
          usableAligned := (idxAligned32 + 1).resized
        } otherwise {
          usableAligned := (U(8) - idxAligned32).resized
        }
      } otherwise {
        when(plus_or_dec) {
          usableAligned := (idxAligned64 + 1).resized
        } otherwise {
          usableAligned := (U(4) - idxAligned64).resized
        }
      }

      // backward 时，为了覆盖更多前面的元素，window 起点往前挪
      val shiftedBase = UInt(src.getWidth bits)
      shiftedBase := src
      when(plus_or_dec) {
        shiftedBase := Mux(io.ConfigIO.UseCompressedOops, src - 28, src - 24)
      }

      when(cacheHit) {
        // 已经在缓存窗口里，直接切
        readFromWindow(heap_oop_addr, heap_oop_cache)
      } otherwise {
        when(remainCount === 1) {
          // 只读一个，就别读整 32B
          issueElemRead(src)
        } elsewhen(remainCount <= usableAligned.resized) {
          // 当前 src 所在的“对齐 32B line”已经足够覆盖接下来要读的元素
          issueWindowRead(alignedBase)
        } otherwise {
          // 当前对齐 line 不够用，才读偏移窗口
          issueWindowRead(shiftedBase)
        }
      }
    }

    is(overall_state.states(9)){
      when(heap_oop === U(0)){
        state := previousState
      }.otherwise{
        when(regionAttrHit){
          regionAttr := regionAttrCache(regionAttrHitIndex)
          heap_oop := current_oop
          state := overall_state.states(10)
        }.otherwise {
          issueReq(io.Mreq, regionAttrAddrLookup, False, U(2), U(0), issued) { rd =>
            regionAttr := rd(15 downto 0)
            heap_oop := current_oop

            regionAttrCacheValid(regionAttrCacheReplacePtr) := True
            regionAttrCacheTag(regionAttrCacheReplacePtr) := regionAttrAddrLookup
            regionAttrCache(regionAttrCacheReplacePtr) := rd(15 downto 0)
            regionAttrCacheReplacePtr := regionAttrCacheReplacePtr + 1

            state := overall_state.states(10)
          }
        }
      }
    }

    is(overall_state.states(10)){
      val regionAttrType = regionAttr(15 downto 8).asSInt
      val cond = ((dest ^ heap_oop) >> io.ConfigIO.LogOfHRGrainBytes(5 downto 0)) =/= U(0)
      when(regionAttrType >= 0){
        val task = dest + Mux(io.ConfigIO.UseCompressedOops, U(1), U(0))
        pushTask(task) {
          state := previousState
        }
      }.elsewhen(cond){
        when(regionAttrType === S(-2)){
          state := overall_state.states(11)
        }.otherwise{
          state := overall_state.states(14)
        }
      }.otherwise{
        state := previousState
      }
    }

    is(overall_state.states(11)){
      when(humRegionHit){
        state := overall_state.states(14)
      }.otherwise {
        val addr = (io.ConfigIO.HumongousReclaimCandidatesBoolBase + regionLookup).resize(MMUAddrWidth)
        issueReq(io.Mreq, addr, False, U(1), U(0), issued) { rd =>
          bool_base_value := rd(7 downto 0)
          region := regionLookup
          state := overall_state.states(12)
        }
      }
    }

    is(overall_state.states(12)){
      humRegionCacheValid(humRegionCacheReplacePtr) := True
      humRegionCacheTag(humRegionCacheReplacePtr) := region

      when(bool_base_value === U(0)){
        state := overall_state.states(14)
        humRegionCacheReplacePtr := humRegionCacheReplacePtr + 1
      }.otherwise{
        val addr = (io.ConfigIO.HumongousReclaimCandidatesBoolBase + region).resize(MMUAddrWidth)
        issueReq(io.Mreq, addr, True, U(1), U(0), issued) { _ =>
        }
        when(issued){
          issued := False
          humRegionCacheReplacePtr := humRegionCacheReplacePtr + 1
          state := overall_state.states(13)
        }
      }
    }

    is(overall_state.states(13)){
      val addr = (io.ConfigIO.RegionAttrBase + region * U(2) + U(1)).resize(MMUAddrWidth)
      val writeValue = S(-1).asUInt
      issueReq(io.Mreq, addr, True, U(1), writeValue, issued){ _ =>
      }
      when(issued){
        issued := False
        state := overall_state.states(14)
      }
    }

    is(overall_state.states(14)){
      when(ScanningInYoung){
        state := previousState
      }.otherwise{
        io.ToAop.Valid := True
        io.ToAop.Task := dest
        io.ToAop.RegionAttr := regionAttr

        when(io.ToAop.Valid && io.ToAop.Ready){
          state := previousState
        }
      }
    }

    is(overall_state.states(15)){
      when(pendingPushValid){
        io.Trace2Fetch.valid := True
        io.Trace2Fetch.payload := pendingPushPayload
        when(io.Trace2Fetch.fire){
          pendingPushValid := False
          resetState()
        }
      }.otherwise{
        resetState()
      }
    }
  }
}

object GCTraceVerilog extends App {
  Config.spinal.generateVerilog(new GCTrace())
}