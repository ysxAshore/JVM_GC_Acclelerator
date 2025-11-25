package hwgc

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

class GCOopCopy2Survivor extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Process2CopySurvivor = slave(new ProcessUnit)
    val Process2Trace = master(new GCProcess2Trace)
    val Process2Copy = master(new GCProcess2Copy)
    val Mreq = master(new LocalMMUIO)
    val ConfigIO = slave(new GCCopy2SurvivorConfigIO)
  }

  // default value
  io.Process2CopySurvivor.DestOopPtr := U(0)
  io.Process2CopySurvivor.Done := False
  io.Process2CopySurvivor.Ready := False

  io.Process2Trace.Valid := False
  io.Process2Trace.OopType := U(0)
  io.Process2Trace.KlassPtr := U(0)
  io.Process2Trace.SrcOopPtr := U(0)
  io.Process2Trace.DestOopPtr := U(0)
  io.Process2Trace.Kid := U(0)
  io.Process2Trace.ArrayLength := U(0)
  io.Process2Trace.PartialArrayStart := U(0)
  io.Process2Trace.StepIndex := U(0)
  io.Process2Trace.StepNCreate := U(0)

  io.Process2Copy.Valid := False
  io.Process2Copy.SrcOopPtr := U(0)
  io.Process2Copy.DestOopPtr := U(0)
  io.Process2Copy.Size := U(0)

  io.Mreq.Request.valid := False
  io.Mreq.Request.payload.clearAll()
  io.Mreq.Response.ready := False

  object overall_state extends SpinalEnum {
    val s_idle, s_readKlassPtr, s_readLhKid, s_getSize, s_readAttrType, s_getAge, s_changeDestAttr, s_readPlabAllocator, s_readDestAttrType, s_getBuffer, s_getObjPtr, s_slowAllocate, s_writeSrcMW, s_updateYoungWords, s_writeDestMW, s_doCopyAndTrace, s_waitDone = newElement()
  }

  object partitial_state extends SpinalEnum {
    val s_0, s_1, s_2, s_3, s_4 = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val sub_state = RegInit(partitial_state.s_0)
  val pss = RegInit(U(0, MMUAddrWidth bits))
  val heapRegionBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val heapRegionShiftBy = RegInit(U(0, 32 bits))
  val regionAttrBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val regionAttrShiftBy = RegInit(U(0, 32 bits))
  val srcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val markWord = RegInit(U(0, MMUDataWidth bits))

  val klass_ptr = RegInit(U(0, MMUAddrWidth bits))
  val lh = RegInit(U(0, 32 bits))
  lh.allowUnsetRegToAvoidLatch()
  val kid = RegInit(U(0, 32 bits))
  kid.allowUnsetRegToAvoidLatch()
  val size = RegInit(U(0, MMUDataWidth bits))
  val src_regionAttrType = RegInit(U(0, 8 bits))
  src_regionAttrType.allowUnsetRegToAvoidLatch()
  val age = RegInit(U(0, 32 bits))
  val destChanged = RegInit(False)
  val dest_attr_ptr = RegInit(U(0, MMUAddrWidth bits))
  val plabAllocatorPtr = RegInit(U(0, MMUAddrWidth bits))
  val dest_regionAttrType = RegInit(U(0, 8 bits))
  val buffer0 = RegInit(U(0, MMUAddrWidth bits))
  val buffer = RegInit(U(0, MMUAddrWidth bits))
  val region_top = RegInit(U(0, MMUAddrWidth bits))
  region_top.allowUnsetRegToAvoidLatch()
  val region_end = RegInit(U(0, MMUAddrWidth bits))
  region_end.allowUnsetRegToAvoidLatch()
  val destOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val from_region = RegInit(U(0, MMUAddrWidth bits))
  val young_index = RegInit(U(0, 32 bits))
  val youngWordsBase = RegInit(U(0, MMUAddrWidth bits))
  val originValue = RegInit(U(0, MMUDataWidth bits))
  val new_mark = RegInit(U(0, MMUAddrWidth bits))

  def mmuOpForState(): (Bool, UInt, Bool, UInt, UInt) = {
    val condition = src_regionAttrType === U(Type_Young) && (markWord & U(UNLOCKED_VALUE)) === U(0)
    val has_monitor = (markWord & U(MONITOR_VALUE)) =/= U(0)
    val ptr = Mux(has_monitor, markWord ^ U(MONITOR_VALUE, MMUDataWidth bits), markWord)
    val temp0 = (markWord & ~U(AGE_MASK_IN_PLACE, MMUDataWidth bits)) | (Mux(age < U(AGE_MASK), age + U(1), age) & U(AGE_MASK, MMUDataWidth bits) << U(AGE_SHIFT))
    val destMW = Mux(dest_regionAttrType === U(Type_Young) && (markWord & U(UNLOCKED_VALUE, MMUDataWidth bits)) =/= U(0), temp0, markWord)
    val temp1 = (new_mark & ~U(AGE_MASK_IN_PLACE, MMUDataWidth bits)) | (Mux(age < U(AGE_MASK), age + U(1), age) & U(AGE_MASK) << U(AGE_SHIFT))

    state match {
      case overall_state.s_readKlassPtr => (True, srcOopPtr + KlassOff, False, U(0), U(0))
      case overall_state.s_readLhKid => (True, klass_ptr + LhKidOff, False, U(0), U(0))
      case overall_state.s_getSize => (lh.asSInt < 0, srcOopPtr + ArrayLenOff, False, U(0), U(0))
      case overall_state.s_readAttrType => (True, regionAttrBiasedBase + (srcOopPtr >> regionAttrShiftBy) * GCHeapRegionAttr_Size, False, U(0), U(0))
      case overall_state.s_getAge => (condition, ptr, False, U(0), U(0))
      case overall_state.s_readPlabAllocator => (True, pss + PLAB_ALLOCATOR_OFFSET, False, U(0), U(0))
      case overall_state.s_readDestAttrType => (destChanged, dest_attr_ptr, False, U(0), U(0))
      case overall_state.s_getBuffer =>
        sub_state match {
          case partitial_state.s_0 =>  (True, plabAllocatorPtr + ALLOC_BUFFERS_OFFSET + dest_regionAttrType * GCObjectPtr_Size, False, U(0), U(0))
          case partitial_state.s_1 =>  (True, buffer0, False, U(0), U(0))
        }
      case overall_state.s_getObjPtr =>
        sub_state match {
          case partitial_state.s_0 => (True, buffer + TOP_OFFSET, False, U(0), U(0))
          case partitial_state.s_1 => (True, buffer + END_OFFSET, False, U(0), U(0))
          case partitial_state.s_2 => ((region_end - region_top) / GCObjectPtr_Size >= size, buffer + TOP_OFFSET, True, allBytesOnes, region_top + size * GCObjectPtr_Size)
        }
      case overall_state.s_writeSrcMW => (True, srcOopPtr + MarkWordOff, True, allBytesOnes, (destOopPtr & ~LOCK_MASK_IN_PLACE) | MARKED_VALUE)
      case overall_state.s_updateYoungWords =>
        sub_state match {
          case partitial_state.s_0 => (True, heapRegionBiasedBase + (srcOopPtr >> heapRegionShiftBy) * GCObjectPtr_Size, False, U(0), U(0))
          case partitial_state.s_1 => (True, from_region + YOUND_INDEX_IN_CSET_OFFSET, False, U(0), U(0))
          case partitial_state.s_2 => (True, pss + SURVIVING_YOUNG_WORDS_BASE_OFFSET, False, U(0), U(0))
          case partitial_state.s_3 => (True, youngWordsBase + young_index * GCObjectPtr_Size, False, U(0), U(0))
          case partitial_state.s_4 => (True, youngWordsBase + young_index * GCObjectPtr_Size, True, allBytesOnes, originValue + size)
        }

      case overall_state.s_writeDestMW =>
        sub_state match {
          case partitial_state.s_0 => (True, destOopPtr + MarkWordOff, True, allBytesOnes, destMW)
          case partitial_state.s_1 => (True, ptr, False, U(0), U(0))
          case partitial_state.s_2 => (True, ptr, True, allBytesOnes, temp1)
        }
      case _ => (False, U(0, MMUAddrWidth bits), False, U(0, MMUDataWidth / 8 bits), U(0, MMUDataWidth bits))
    }
  }

  def mmuResponseHandler(rd : UInt): Unit = {
    state match {
      case overall_state.s_readKlassPtr =>
        klass_ptr := rd
        state := overall_state.s_readLhKid
      case overall_state.s_readLhKid =>
        lh := rd(31 downto 0)
        kid := rd(63 downto 32)
        state := overall_state.s_getSize
      case overall_state.s_getSize =>
        val size_in_bytes = (rd(31 downto 0) << lh(7 downto 0)) + lh(23 downto 16)
        size := Mux(size_in_bytes(LogHeapWordSize - 1 downto 0) =/= U(0), (size_in_bytes >> LogHeapWordSize) + U(1), size_in_bytes >> LogHeapWordSize).resized
        state := overall_state.s_readAttrType
      case overall_state.s_readAttrType =>
        src_regionAttrType := rd(15 downto 8)
        dest_attr_ptr := (pss + REGION_ATTR_DEST_OFFSET + rd(15 downto 8) * GCHeapRegionAttr_Size).resized
        state := overall_state.s_getAge
      case overall_state.s_getAge =>
        age := (rd >> U(AGE_SHIFT)) & U(AGE_MASK)
        state := overall_state.s_changeDestAttr
      case overall_state.s_changeDestAttr =>
        when(age < rd){
          dest_attr_ptr := (regionAttrBiasedBase + (srcOopPtr >> regionAttrShiftBy) * GCHeapRegionAttr_Size).resized
          destChanged := True
        }
        state := overall_state.s_readPlabAllocator
      case overall_state.s_readPlabAllocator =>
        plabAllocatorPtr := rd
        state := overall_state.s_readDestAttrType
      case overall_state.s_readDestAttrType =>
        dest_regionAttrType := rd(15 downto 8)
        state := overall_state.s_getBuffer
      case overall_state.s_getBuffer =>
        switch(sub_state){
          is(partitial_state.s_0){
            buffer0 := rd
            sub_state := partitial_state.s_1
          }
          is(partitial_state.s_1){
            buffer := rd
            sub_state := partitial_state.s_0
            state := overall_state.s_getObjPtr
          }
        }
      case overall_state.s_getObjPtr =>
        switch(sub_state){
          is(partitial_state.s_0){
             region_top := rd
             sub_state := partitial_state.s_1
          }
          is(partitial_state.s_1){
             region_end := rd
             sub_state := partitial_state.s_2
          }
          is(partitial_state.s_2){
            destOopPtr := region_top
            sub_state := partitial_state.s_0
            state := overall_state.s_writeSrcMW
          }
        }
      case overall_state.s_writeSrcMW =>
        state := overall_state.s_writeDestMW
      case overall_state.s_updateYoungWords =>
        switch(sub_state){
          is(partitial_state.s_0){
            from_region := rd
            sub_state := partitial_state.s_1
          }
          is(partitial_state.s_1){
            young_index := rd(31 downto 0)
            sub_state := partitial_state.s_2
          }
          is(partitial_state.s_2){
            youngWordsBase := rd
            sub_state := partitial_state.s_3
          }
          is(partitial_state.s_3){
            originValue := rd
            sub_state := partitial_state.s_4
          }
          is(partitial_state.s_4){
            sub_state := partitial_state.s_0
          }
        }
      case overall_state.s_writeDestMW =>
        switch(sub_state){
          is(partitial_state.s_0){
            when(src_regionAttrType === U(Type_Young) && (markWord & UNLOCKED_VALUE) === U(0)){
              sub_state := partitial_state.s_1
            }.otherwise{
              state := overall_state.s_doCopyAndTrace
            }
          }
          is(partitial_state.s_1){
            new_mark := rd
            sub_state := partitial_state.s_2
          }
          is(partitial_state.s_2){
            sub_state := partitial_state.s_0
            state := overall_state.s_doCopyAndTrace
          }
        }
      case _ =>
    }
  }

  val issuedCopy = RegInit(False)
  val issuedTrace = RegInit(False)
  val copyDone = RegInit(False)
  val traceDone = RegInit(False)

  switch(state){
    is(overall_state.s_idle){
      io.Process2CopySurvivor.Ready := True
      when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
        srcOopPtr := io.Process2CopySurvivor.SrcOopPtr
        markWord := io.Process2CopySurvivor.MarkWord

        pss := io.ConfigIO.ParScanThreadStatePtr
        heapRegionBiasedBase := io.ConfigIO.HeapRegionBiasedBase
        heapRegionShiftBy := io.ConfigIO.HeapRegionShiftBy
        regionAttrBiasedBase := io.ConfigIO.RegionAttrBiasedBase
        regionAttrShiftBy := io.ConfigIO.RegionAttrShiftBy

        state := overall_state.s_readKlassPtr
      }
    }
    is(overall_state.s_getSize){
      when(lh.asSInt >= 0){
        size := (lh >> LogHeapWordSize).resized
        state := overall_state.s_readAttrType
      }
    }
    is(overall_state.s_getAge){
      destChanged := False
      when(src_regionAttrType === U(Type_Young) && (markWord & UNLOCKED_VALUE) === U(0)){
      }.elsewhen(src_regionAttrType === U(Type_Young)) {
        age := ((markWord >> U(AGE_SHIFT)) & U(AGE_MASK)).resized
        state := overall_state.s_changeDestAttr
      }.otherwise{
        age := U(0)
        state := overall_state.s_readPlabAllocator
      }
    }
    is(overall_state.s_readDestAttrType){
      when(!destChanged){
        dest_regionAttrType := src_regionAttrType
        state := overall_state.s_getBuffer
      }
    }
    is(overall_state.s_getObjPtr){
      when(sub_state === partitial_state.s_2){
        when((region_end - region_top)/GCObjectPtr_Size < size){
          destOopPtr := U(0)
          sub_state := partitial_state.s_0
          state := overall_state.s_slowAllocate
        }
      }
    }

    is(overall_state.s_slowAllocate){
      // not enter this state temporately
    }

    is(overall_state.s_doCopyAndTrace){
      when(!issuedCopy){
        io.Process2Copy.Valid := True
        io.Process2Copy.SrcOopPtr := srcOopPtr
        io.Process2Copy.DestOopPtr := destOopPtr
      }
      when(!issuedTrace){
        io.Process2Trace.Valid := True
        io.Process2Trace.OopType := U(CommonOop)
        io.Process2Trace.SrcOopPtr := srcOopPtr
        io.Process2Trace.DestOopPtr := destOopPtr
        io.Process2Trace.Kid := kid
      }
      when((io.Process2Copy.Valid && io.Process2Copy.Ready && io.Process2Trace.Valid && io.Process2Trace.Ready) || (issuedTrace && issuedCopy)){
        issuedCopy := False
        issuedTrace := False
        state := overall_state.s_waitDone
      }.elsewhen(io.Process2Copy.Valid && io.Process2Copy.Ready){
        issuedCopy := True
      }.elsewhen(io.Process2Trace.Valid && io.Process2Trace.Ready){
        issuedTrace := True
      }
    }


    is(overall_state.s_waitDone){
      when(io.Process2Copy.Done && io.Process2Trace.Done || copyDone && traceDone){
        traceDone := False
        copyDone := False
        io.Process2CopySurvivor.Done := True
        io.Process2CopySurvivor.DestOopPtr := destOopPtr
        state := overall_state.s_idle
      }.elsewhen(io.Process2Trace.Done){
        traceDone := True
      }.elsewhen(io.Process2Copy.Done){
        copyDone := True
      }
    }
  }

  val reqIssued = RegInit(False)
  val (want, addr, isWrite, wmask, wdata) = mmuOpForState()
  when(want){
    issueReq(io.Mreq, addr.resized, isWrite, wmask.resized, wdata.resized, reqIssued) { rd =>
      mmuResponseHandler(rd)
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}
