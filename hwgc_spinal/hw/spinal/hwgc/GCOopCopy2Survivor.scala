package hwgc

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

class GCOopCopy2Survivor extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val Process2CopySurvivor = slave(new GCProcess2Survivor)
    val Process2Trace = master(new GCProcess2Trace)
    val Process2Copy = master(new GCProcess2Copy)
    val CommonMreq = master(new LocalMMUIO)
    val SpecialMreq = Vec.fill(5)(new LocalMMUIO)
    val ConfigIO = slave(new GCCopy2SurvivorConfigIO)
    SpecialMreq.foreach(_.asMaster)
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
  io.Process2Trace.ScanningInYoung := False

  io.Process2Copy.Valid := False
  io.Process2Copy.SrcOopPtr := U(0)
  io.Process2Copy.DestOopPtr := U(0)
  io.Process2Copy.Size := U(0)

  io.CommonMreq.Request.valid := False
  io.CommonMreq.Request.payload.clearAll()
  io.CommonMreq.Response.ready := False

  for(i <- 0 until 5){
    io.SpecialMreq(i).Request.valid := False
    io.SpecialMreq(i).Request.payload.clearAll()
    io.SpecialMreq(i).Response.ready := False
  }

  object overall_state extends SpinalEnum {
    val s_idle, s_readKlassPtr, s_readLhKid, s_getSize, s_readAttr, s_getAge, s_readDestAttr, s_getBuffer, s_getObjPtr, s_slowAllocate, s_doCopyAndTrace, s_waitDone = newElement()
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
  val ageThreshold = RegInit(U(0, 32 bits))
  val chunkSize = RegInit(U(0, 32 bits))
  val plabAllocatorPtr = RegInit(U(0, MMUAddrWidth bits))
  val markWord = RegInit(U(0, MMUDataWidth bits))
  val srcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val regionAttrPtr = RegInit(U(0, MMUAddrWidth bits))

  val klass_ptr = RegInit(U(0, MMUAddrWidth bits))
  val lh = RegInit(U(0, 32 bits))
  lh.allowUnsetRegToAvoidLatch()
  val kid = RegInit(U(0, 32 bits))
  kid.allowUnsetRegToAvoidLatch()
  val arrayLength = RegInit(U(0, 32 bits))
  val size = RegInit(U(0, MMUDataWidth bits))
  val srcRegionAttr = RegInit(U(0, 16 bits))
  srcRegionAttr.allowUnsetRegToAvoidLatch()
  val age = RegInit(U(0, 32 bits))
  val dest_attr_ptr = RegInit(U(0, MMUAddrWidth bits))
  val destRegionAttr = RegInit(U(0, 16 bits))
  val bufferPtr = RegInit(U(0, MMUAddrWidth bits))
  val buffer = RegInit(U(0, MMUAddrWidth bits))
  val region_top = RegInit(U(0, MMUAddrWidth bits))
  region_top.allowUnsetRegToAvoidLatch()
  val region_end = RegInit(U(0, MMUAddrWidth bits))
  region_end.allowUnsetRegToAvoidLatch()
  val destOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val from_region = RegInit(U(0, MMUAddrWidth bits))

  val hasMonitor = (markWord & U(MONITOR_VALUE, MMUDataWidth bits)) =/= U(0, MMUDataWidth bits)
  val ptr = Mux(hasMonitor, markWord ^ U(MONITOR_VALUE, MMUDataWidth bits), markWord)
  val getAgeHasMarkHelper = srcRegionAttr(15 downto 8) === U(Type_Young, 8 bits) && (markWord & U(UNLOCKED_VALUE, MMUDataWidth bits)) === U(0)

  def mmuOpForState(): (Bool, UInt, Bool, UInt, UInt) = {
    state match {
      case overall_state.s_readKlassPtr => (True, srcOopPtr + U(KlassOff), False, U(0), U(0))
      case overall_state.s_readLhKid => (True, klass_ptr + U(LhKidOff), False, U(0), U(0))
      case overall_state.s_getSize => (lh.asSInt < 0, srcOopPtr + U(ArrayLenOff), False, U(0), U(0))
      case overall_state.s_readAttr => (True, regionAttrPtr, False, U(0), U(0))
      case overall_state.s_getAge => (getAgeHasMarkHelper, ptr, False, U(0), U(0))
      case overall_state.s_readDestAttr => (age >= ageThreshold || srcRegionAttr(15 downto 8) =/= U(Type_Young, 8 bits), dest_attr_ptr, False, U(0), U(0))
      case overall_state.s_getBuffer =>
        sub_state match {
          case partitial_state.s_0 =>  (True, (plabAllocatorPtr + U(ALLOC_BUFFERS_OFFSET) + destRegionAttr(15 downto 8) * U(GCObjectPtr_Size)).resized, False, U(0), U(0))
          case partitial_state.s_1 =>  (True, bufferPtr, False, U(0), U(0))
        }
      case overall_state.s_getObjPtr =>
        sub_state match {
          case partitial_state.s_0 => (True, buffer + U(TOP_OFFSET), False, U(0), U(0))
          case partitial_state.s_1 => (True, buffer + U(END_OFFSET), False, U(0), U(0))
          case partitial_state.s_2 => ((region_end - region_top) / U(GCObjectPtr_Size) >= size, buffer + U(TOP_OFFSET), True, allBytesOnes, (region_top + size * U(GCObjectPtr_Size)).resized)
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
        arrayLength := rd(31 downto 0)
        val size_in_bytes = ((rd(31 downto 0) << lh(7 downto 0)) + lh(23 downto 16)).resize(64 bits)
        size := Mux(size_in_bytes(LogHeapWordSize - 1 downto 0) =/= U(0), (size_in_bytes >> U(LogHeapWordSize)) + U(1), size_in_bytes >> LogHeapWordSize).resized
        state := overall_state.s_readAttr
      case overall_state.s_readAttr =>
        srcRegionAttr := rd(15 downto 0)
        dest_attr_ptr := (pss + REGION_ATTR_DEST_OFFSET + rd(15 downto 8) * GCHeapRegionAttr_Size).resized
        state := overall_state.s_getAge
      case overall_state.s_getAge =>
        age := ((rd >> U(AGE_SHIFT)) & U(AGE_MASK, MMUDataWidth bits)).resized
        state := overall_state.s_readDestAttr
      case overall_state.s_readDestAttr =>
        destRegionAttr := rd(15 downto 0)
        state := overall_state.s_getBuffer
      case overall_state.s_getBuffer =>
        switch(sub_state){
          is(partitial_state.s_0){
            bufferPtr := rd
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
  // writeSrcMW
  val writeSrcMW_issued = RegInit(False)
  val completedWriteSrcMW = RegInit(False)

  val writeDestMW_issued = RegInit(False)
  val completedWriteDestMW = RegInit(False)

  val new_mark = RegInit(U(0, MMUDataWidth bits))
  val monitorPtr_issued = RegInit(False)
  val writeMonitor_state = RegInit(partitial_state.s_0)
  val completedWriteMonitorPtr = RegInit(False)

  val young_index = RegInit(U(0, 32 bits))
  val originValue = RegInit(U(0, MMUDataWidth bits))
  val youngWordsBase = RegInit(U(0, MMUAddrWidth bits))
  val updatedYoungWordsBase_state = RegInit(partitial_state.s_0)
  val updatedYoungWordsBase_issued = RegInit(False)
  val completedUpdatedYoungWordsBase = RegInit(False)

  val completedWriteDestLength = RegInit(False)
  val writeDestLengthIssued = RegInit(False)
  val specialWriteDone = completedWriteDestMW && completedWriteMonitorPtr && completedWriteMonitorPtr && completedUpdatedYoungWordsBase && writeDestLengthIssued

  switch(state){
    is(overall_state.s_idle){
      completedWriteSrcMW := False
      completedWriteDestMW := False
      completedWriteMonitorPtr := False
      completedWriteDestLength := False
      completedUpdatedYoungWordsBase := False

      io.Process2CopySurvivor.Ready := True
      when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
        srcOopPtr := io.Process2CopySurvivor.SrcOopPtr
        markWord := io.Process2CopySurvivor.MarkWord
        regionAttrPtr := io.Process2CopySurvivor.RegionAttrPtr

        pss := io.ConfigIO.ParScanThreadStatePtr
        plabAllocatorPtr := io.ConfigIO.PlabAllocatorPtr
        ageThreshold := io.ConfigIO.AgeThreshold
        chunkSize := io.ConfigIO.ChunkSize
        heapRegionBiasedBase := io.ConfigIO.HeapRegionBiasedBase
        heapRegionShiftBy := io.ConfigIO.HeapRegionShiftBy

        state := overall_state.s_readKlassPtr
      }
    }
    is(overall_state.s_getSize){
      when(lh.asSInt >= 0){
        size := (lh >> LogHeapWordSize).resized
        state := overall_state.s_readAttr
      }
    }
    is(overall_state.s_getAge){
      when(getAgeHasMarkHelper){
        // read ptr to get age
      }.elsewhen(srcRegionAttr(15 downto 8) === U(Type_Young, 8 bits)) {
        age := ((markWord >> U(AGE_SHIFT)) & U(AGE_MASK, MMUDataWidth bits)).resized
        state := overall_state.s_readDestAttr
      }.otherwise{
        age := U(0)
        state := overall_state.s_readDestAttr
      }
    }
    is(overall_state.s_readDestAttr){
      when(age < ageThreshold && srcRegionAttr(15 downto 8) === U(Type_Young, 8 bits)){
        dest_attr_ptr := regionAttrPtr
        destRegionAttr := srcRegionAttr
        state := overall_state.s_getBuffer
      }
    }
    is(overall_state.s_getObjPtr){
      when(sub_state === partitial_state.s_2){
        when((region_end - region_top)/ U(GCObjectPtr_Size) < size){
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
      val needTrace = kid =/= TypeArrayKlassID

      io.Process2Copy.Valid := !issuedCopy
      io.Process2Copy.Size := size
      io.Process2Copy.SrcOopPtr := srcOopPtr
      io.Process2Copy.DestOopPtr := destOopPtr

      io.Process2Trace.Valid := needTrace && !issuedTrace
      io.Process2Trace.OopType := U(CommonOop)
      io.Process2Trace.KlassPtr := klass_ptr
      io.Process2Trace.SrcOopPtr := srcOopPtr
      io.Process2Trace.DestOopPtr := destOopPtr

      io.Process2Trace.Kid := kid
      io.Process2Trace.ScanningInYoung := destRegionAttr(15 downto 8) === U(Type_Young, 8 bits)

      io.Process2Trace.ArrayLength := arrayLength
      io.Process2Trace.PartialArrayStart := U(0)
      io.Process2Trace.StepIndex := arrayLength % chunkSize
      io.Process2Trace.StepNCreate := Mux(arrayLength > (arrayLength % chunkSize), U(1), U(0)).resized

      val copyFire = io.Process2Copy.Valid && io.Process2Copy.Ready
      val traceFire = io.Process2Trace.Valid && io.Process2Trace.Ready

      when(copyFire){
        issuedCopy := True
      }

      when(traceFire){
        issuedTrace := True
      }

      val copyIssuedDone  = copyFire || issuedCopy
      val traceIssuedDone = traceFire || issuedTrace || !needTrace

      when(copyIssuedDone && traceIssuedDone) {
        issuedCopy  := False
        issuedTrace := False
        state := overall_state.s_waitDone
      }
    }

    is(overall_state.s_waitDone){
      val needTrace = kid =/= TypeArrayKlassID
      val copyFinished  = copyDone  || io.Process2Copy.Done
      val traceFinished = traceDone || io.Process2Trace.Done || !needTrace

      when(io.Process2Copy.Done) {
        copyDone := True
      }

      when(io.Process2Trace.Done) {
        traceDone := True
      }


      when(specialWriteDone && copyFinished && traceFinished) {
        copyDone  := False
        traceDone := False

        io.Process2CopySurvivor.Done := True
        io.Process2CopySurvivor.DestOopPtr := destOopPtr

        state := overall_state.s_idle
      }
    }
  }

  when(state === overall_state.s_doCopyAndTrace || state === overall_state.s_waitDone){
    when(!completedWriteSrcMW){
      val srcMW = (destOopPtr & ~U(LOCK_MASK_IN_PLACE, MMUAddrWidth bits)) | U(MARKED_VALUE, MMUAddrWidth bits)
      issueReq(io.SpecialMreq(0), srcOopPtr + U(MarkWordOff), True, allBytesOnes, srcMW, writeSrcMW_issued){ rd =>
        completedWriteSrcMW := True
      }
    }

    when(!completedWriteDestMW){
      val hasChangedMW = destRegionAttr(15 downto 8) === U(Type_Young, 8 bits) && (markWord & U(UNLOCKED_VALUE, MMUDataWidth bits)) === U(0)
      val oldMarkChanged = (markWord & ~U(AGE_MASK_IN_PLACE, MMUDataWidth bits)) | Mux(age < U(AGE_MASK), age + U(1), age).resize(MMUDataWidth bits) & (U(AGE_MASK) << U(AGE_SHIFT)).resize(MMUDataWidth bits)
      val destMW = Mux(hasChangedMW, oldMarkChanged, markWord)
      issueReq(io.SpecialMreq(1), destOopPtr + U(MarkWordOff), True, allBytesOnes, destMW, writeDestMW_issued){ rd =>
        completedWriteDestMW := True
      }
    }

    val hasWriteMonitorPtr = destRegionAttr === U(Type_Young, 8 bits) && (markWord & U(UNLOCKED_VALUE, MMUDataWidth bits)) === U(0)
    when(hasWriteMonitorPtr && !completedWriteMonitorPtr){
      when(writeMonitor_state === partitial_state.s_0){
        issueReq(io.SpecialMreq(2), ptr, False, U(0), U(0), monitorPtr_issued){ rd =>
          new_mark := rd
          writeMonitor_state := partitial_state.s_1
        }
      }.elsewhen(writeMonitor_state === partitial_state.s_1){
        val newMarkChanged = (new_mark & ~U(AGE_MASK_IN_PLACE, MMUDataWidth bits)) | Mux(age < U(AGE_MASK), age + U(1), age).resize(MMUDataWidth bits) & (U(AGE_MASK) << U(AGE_SHIFT)).resize(MMUDataWidth bits)
        issueReq(io.SpecialMreq(2), ptr, True, allBytesOnes, newMarkChanged, monitorPtr_issued) { rd =>
          completedWriteMonitorPtr := True
          writeMonitor_state := partitial_state.s_0
        }
      }
    }.otherwise{
      completedWriteMonitorPtr := True
    }

    when(!completedUpdatedYoungWordsBase){
      when(updatedYoungWordsBase_state === partitial_state.s_0){
        issueReq(io.SpecialMreq(3), (heapRegionBiasedBase + (srcOopPtr >> heapRegionShiftBy) * U(GCObjectPtr_Size)).resized, False, U(0), U(0), updatedYoungWordsBase_issued){ rd =>
          from_region := rd
          updatedYoungWordsBase_state := partitial_state.s_1
        }
      }
      when(updatedYoungWordsBase_state === partitial_state.s_1){
        issueReq(io.SpecialMreq(3), (from_region + U(YOUND_INDEX_IN_CSET_OFFSET)).resized, False, U(0), U(0), updatedYoungWordsBase_issued){ rd =>
          young_index := rd(31 downto 0)
          updatedYoungWordsBase_state := partitial_state.s_2
        }
      }.elsewhen(updatedYoungWordsBase_state === partitial_state.s_2){
        issueReq(io.SpecialMreq(3), (pss + U(SURVIVING_YOUNG_WORDS_BASE_OFFSET)).resized, False, U(0), U(0), updatedYoungWordsBase_issued){ rd =>
          youngWordsBase := rd
          updatedYoungWordsBase_state := partitial_state.s_3
        }
      }.elsewhen(updatedYoungWordsBase_state === partitial_state.s_3){
        issueReq(io.SpecialMreq(3), (youngWordsBase + young_index * U(GCObjectPtr_Size)).resized, False, U(0), U(0), updatedYoungWordsBase_issued){ rd =>
          originValue := rd
          updatedYoungWordsBase_state := partitial_state.s_4
        }
      }.elsewhen(updatedYoungWordsBase_state === partitial_state.s_4){
        issueReq(io.SpecialMreq(3), (youngWordsBase + young_index * U(GCObjectPtr_Size)).resized, True, allBytesOnes, originValue + size, updatedYoungWordsBase_issued){ rd =>
          updatedYoungWordsBase_state := partitial_state.s_0
          completedUpdatedYoungWordsBase := True
        }
      }
    }

    when(kid === ObjectArrayKlassID){
      when(!completedWriteDestLength){
        issueReq(io.SpecialMreq(4), destOopPtr + U(ArrayLenOff), True, halfBytesOnes, (arrayLength % chunkSize).resized, writeDestLengthIssued){ rd =>
          completedWriteDestLength := True
        }
      }
    }.otherwise{
      completedWriteDestLength := True
    }
  }

  val reqIssued = RegInit(False)
  val (want, addr, isWrite, wmask, wdata) = mmuOpForState()
  when(want){
    issueReq(io.CommonMreq, addr.resized, isWrite, wmask.resized, wdata.resized, reqIssued) { rd =>
      mmuResponseHandler(rd)
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}
