package hwgc

import spinal.core._
import spinal.lib._
import scala.language.postfixOps

class GCOopCopy2Survivor extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val CommonMreq = master(new LocalMMUIO)
    val SpecialMreq = Vec.fill(4)(new LocalMMUIO)
    val Process2Copy = master(new GCProcess2Copy)
    val Process2Trace = master(new GCProcess2Trace)
    val Process2CopySurvivor = slave(new GCProcess2Survivor)
    val ConfigIO = slave(new GCCopy2SurvivorConfigIO)
    val DebugTimeStamp = in(UInt(MMUDataWidth bits))
    SpecialMreq.foreach(_.asMaster)
  }

  // default value
  io.CommonMreq.Request.valid := False
  io.CommonMreq.Request.payload.clearAll()
  io.CommonMreq.Response.ready := False

  for(i <- 0 until 4){
    io.SpecialMreq(i).Request.valid := False
    io.SpecialMreq(i).Request.payload.clearAll()
    io.SpecialMreq(i).Response.ready := False
  }

  io.Process2Copy.Valid := False
  io.Process2Copy.SrcOopPtr := U(0)
  io.Process2Copy.DestOopPtr := U(0)
  io.Process2Copy.Size := U(0)

  io.Process2CopySurvivor.DestOopPtr := U(0)
  io.Process2CopySurvivor.Done := False
  io.Process2CopySurvivor.Ready := False

  io.Process2Trace.Valid := False
  io.Process2Trace.Kid := U(0)
  io.Process2Trace.OopType := U(0)
  io.Process2Trace.KlassPtr := U(0)
  io.Process2Trace.SrcOopPtr := U(0)
  io.Process2Trace.DestOopPtr := U(0)
  io.Process2Trace.ScanningInYoung := False
  io.Process2Trace.StepIndex := U(0)
  io.Process2Trace.StepNCreate := U(0)
  io.Process2Trace.ArrayLength := U(0)
  io.Process2Trace.PartialArrayStart := U(0)

  object overall_state extends SpinalEnum {
    val s_idle, s_readKlassPtr, s_readLhKid, s_getSize, s_getAge, s_readDestAttr, s_getBuffer, s_getObjPtr, s_slowAllocate, s_doCopyAndTrace, s_waitDone = newElement()
  }

  object partitial_state extends SpinalEnum {
    val s_0, s_1, s_2 = newElement()
  }

  val state = RegInit(overall_state.s_idle)
  val sub_state = RegInit(partitial_state.s_0)

  val pss = RegInit(U(0, MMUAddrWidth bits))
  val chunkSize = RegInit(U(0, 32 bits))
  val ageThreshold = RegInit(U(0, 32 bits))
  val youngWordsBase = RegInit(U(0, MMUAddrWidth bits))
  val plabAllocatorPtr = RegInit(U(0, MMUAddrWidth bits))
  val heapRegionShiftBy = RegInit(U(0, 32 bits))
  val heapRegionBiasedBase = RegInit(U(0, MMUAddrWidth bits))

  val markWord = RegInit(U(0, MMUDataWidth bits))
  val srcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val regionAttrPtr = RegInit(U(0, MMUAddrWidth bits))

  val klass_ptr = RegInit(U(0, MMUAddrWidth bits))
  val lh = RegInit(U(0, 32 bits))
  val kid = RegInit(U(0, 32 bits))
  val arrayLength = RegInit(U(0, 32 bits))
  val size = RegInit(U(0, 32 bits))
  val srcRegionAttr = RegInit(U(0, 16 bits))
  val age = RegInit(U(0, 32 bits))
  val new_mark = RegInit(U(0, MMUDataWidth bits))
  val dest_attr_ptr = RegInit(U(0, MMUAddrWidth bits))
  val destRegionAttr = RegInit(U(0, 16 bits))
  val bufferPtr = RegInit(U(0, MMUAddrWidth bits))
  val buffer = RegInit(U(0, MMUAddrWidth bits))
  val region_top = RegInit(U(0, MMUAddrWidth bits))
  val region_end = RegInit(U(0, MMUAddrWidth bits))
  val destOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val from_region = RegInit(U(0, MMUAddrWidth bits))

  val completedReadHR = RegInit(False)
  val completedReadMW = RegInit(False)
  val completedReadArrayLen = RegInit(False)
  val completedReadRegionAttr = RegInit(False)

  val issueCommonMreq = RegInit(False)
  val issueSpecialMreq = Vec.fill(4)(RegInit(False))


  when(state === overall_state.s_idle){
    completedReadHR := False
    completedReadMW := False
    completedReadArrayLen := False
    completedReadRegionAttr := False

    io.Process2CopySurvivor.Ready := True
    when(io.Process2CopySurvivor.Valid && io.Process2CopySurvivor.Ready){
      srcOopPtr := io.Process2CopySurvivor.SrcOopPtr
      markWord := io.Process2CopySurvivor.MarkWord
      regionAttrPtr := io.Process2CopySurvivor.RegionAttrPtr

      pss := io.ConfigIO.ParScanThreadStatePtr
      chunkSize := io.ConfigIO.ChunkSize
      ageThreshold := io.ConfigIO.AgeThreshold
      youngWordsBase := io.ConfigIO.YoungWordsBase
      plabAllocatorPtr := io.ConfigIO.PlabAllocatorPtr
      heapRegionShiftBy := io.ConfigIO.HeapRegionShiftBy
      heapRegionBiasedBase := io.ConfigIO.HeapRegionBiasedBase

      state := overall_state.s_readKlassPtr

      if(DebugEnable){
        report(Seq(
          "[GCOopCopy2Survivor<", io.DebugTimeStamp,
          ">]Receive task from OopProcess",
          "\n"
        ))
      }
    }
  }

  when(state === overall_state.s_readKlassPtr){
    val addr = (srcOopPtr + U(KlassOff)).resize(MMUAddrWidth bits)
    issueReq(io.CommonMreq, addr, False, U(0), U(0), issueCommonMreq){ rd =>
      klass_ptr := rd
      state := overall_state.s_readLhKid
    }
  }

  when(state === overall_state.s_readLhKid){
    val addr = (klass_ptr + U(LhKidOff)).resize(MMUAddrWidth bits)
    issueReq(io.CommonMreq, addr, False, U(0), U(0), issueCommonMreq){ rd =>
      lh := rd(31 downto 0)
      kid := rd(63 downto 32)
      state := overall_state.s_getSize
    }
  }

  when(state === overall_state.s_getSize){
    val sizeComputedReady = lh.asSInt > 0 || completedReadArrayLen || io.SpecialMreq(0).Response.valid
    when(sizeComputedReady){
      when(completedReadRegionAttr || io.SpecialMreq(1).Response.valid){
        state := overall_state.s_getAge
      }
    }
    when(lh.asSInt > 0){
      size := (lh >> LogHeapWordSize).resize(32 bits)
    }.elsewhen(completedReadArrayLen || io.SpecialMreq(0).Response.valid){
      val arrayLen = Mux(completedReadArrayLen, arrayLength, io.SpecialMreq(0).Response.payload.ResponseData(31 downto 0))
      val size_in_bytes = ((arrayLen << lh(7 downto 0)) + lh(23 downto 16)).resize(32 bits)
      size := Mux(size_in_bytes(LogHeapWordSize - 1 downto 0) =/= U(0), (size_in_bytes >> U(LogHeapWordSize)) + U(1), size_in_bytes >> U(LogHeapWordSize)).resize(32 bits)
    }
  }

  val hasMonitor = (markWord & U(MONITOR_VALUE, MMUDataWidth bits)) =/= U(0, MMUDataWidth bits)
  val ptr = Mux(hasMonitor, markWord ^ U(MONITOR_VALUE, MMUDataWidth bits), markWord)
  when(state === overall_state.s_getAge){
    dest_attr_ptr := (pss + U(REGION_ATTR_DEST_OFFSET) + srcRegionAttr(15 downto 8) * U(GCHeapRegionAttr_Size)).resize(MMUAddrWidth bits)
    val isYoungRegion = srcRegionAttr(15 downto 8) === U(Type_Young, 8 bits)
    val markWordReady = !hasMonitor || completedReadMW || io.SpecialMreq(2).Response.valid

    when(isYoungRegion && markWordReady){
      // 统一获取 mark word
      val effectiveMark = Mux(hasMonitor && completedReadMW, new_mark,
        Mux(hasMonitor, io.SpecialMreq(2).Response.payload.ResponseData,
          markWord))
      val extractedAge = ((effectiveMark >> U(AGE_SHIFT)) & U(AGE_MASK)).resize(32 bits)
      age := extractedAge

      when(extractedAge < ageThreshold){
        dest_attr_ptr := regionAttrPtr
        destRegionAttr := srcRegionAttr
        state := overall_state.s_getBuffer
      }.otherwise{
        state := overall_state.s_readDestAttr
      }
    }.elsewhen(!isYoungRegion){
      age := U(0)
      state := overall_state.s_readDestAttr
    }
  }

  when(state === overall_state.s_readDestAttr){
    issueReq(io.CommonMreq, dest_attr_ptr, False, U(0), U(0), issueCommonMreq) { rd =>
      destRegionAttr := rd(15 downto 0)
      state := overall_state.s_getBuffer
    }
  }

  val bufferPtrValid = RegInit(False)
  when(state === overall_state.s_getBuffer){
    when(!bufferPtrValid) {
      val addr = (plabAllocatorPtr + U(ALLOC_BUFFERS_OFFSET) + destRegionAttr(15 downto 8) * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
      issueReq(io.CommonMreq, addr, False, U(0), U(0), issueCommonMreq) { rd =>
        bufferPtr := rd
        bufferPtrValid := True
      }
    }.elsewhen(bufferPtrValid){
      issueReq(io.CommonMreq, bufferPtr, False, U(0), U(0), issueCommonMreq){ rd =>
        buffer := rd
        bufferPtrValid := False
        state := overall_state.s_getObjPtr
      }
    }
  }

  val completedReadTop = RegInit(False)
  val completedReadEnd = RegInit(False)
  when(state === overall_state.s_getObjPtr){
    when(!completedReadEnd && completedReadArrayLen){
      issueReq(io.SpecialMreq(0), (buffer + U(REGION_END_OFFSET)).resize(MMUAddrWidth bits), False, U(0), U(0), issueSpecialMreq(0)) { rd =>
        completedReadEnd := True
        region_end := rd
      }
    }
    when(!completedReadTop && completedReadRegionAttr){
      issueReq(io.SpecialMreq(1), (buffer + U(REGION_TOP_OFFSET)).resize(MMUAddrWidth bits), False, U(0), U(0), issueSpecialMreq(1)){ rd =>
        completedReadTop := True
        region_top := rd
      }
    }
    when((completedReadEnd || io.SpecialMreq(0).Response.valid) && (completedReadEnd || io.SpecialMreq(1).Response.valid)){
      completedReadTop := False
      completedReadEnd := False
      val end = Mux(completedReadEnd, region_end, io.SpecialMreq(0).Response.payload.ResponseData)
      val top = Mux(completedReadTop, region_top, io.SpecialMreq(1).Response.payload.ResponseData)
      when(((end - top) / U(GCObjectPtr_Size)).resize(32 bits) >= size){
        destOopPtr := top
        issueReq(io.CommonMreq, (buffer + U(REGION_TOP_OFFSET)).resize(MMUAddrWidth bits), True, allBytesOnes, (top + size * U(GCObjectPtr_Size)).resize(MMUDataWidth bits), issueCommonMreq){ rd =>
          state := overall_state.s_doCopyAndTrace
        }
      }.otherwise{
        state := overall_state.s_slowAllocate
      }
    }
  }

  when(state === overall_state.s_slowAllocate){
    if(DebugEnable){
      report(Seq(
        "[GCOopCopy2Survivor<", io.DebugTimeStamp,
        ">]issue interrupt call allocate_copy_slow function",
        "\n"
      ))
    }
  }

  val issuedCopy = RegInit(False)
  val issuedTrace = RegInit(False)
  when(state === overall_state.s_doCopyAndTrace){
    val needTrace = kid =/= U(TypeArrayKlassID, 32 bits)

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
    io.Process2Trace.StepIndex := (arrayLength % chunkSize).resize(32 bits)
    io.Process2Trace.StepNCreate := Mux(arrayLength > (arrayLength % chunkSize), U(1), U(0)).resize(32 bits)

    val copyFire = io.Process2Copy.Valid && io.Process2Copy.Ready
    val traceFire = io.Process2Trace.Valid && io.Process2Trace.Ready

    when(copyFire){
      issuedCopy := True
      if(DebugEnable){
        report(Seq(
          "[GCOopCopy2Survivor<", io.DebugTimeStamp,
          ">]send the task to copy module",
          "\n"
        ))
      }
    }

    when(traceFire){
      issuedTrace := True
      if(DebugEnable){
        report(Seq(
          "[GCOopCopy2Survivor<", io.DebugTimeStamp,
          ">]send the task to trace module",
          "\n"
        ))
      }
    }

    val copyIssuedDone  = copyFire || issuedCopy
    val traceIssuedDone = traceFire || issuedTrace || !needTrace

    when(copyIssuedDone && traceIssuedDone) {
      issuedCopy  := False
      issuedTrace := False
      state := overall_state.s_waitDone
    }
  }

  val copyDone = RegInit(False)
  val traceDone = RegInit(False)
  val completedWriteSrcMW = RegInit(False)
  val completedWriteDestMW = RegInit(False)
  val completedWriteDestLength = RegInit(False)
  val completedWriteMonitorPtr = RegInit(False)
  val completedUpdatedYoungWordsBase = RegInit(False)
  val specialWriteDone = completedWriteSrcMW && completedWriteDestMW && completedWriteMonitorPtr && completedUpdatedYoungWordsBase && completedWriteDestLength
  when(state === overall_state.s_waitDone){
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
      if(DebugEnable){
        report(Seq(
          "[GCOopCopy2Survivor<", io.DebugTimeStamp,
          ">]the copy2survivor has done",
          "\n"
        ))
      }
      copyDone  := False
      traceDone := False

      completedWriteSrcMW := False
      completedWriteDestMW := False
      completedWriteMonitorPtr := False
      completedUpdatedYoungWordsBase := False
      completedWriteDestLength := False

      io.Process2CopySurvivor.Done := True
      io.Process2CopySurvivor.DestOopPtr := destOopPtr

      state := overall_state.s_idle
    }
  }

  when(state =/= overall_state.s_idle){
    when(!completedReadArrayLen){
      val addr = (srcOopPtr + U(ArrayLenOff)).resize(MMUAddrWidth bits)
      issueReq(io.SpecialMreq(0), addr, False, U(0), U(0), issueSpecialMreq(0)){ rd =>
        arrayLength := rd(31 downto 0)
        completedReadArrayLen := True
      }
    }
    when(!completedReadRegionAttr){
      issueReq(io.SpecialMreq(1), regionAttrPtr, False, U(0), U(0), issueSpecialMreq(1)){ rd =>
        srcRegionAttr := rd(15 downto 0)
        completedReadRegionAttr := True
      }
    }
    when(!completedReadMW){
      issueReq(io.SpecialMreq(2), ptr, False, U(0), U(0), issueSpecialMreq(2)){ rd =>
        new_mark := rd
        completedReadMW := True
      }
    }
    when(!completedReadHR){
      val addr = (heapRegionBiasedBase + (srcOopPtr >> heapRegionShiftBy) * U(GCObjectPtr_Size)).resize(MMUDataWidth bits)
      issueReq(io.SpecialMreq(3), addr, False, U(0), U(0), issueSpecialMreq(3)){ rd =>
        from_region := rd
        completedReadHR := True
      }
    }
  }

  val young_index = RegInit(U(0, 32 bits))
  val originValue = RegInit(U(0, MMUDataWidth bits))
  val writeMonitor_state = RegInit(partitial_state.s_0)
  val updatedYoungWordsBase_state = RegInit(partitial_state.s_0)
  when(state === overall_state.s_doCopyAndTrace || state === overall_state.s_waitDone){
    when(!completedWriteSrcMW && completedReadArrayLen){
      val srcMW = (destOopPtr & ~U(LOCK_MASK_IN_PLACE, MMUAddrWidth bits)) | U(MARKED_VALUE, MMUAddrWidth bits)
      issueReq(io.SpecialMreq(0), srcOopPtr + U(MarkWordOff), True, allBytesOnes, srcMW, issueSpecialMreq(0)){ rd =>
        completedWriteSrcMW := True
      }
    }

    when(!completedWriteDestMW && completedReadRegionAttr){
      val hasChangedMW = destRegionAttr(15 downto 8) === U(Type_Young, 8 bits) && (markWord & U(UNLOCKED_VALUE, MMUDataWidth bits)) === U(0)
      val oldMarkChanged = (markWord & ~U(AGE_MASK_IN_PLACE, MMUDataWidth bits)) | Mux(age < U(AGE_MASK), age + U(1), age).resize(MMUDataWidth bits) & (U(AGE_MASK) << U(AGE_SHIFT)).resize(MMUDataWidth bits)
      val destMW = Mux(hasChangedMW, oldMarkChanged, markWord)
      issueReq(io.SpecialMreq(1), destOopPtr + U(MarkWordOff), True, allBytesOnes, destMW, issueSpecialMreq(1)){ rd =>
        completedWriteDestMW := True
      }
    }

    val hasWriteMonitorPtr = destRegionAttr === U(Type_Young, 8 bits) && (markWord & U(UNLOCKED_VALUE, MMUDataWidth bits)) === U(0)
    when(hasWriteMonitorPtr && !completedWriteMonitorPtr && completedReadMW){
      when(writeMonitor_state === partitial_state.s_0){
        issueReq(io.SpecialMreq(2), ptr, False, U(0), U(0), issueSpecialMreq(2)){ rd =>
          new_mark := rd
          writeMonitor_state := partitial_state.s_1
        }
      }.elsewhen(writeMonitor_state === partitial_state.s_1){
        val newMarkChanged = (new_mark & ~U(AGE_MASK_IN_PLACE, MMUDataWidth bits)) | Mux(age < U(AGE_MASK), age + U(1), age).resize(MMUDataWidth bits) & (U(AGE_MASK) << U(AGE_SHIFT)).resize(MMUDataWidth bits)
        issueReq(io.SpecialMreq(2), ptr, True, allBytesOnes, newMarkChanged, issueSpecialMreq(2)) { rd =>
          completedWriteMonitorPtr := True
          writeMonitor_state := partitial_state.s_0
        }
      }
    }.otherwise{
      completedWriteMonitorPtr := True
    }

    when(!completedUpdatedYoungWordsBase && completedReadHR){
      when(updatedYoungWordsBase_state === partitial_state.s_0){
        issueReq(io.SpecialMreq(3), (from_region + U(YOUND_INDEX_IN_CSET_OFFSET)).resized, False, U(0), U(0), issueSpecialMreq(3)){ rd =>
          young_index := rd(31 downto 0)
          updatedYoungWordsBase_state := partitial_state.s_1
        }
      }.elsewhen(updatedYoungWordsBase_state === partitial_state.s_1){
        issueReq(io.SpecialMreq(3), (youngWordsBase + young_index * U(GCObjectPtr_Size)).resized, False, U(0), U(0), issueSpecialMreq(3)){ rd =>
          originValue := rd
          updatedYoungWordsBase_state := partitial_state.s_2
        }
      }.elsewhen(updatedYoungWordsBase_state === partitial_state.s_2){
        issueReq(io.SpecialMreq(3), (youngWordsBase + young_index * U(GCObjectPtr_Size)).resized, True, allBytesOnes, originValue + size, issueSpecialMreq(3)){ rd =>
          updatedYoungWordsBase_state := partitial_state.s_0
          completedUpdatedYoungWordsBase := True
        }
      }
    }

    when(kid === ObjectArrayKlassID){
      when(!completedWriteDestLength){
        issueReq(io.CommonMreq, destOopPtr + U(ArrayLenOff), True, halfBytesOnes, (arrayLength % chunkSize).resized, issueCommonMreq){ rd =>
          completedWriteDestLength := True
        }
      }
    }.otherwise{
      completedWriteDestLength := True
    }
  }
}

object GCOopCopy2SurvivorVerilog extends App {
  Config.spinal.generateVerilog(new GCOopCopy2Survivor())
}