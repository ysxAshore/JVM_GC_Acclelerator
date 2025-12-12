package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

case class TaskData() extends Bundle with GCParameters with HWParameters{
  val src = UInt(MMUAddrWidth bits)
  val dest = UInt(MMUAddrWidth bits)
  val scanning_in_young = Bool()
}

case class StageData() extends Bundle with GCParameters with HWParameters {
  val src = UInt(MMUAddrWidth bits)
  val dest = UInt(MMUAddrWidth bits)
  val obj = UInt(MMUAddrWidth bits)
  val scanning_in_young = Bool()
  val regionAttr = UInt(16 bits)
}

class GCTrace extends Module with GCParameters with HWParameters{
  val io = new Bundle {
    val ToTrace = slave(new GCProcess2Trace)
    val Trace2Aop = master(new AopParameters)
    val TraceMMUIO = master(new TraceMReq2MMU(GCoopWorkStages))
    val Trace2Stack = master Stream UInt(MMUAddrWidth bits)
    val ConfigIO = slave(new GCTraceConfigIO)
    val DebugTimeStampe = in UInt(MMUDataWidth bits)
  }

  // default value
  io.ToTrace.Ready := False
  io.ToTrace.Done := False

  io.Trace2Aop.Valid := False
  io.Trace2Aop.ParScanThreadStatePtr := U(0)
  io.Trace2Aop.CardTablePtr := U(0)
  io.Trace2Aop.RegionAttr := U(0)
  io.Trace2Aop.Task := U(0)

  for(i <- 0 until GCoopWorkStages){
    io.TraceMMUIO.oopWorkMReqs(i).Request.valid := False
    io.TraceMMUIO.oopWorkMReqs(i).Request.payload.clearAll()
    io.TraceMMUIO.oopWorkMReqs(i).Response.ready := False
  }
  io.TraceMMUIO.commonMReq.Request.valid := False
  io.TraceMMUIO.commonMReq.Request.payload.clearAll()
  io.TraceMMUIO.commonMReq.Response.ready := False

  io.Trace2Stack.valid := False
  io.Trace2Stack.payload.clearAll()

  // State Machine
  object overall_state extends SpinalEnum {
    val s_idle, s_arrayPush, s_arrayTrace, s_refKlassTrace, s_staticTrace, s_oopTrace, s_end = newElement()
  }
  object sub_state extends SpinalEnum{
    val s_0, s_1, s_2, s_3 = newElement()
  }

  // helper: pushStack
  def pushFifo(src: UInt, dest: UInt, scanning_in_young:Bool, issued: Bool): Bool = {
    traceTaskFifo.io.push.valid := False
    when(!issued) {
      traceTaskFifo.io.push.valid := True
      traceTaskFifo.io.push.payload.src := src.resize(MMUAddrWidth bits)
      traceTaskFifo.io.push.payload.dest := dest.resize(MMUAddrWidth bits)
      traceTaskFifo.io.push.scanning_in_young := scanning_in_young
      issued := True
    }
    val pushed = traceTaskFifo.io.push.fire
    pushed
  }

  val state = RegInit(overall_state.s_idle)

  val CardTablePtr = RegInit(U(0, MMUAddrWidth bits))
  val RegionAttrBase = RegInit(U(0, MMUAddrWidth bits))
  val RegionAttrShiftBy = RegInit(U(0, 32 bits))
  val RegionAttrBiasedBase = RegInit(U(0, MMUAddrWidth bits))
  val HeapRegionBias = RegInit(U(0, 32 bits))
  val HeapRegionShiftBy = RegInit(U(0, 32 bits))
  val LogOfHRGrainBytes = RegInit(U(0, 32 bits))
  val ParScanThreadStatePtr = RegInit(U(0, MMUAddrWidth bits))
  val HumongousReclaimCandidatesBoolBase = RegInit(U(0, MMUAddrWidth bits))
  val Kid = RegInit(U(0, 32 bits))
  val KlassPtr = RegInit(U(0, MMUAddrWidth bits))
  val SrcOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val DestOopPtr = RegInit(U(0, MMUAddrWidth bits))
  val ScanningInYoung = RegInit(False)
  val StepIndex = RegInit(U(0, 32 bits))
  val StepNCreate = RegInit(U(0, 32 bits))
  val ArrayLength = RegInit(U(0, 32 bits))
  val PartialArrayStart = RegInit(U(0, 32 bits))

  when(state === overall_state.s_idle){
    io.ToTrace.Ready := True
    when(io.ToTrace.Valid && io.ToTrace.Ready){
      io.ToTrace.Done := False

      CardTablePtr := io.ConfigIO.CardTablePtr
      RegionAttrBase := io.ConfigIO.RegionAttrBase
      RegionAttrShiftBy := io.ConfigIO.RegionAttrShiftBy
      RegionAttrBiasedBase:= io.ConfigIO.RegionAttrBiasedBase
      HeapRegionBias := io.ConfigIO.HeapRegionBias
      HeapRegionShiftBy := io.ConfigIO.HeapRegionShiftBy
      LogOfHRGrainBytes := io.ConfigIO.LogOfHRGrainBytes
      ParScanThreadStatePtr := io.ConfigIO.ParScanThreadStatePtr
      HumongousReclaimCandidatesBoolBase := io.ConfigIO.HumongousReclaimCandidatesBoolBase

      Kid := io.ToTrace.Kid
      KlassPtr := io.ToTrace.KlassPtr
      SrcOopPtr := io.ToTrace.SrcOopPtr
      DestOopPtr := io.ToTrace.DestOopPtr
      ScanningInYoung := io.ToTrace.ScanningInYoung
      StepIndex := io.ToTrace.StepIndex
      StepNCreate := io.ToTrace.StepNCreate
      ArrayLength := io.ToTrace.ArrayLength
      PartialArrayStart := Mux(io.ToTrace.OopType === U(PartialArrayOop), io.ToTrace.PartialArrayStart, U(0))

      // typeArrayKid not enter to trace module
      when(io.ToTrace.OopType === U(PartialArrayOop) || io.ToTrace.Kid === U(ObjectArrayKlassID)){
        state := overall_state.s_arrayPush
      }.otherwise {
        state := overall_state.s_oopTrace
      }

      if(DebugEnable){
          report(Seq(
            "[GCTrace<", io.DebugTimeStampe,
            ">] Receive GCTrace Task",
            ", CardTablePtr = ", io.ConfigIO.CardTablePtr,
            ", RegionAttrBase = ", io.ConfigIO.RegionAttrBase,
            ", RegionAttrShiftBy = ", io.ConfigIO.RegionAttrShiftBy,
            ", RegionAttrBiasedBase = ", io.ConfigIO.RegionAttrBiasedBase,
            ", HeapRegionBias = ", io.ConfigIO.HeapRegionBias,
            ", HeapRegionShiftBy = ", io.ConfigIO.HeapRegionShiftBy,
            ", LogOfHRGrainBytes = ", io.ConfigIO.LogOfHRGrainBytes,
            ", ParScanThreadStatePtr = ", io.ConfigIO.ParScanThreadStatePtr,
            ", HumongousReclaimCandidatesBoolBase = ", io.ConfigIO.HumongousReclaimCandidatesBoolBase,
            ", OopType = ", io.ToTrace.OopType,
            ", KlassPtr = ", io.ToTrace.KlassPtr,
            ", SrcOopPtr = ", io.ToTrace.SrcOopPtr,
            ", DestOopPtr = ", io.ToTrace.DestOopPtr,
            ", Kid = ", io.ToTrace.Kid, "\n",
            ", ArrayLength = ", io.ToTrace.ArrayLength,
            ", PartialArrayStart = ", io.ToTrace.PartialArrayStart,
            ", StepIndex = ", io.ToTrace.StepIndex,
            ", StepNCreate = ", io.ToTrace.StepNCreate,
            ", next state is = ", Mux(io.ToTrace.OopType === U(PartialArrayOop) || io.ToTrace.Kid === U(ObjectArrayKlassID), overall_state.s_arrayPush, overall_state.s_oopTrace),
            "\n"
          ))
      }
    }
  }

  val for_counter = RegInit(U(0, 32 bits))
  when(state === overall_state.s_arrayPush){
    when(for_counter === StepNCreate){
      state := overall_state.s_arrayTrace
    }.otherwise{
      io.Trace2Stack.valid := True
      io.Trace2Stack.payload := SrcOopPtr + U(PartialArrayTag)
      when(io.Trace2Stack.fire){
        for_counter := for_counter + U(1)
        if(DebugEnable){
          report(Seq(
            "[GCTrace<", io.DebugTimeStampe,
            ">]Push task ", SrcOopPtr + U(PartialArrayTag),
            " to GCTaskStack",
            ", the counter is ", for_counter,
            "\n"
          ))
        }
      }
    }
  }

  val traceTaskFifo = StreamFifo(
    dataType = TaskData(),
    depth = 16
  )
  traceTaskFifo.io.push.valid := False
  traceTaskFifo.io.push.payload.clearAll()

  val doOopWork = new Area{
    // MMU req and resp
    case class Stage() extends Area{
      val valid = RegInit(False)
      val reg = RegInit(StageData().getZero)
      val reqIssued = RegInit(False)
      val reqDone = RegInit(False)
      val responseData = RegInit(U(0, MMUDataWidth bits))
    }

    val stages = Seq.fill(GCoopWorkStages)(Stage())

    traceTaskFifo.io.pop.ready := !stages(0).valid
    when(traceTaskFifo.io.pop.fire){
      stages(0).valid := True
      stages(0).reg.src := traceTaskFifo.io.pop.payload.src
      stages(0).reg.dest := traceTaskFifo.io.pop.payload.dest
      stages(0).reg.scanning_in_young := traceTaskFifo.io.pop.payload.scanning_in_young
      stages(0).reg.obj := U(0)
      stages(0).reg.regionAttr := U(0)
      if(DebugEnable){
        report(Seq(
          "[GCTrace<", io.DebugTimeStampe,
          ">]Receive task from traceTaskFifo",
          ", the fifo usage is ", traceTaskFifo.io.occupancy,
          "\n"
        ))
      }
    }

    // helper to compute MMU op properties per stage index (pure combinational)
    // return (want, addr, isWrite, wmask, wdata)
    def mmuOpForIndex(i: Int, s: StageData): (Bool, UInt, Bool, UInt, UInt) = {
      val delta = s.obj - (HeapRegionBias << HeapRegionShiftBy(5 downto 0)).resize(MMUAddrWidth bits)
      val regionIndex = (delta >> LogOfHRGrainBytes).resize(MMUAddrWidth bits)
      i match {
        case 0 => (True, s.src, False, U(0), U(0))
        case 1 => (True, (RegionAttrBiasedBase + (s.obj >> RegionAttrShiftBy) * U(GCHeapRegionAttr_Size)).resize(MMUAddrWidth bits), False, U(0), U(0))
        case 3 => (True, (HumongousReclaimCandidatesBoolBase + regionIndex).resize(MMUAddrWidth bits), False, U(0), U(0))
        case 4 => (True, (HumongousReclaimCandidatesBoolBase + regionIndex).resize(MMUAddrWidth bits), True, oneByteOnes, U(0))
        case 5 => (True, (RegionAttrBase + regionIndex * U(GCHeapRegionAttr_Size) + U(TypeOffSet)).resize(MMUAddrWidth bits), True, U(1), S(Type_NoInCset, MMUDataWidth bits).asUInt) // U(-1) literal value is negative and cannot be represented -> S(-1, bits).asUInt
        case _ =>  (False, U(0), False, U(0), U(0))
      }
    }

    def resetStage(i: Int): Unit = {
      stages(i).valid := False
      stages(i).reqDone := False
      stages(i).reqIssued := False
    }

    def transfer(from: Int, to: Int, changeObj: Option[UInt] = None, changeRegionAttr: Option[UInt] = None): Unit = {
      val sFrom = stages(from)
      val sTo   = stages(to)

      resetStage(from)

      sTo.valid := True
      sTo.reg.src := sFrom.reg.src
      sTo.reg.dest := sFrom.reg.dest
      sTo.reg.obj := changeObj.getOrElse(sFrom.reg.obj)
      sTo.reg.regionAttr := changeRegionAttr.getOrElse(sFrom.reg.regionAttr)
      sTo.reg.scanning_in_young := sFrom.reg.scanning_in_young
    }

    def advance(from: Int, to: Int, changeObj: Option[UInt] = None, changeRegionAttr: Option[UInt] = None): Unit = {
      val sFrom = stages(from)
      val sTo   = stages(to)

      when(sFrom.valid && sFrom.reqDone && !sTo.valid) {
        transfer(from, to, changeObj, changeRegionAttr)
      }
    }

    // send and save response
    for(i <- 0 until GCoopWorkStages){
      val s = stages(i)
      val mreq = io.TraceMMUIO.oopWorkMReqs(i)
      val (want, addr, isWrite, wmask, wdata) = mmuOpForIndex(i, s.reg)
      when(s.valid && !s.reqDone && want) {
        issueReq(mreq, addr.resized, isWrite, wmask.resized, wdata.resized, s.reqIssued) { rd =>
          s.reqDone := True
          s.responseData := rd
        }
      }
    }

    // stage0 -> stage1: determined transfer 1 or not by responseData
    when(stages(0).valid && stages(0).reqDone){
      val s0 = stages(0)
      when(s0.responseData === U(0)){
        resetStage(0)
      }elsewhen(!stages(1).valid){
        transfer(0, 1, changeObj = Some(s0.responseData))
      }
    }

    // stage1 -> stage2: determined to stage
    when(stages(1).valid && stages(1).reqDone){
      val s1 = stages(1)
      val regionAttrType = s1.responseData(15 downto 8)
      when(regionAttrType.asSInt >= S(Type_Young, 8 bits)){
        when(!stages(2).valid){
          transfer(1, 2, changeRegionAttr = Some(s1.responseData(15 downto 0)))
        }
      }.elsewhen((s1.reg.dest ^ s1.reg.obj) >> LogOfHRGrainBytes =/= 0){
        when(regionAttrType === S(Type_Humongous, 8 bits).asUInt){
          when(!stages(3).valid){
            transfer(1, 3, changeRegionAttr = Some(s1.responseData(15 downto 0)))
          }
        }.elsewhen(s1.reg.scanning_in_young){
          resetStage(1)
        }.elsewhen(!stages(6).valid){
          transfer(1, 6, changeRegionAttr = Some(s1.responseData(15 downto 0)))
        }
      }.otherwise{
        resetStage(1)
      }
    }

    // stage2 : push taskStack
    when(stages(2).valid && stages(2).reqDone){
      io.Trace2Stack.valid := True
      io.Trace2Stack.payload := stages(2).reg.dest
      when(io.Trace2Stack.fire) {
        resetStage(2)
        if(DebugEnable){
          report(Seq(
            "[GCTrace<", io.DebugTimeStampe,
            ">]Push task ", stages(2).reg.dest,
            " to GCTaskStack",
            "\n"
          ))
        }
      }
    }

    when(stages(3).valid && stages(3).reqDone){
      val s3 = stages(3)
      when(s3.responseData(7 downto 0) === U(1, 8 bits)){
        when(!stages(4).valid){
          transfer(3, 4)
        }
      }otherwise {
        when(s3.reg.scanning_in_young){
          resetStage(3)
        }.elsewhen(!stages(6).valid){
          transfer(3, 6)
        }
      }
    }

    advance(4, 5)

    when(stages(5).valid && stages(5).reqDone){
      val s5 = stages(5)
      when(s5.reg.scanning_in_young){
        resetStage(5)
      }.elsewhen(!stages(6).valid){
        transfer(5, 6)
      }
    }

    when(stages(6).valid){
      val s6 = stages(6)
      io.Trace2Aop.Valid := True
      io.Trace2Aop.ParScanThreadStatePtr := ParScanThreadStatePtr
      io.Trace2Aop.CardTablePtr := CardTablePtr
      io.Trace2Aop.RegionAttr := s6.reg.regionAttr
      io.Trace2Aop.Task := s6.reg.dest
      when(io.Trace2Aop.Valid && io.Trace2Aop.Ready){
        resetStage(6)
        if(DebugEnable){
          report(Seq(
            "[GCTrace<", io.DebugTimeStampe,
            ">]Send the task to aop",
            "\n"
          ))
        }
      }
    }

    val anyValid = stages.map(_.valid).reduce(_ || _)
    val done = traceTaskFifo.io.occupancy === U(0) && !anyValid && io.Trace2Aop.Done
  }

  val p = RegInit(U(0, MMUAddrWidth bits))
  val q = RegInit(U(0, MMUAddrWidth bits))
  val arrayTrace_subState = RegInit(sub_state.s_0)
  val issued_arrayPush = RegInit(False)
  when(state === overall_state.s_arrayTrace){
    switch(arrayTrace_subState){
      is(sub_state.s_0){
        //assign p, q and prev_state
        val low = (DestOopPtr + U(ArrayElementOff) + PartialArrayStart * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
        val high = (DestOopPtr + U(ArrayElementOff) + StepIndex * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
        p := Mux(DestOopPtr + U(ArrayElementOff) < low, low, DestOopPtr + U(ArrayElementOff)).resize(MMUAddrWidth bits)
        q := Mux((DestOopPtr + U(ArrayElementOff) + ArrayLength * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits) > high, high, (DestOopPtr + U(ArrayElementOff) + ArrayLength * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits))

        arrayTrace_subState := sub_state.s_1
      }
      is(sub_state.s_1){
        when(p < q){
          when(pushFifo(p - DestOopPtr + SrcOopPtr, p, ScanningInYoung, issued_arrayPush)){
            p := p + U(GCObjectPtr_Size)
            issued_arrayPush := False
            if(DebugEnable){
              report(Seq(
                "[GCTrace<", io.DebugTimeStampe,
                ">]Send the task to FIFO",
                ", p is ", p,
                ", q is ", q,
                "\n"
              ))
            }
          }
        }.otherwise{
          when(doOopWork.done){
            arrayTrace_subState := sub_state.s_0
            state := overall_state.s_end
            if(DebugEnable){
              report(Seq(
                "[GCTrace<", io.DebugTimeStampe,
                ">]The oopWork has done",
                "\n"
              ))
            }
          }
        }
      }
    }
  }

  val oopTrace_subState = RegInit(sub_state.s_0)
  val vtable_len = RegInit(U(0, 32 bits))
  val start_map = RegInit(U(0, MMUAddrWidth bits))
  val end_map = RegInit(U(0, MMUAddrWidth bits))
  val mReqIssued = RegInit(False)
  val issued_oopTrace = RegInit(False)
  when(state === overall_state.s_oopTrace){
    switch(oopTrace_subState){
      is(sub_state.s_0){
        // access (KlassPtr + VTableLenOff) to get the vtableLen
        val addr = (KlassPtr + U(VTableLenOff)).resize(MMUAddrWidth bits)
        issueReq(io.TraceMMUIO.commonMReq, addr, False, U(0), U(0), mReqIssued) {rd =>
          vtable_len := rd(31 downto 0)
          oopTrace_subState := sub_state.s_1
        }
      }
      is(sub_state.s_1){
        // access (KlassPtr + NonStaticOopMapSizeOff) to get the itableLen and nonStaticOopMapSize
        val addr = (KlassPtr + U(NonstaticOopMapSizeOff)).resize(MMUAddrWidth bits)
        issueReq(io.TraceMMUIO.commonMReq, addr, False, U(0), U(0), mReqIssued) {rd =>
          start_map := (KlassPtr + U(NonstaticOopMapSizeOff) + (vtable_len + rd(63 downto 32)) * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
          end_map := (KlassPtr + U(NonstaticOopMapSizeOff) + (vtable_len + rd(63 downto 32) + rd(31 downto 0)) * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
          oopTrace_subState := sub_state.s_2
        }
      }
      is(sub_state.s_2){
        when(start_map < end_map){
          // access p and q
          val addr = (end_map - U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
          issueReq(io.TraceMMUIO.commonMReq, addr, False, U(0), U(0), mReqIssued) {rd =>
            p := (DestOopPtr + rd(31 downto 0)).resize(MMUAddrWidth bits)
            q := (DestOopPtr + rd(31 downto 0) + rd(63 downto 32) * U(GCObjectPtr_Size) - U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
            oopTrace_subState := sub_state.s_3
          }
        }.otherwise{
          when(Kid === U(InstanceMirrorKlassID)){
            oopTrace_subState := sub_state.s_0
            state := overall_state.s_staticTrace
            if(DebugEnable){
              report(Seq(
                "[GCTrace<", io.DebugTimeStampe,
                ">]The oopTrace has done",
                ", next to staticTrace",
                "\n"
              ))
            }
          }.elsewhen(Kid === U(InstanceRefKlassID)){
            oopTrace_subState := sub_state.s_0
            state := overall_state.s_refKlassTrace
            if(DebugEnable){
              report(Seq(
                "[GCTrace<", io.DebugTimeStampe,
                ">]The oopTrace has done",
                ", next to refTrace",
                "\n"
              ))
            }
          }.otherwise{
            when(doOopWork.done){
              oopTrace_subState := sub_state.s_0
              state := overall_state.s_end
              if(DebugEnable){
                report(Seq(
                  "[GCTrace<", io.DebugTimeStampe,
                  ">]The oopWork has done",
                  "\n"
                ))
              }
            }
          }
        }
      }
      is(sub_state.s_3) {
        when(p < q + U(GCObjectPtr_Size)) {
          when(pushFifo(q - DestOopPtr + SrcOopPtr, q, ScanningInYoung, issued_oopTrace)){
            q := q - U(GCObjectPtr_Size)
            issued_oopTrace := False
            if(DebugEnable){
              report(Seq(
                "[GCTrace<", io.DebugTimeStampe,
                ">]Send the task to FIFO",
                ", p is ", p,
                ", q is ", q,
                "\n"
              ))
            }
          }
        }.otherwise {
          oopTrace_subState := sub_state.s_2
          end_map := end_map - U(GCObjectPtr_Size)
        }
      }
    }
  }

  val staticTrace_subState = RegInit(sub_state.s_0)
  val issued_staticTrace = RegInit(False)
  when(state === overall_state.s_staticTrace){
    switch(staticTrace_subState){
      is(sub_state.s_0){
        //access (SrcOopPtr + staticOopFieldOff)
        val addr = (SrcOopPtr + U(staticOopFieldCountOff)).resize(MMUAddrWidth bits)
        issueReq(io.TraceMMUIO.commonMReq, addr, False, U(0), U(0), mReqIssued) {rd =>
          p := (SrcOopPtr + U(StaticFieldOff)).resize(MMUAddrWidth bits)
          q := (SrcOopPtr + U(StaticFieldOff) + rd(31 downto 0) * U(GCObjectPtr_Size)).resize(MMUAddrWidth bits)
          staticTrace_subState := sub_state.s_1
        }
      }
      is(sub_state.s_1){
        when(p < q){
          when(pushFifo(p - DestOopPtr + SrcOopPtr, p, ScanningInYoung, issued_staticTrace)){
            p := p + U(GCObjectPtr_Size)
            issued_staticTrace := False
            if(DebugEnable){
              report(Seq(
                "[GCTrace<", io.DebugTimeStampe,
                ">]Send the task to FIFO",
                ", p is ", p,
                ", q is ", q,
                "\n"
              ))
            }
          }
        }.otherwise{
          when(doOopWork.done){
            staticTrace_subState := sub_state.s_0
            state := overall_state.s_end
            if(DebugEnable){
              report(Seq(
                "[GCTrace<", io.DebugTimeStampe,
                ">]The oopWork has done",
                "\n"
              ))
            }
          }
        }
      }
    }
  }

  val refTrace_subState = RegInit(sub_state.s_0)
  val issued_refTrace = RegInit(False)
  when(state === overall_state.s_refKlassTrace){
    switch(refTrace_subState){
      is(sub_state.s_0){
        // do_oop_work(DISCOVERED)
        when(pushFifo((SrcOopPtr + U(DISCOVERED_OFFSET)).resize(MMUAddrWidth bits), (DestOopPtr + U(DISCOVERED_OFFSET)).resize(MMUAddrWidth bits), ScanningInYoung, issued_refTrace)){
          refTrace_subState := sub_state.s_1
          issued_refTrace := False
          if(DebugEnable){
            report(Seq(
              "[GCTrace<", io.DebugTimeStampe,
              ">]Send the discovered task to FIFO",
              "\n"
            ))
          }
        }
      }
      is(sub_state.s_1){
        // do_oop_work(DISCOVERED)
        when(pushFifo((SrcOopPtr + U(REFERENT_OFFSET)).resize(MMUAddrWidth bits), (DestOopPtr + U(REFERENT_OFFSET)).resize(MMUAddrWidth bits), ScanningInYoung, issued_refTrace)){
          refTrace_subState := sub_state.s_2
          issued_refTrace := False
          if(DebugEnable){
            report(Seq(
              "[GCTrace<", io.DebugTimeStampe,
              ">]Send the referent task to FIFO",
              "\n"
            ))
          }
        }
      }
      is(sub_state.s_2){
        // do_oop_work(DISCOVERED)
        when(pushFifo((SrcOopPtr + U(DISCOVERED_OFFSET)).resize(MMUAddrWidth bits), (DestOopPtr + U(DISCOVERED_OFFSET)).resize(MMUAddrWidth bits), ScanningInYoung, issued_refTrace)){
          refTrace_subState := sub_state.s_3
          issued_refTrace := False
          if(DebugEnable){
            report(Seq(
              "[GCTrace<", io.DebugTimeStampe,
              ">]Send the discovered task to FIFO",
              "\n"
            ))
          }
        }
      }
      is(sub_state.s_3){
        when(doOopWork.done){
          refTrace_subState := sub_state.s_0
          state := overall_state.s_end
          if(DebugEnable){
            report(Seq(
              "[GCTrace<", io.DebugTimeStampe,
              ">]The oopWork has done",
              "\n"
            ))
          }
        }
      }
    }
  }

  when(state === overall_state.s_end){
    state := overall_state.s_idle
    io.ToTrace.Done := True
    if(DebugEnable){
      report(Seq(
        "[GCTrace<", io.DebugTimeStampe,
        ">]The trace has done",
        "\n"
      ))
    }
  }
}

object GCTraceVerilog extends App {
  Config.spinal.generateVerilog(new GCTrace())
}