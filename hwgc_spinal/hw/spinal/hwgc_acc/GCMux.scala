package hwgc_acc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCTraceMux extends Module {
  val io = new Bundle {
    val In0 = slave(new GCToTrace)
    val In1 = slave(new GCToTrace)
    val Out = master(new GCToTrace)
  }
  io.In0.clearOut()
  io.In1.clearOut()
  // 优先级 when() otherwise
  // 轮询
  val lastGrant = RegInit(False)
  val busy = RegInit(False)

  val grant1 = io.In1.Valid && (!io.In0.Valid || lastGrant === False)
  val grant0 = io.In0.Valid && !grant1

  val buf = Reg(new GCToTrace)
  val sel = Reg(Bool()) // 0 = In0 1 = In1

  when(!busy){
    when(grant0 || grant1){
      busy := True
      buf := Mux(grant0, io.In0, io.In1)
      sel := Mux(grant0, False, True)
    }
  }

  io.Out.Valid := busy
  io.Out.Kid := buf.Kid
  io.Out.OopType := buf.OopType
  io.Out.KlassPtr := buf.KlassPtr
  io.Out.SrcOopPtr := buf.SrcOopPtr
  io.Out.DestOopPtr := buf.DestOopPtr
  io.Out.ScanningInYoung := buf.ScanningInYoung
  io.Out.StepIndex := buf.StepIndex
  io.Out.StepNCreate := buf.StepNCreate
  io.Out.ArrayLength := buf.ArrayLength
  io.Out.PartialArrayStart := buf.PartialArrayStart

  when(busy){
    when(sel === False){
      io.In0.Ready := io.Out.Ready
      io.In0.Done := io.Out.Done
    }.otherwise{
      io.In1.Ready := io.Out.Ready
      io.In1.Done := io.Out.Done
    }

    when(io.Out.Done){
      busy := False
      lastGrant := sel
    }
  }
}

class GCAopMux extends Module {
  val io = new Bundle {
    val In0 = slave(new GCToAop)
    val In1 = slave(new GCToAop)
    val Out = master(new GCToAop)
  }
  io.In0.clearOut()
  io.In1.clearOut()
  // 优先级 when() otherwise
  // 轮询
  val lastGrant = RegInit(False)
  val busy = RegInit(False)

  val grant1 = io.In1.Valid && (!io.In0.Valid || lastGrant === False)
  val grant0 = io.In0.Valid && !grant1

  val buf = Reg(new GCToAop)
  val sel = Reg(Bool())

  when(!busy){
    when(grant0 || grant1){
      busy := True
      buf := Mux(grant0, io.In0, io.In1)
      sel := Mux(grant0, False, True)
    }
  }

  io.Out.Valid := busy
  io.Out.RegionAttr := buf.RegionAttr
  io.Out.Task := buf.Task

  when(busy){
    when(sel === False){
      io.In0.Ready := io.Out.Ready
      io.In0.Done := io.Out.Done
    }.otherwise{
      io.In1.Ready := io.Out.Ready
      io.In1.Done := io.Out.Done
    }

    when(io.Out.Done){
      busy := False
      lastGrant := sel
    }
  }
}