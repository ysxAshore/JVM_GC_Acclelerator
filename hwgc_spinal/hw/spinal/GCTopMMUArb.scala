package hwgc_top

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCTopMMUArb extends Module with HWParameters with GCTopParameters {
  val io = new Bundle {
    val localMMUIOs = Vec.fill(2)(new LocalMMUIO)
    val LastLevelCacheTLIO = master(new LocalMMUIO)

    localMMUIOs.foreach(_.asSlave())
  }

  // defaults
  io.LastLevelCacheTLIO.Request.payload.clearAll()

  io.LastLevelCacheTLIO.RequestSize.valid := False
  io.LastLevelCacheTLIO.RequestSize.payload.clearAll()

  io.LastLevelCacheTLIO.Response.ready := False

  for (i <- 0 until 2) {
    // 重点：sourceID grant 不要由 reqFire 产生
    // 顶层直接把 LLC 给的 sourceID 广播给所有 child
    io.localMMUIOs(i).ConherentRequsetSourceID.valid :=
      io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid

    io.localMMUIOs(i).ConherentRequsetSourceID.payload :=
      io.LastLevelCacheTLIO.ConherentRequsetSourceID.payload

    io.localMMUIOs(i).Response.valid := False
    io.localMMUIOs(i).Response.payload.clearAll()
  }

  // sourceID -> child port
  val sourceid2port = RegInit(Vec(Seq.fill(LLCSourceMaxNum)(U(0, 1 bits))))

  // round-robin request arbitration
  val prefer1 = RegInit(False)

  val req0 = io.localMMUIOs(0).Request.valid
  val req1 = io.localMMUIOs(1).Request.valid

  val hasRequest = req0 || req1

  val choose1 = Bool()

  when(req0 && req1) {
    choose1 := prefer1
  } otherwise {
    choose1 := req1
  }

  io.LastLevelCacheTLIO.Request.valid := hasRequest

  when(hasRequest) {
    when(choose1) {
      io.LastLevelCacheTLIO.Request.payload.assignAllByName(io.localMMUIOs(1).Request.payload)
      io.LastLevelCacheTLIO.RequestSize.payload := io.localMMUIOs(1).RequestSize.payload
    } otherwise {
      io.LastLevelCacheTLIO.Request.payload.assignAllByName(io.localMMUIOs(0).Request.payload)
      io.LastLevelCacheTLIO.RequestSize.payload := io.localMMUIOs(0).RequestSize.payload
    }

    // 你的 Request 和 RequestSize 是绑定的，所以这里跟 Request.valid 同步
    io.LastLevelCacheTLIO.RequestSize.valid := True
  }

  val reqFire = io.LastLevelCacheTLIO.Request.valid && io.LastLevelCacheTLIO.Request.ready

  io.localMMUIOs(0).Request.ready := reqFire && !choose1
  io.localMMUIOs(1).Request.ready := reqFire && choose1

  when(reqFire) {
    val sid = io.LastLevelCacheTLIO.Request.payload.RequestSourceID.resized

    sourceid2port(sid) := choose1.asUInt

    // 下一次两个都有请求时，优先另一个
    prefer1 := !choose1
  }

  // response route by sourceID
  val response_index = sourceid2port(io.LastLevelCacheTLIO.Response.payload.ResponseSourceID.resized)

  when(io.LastLevelCacheTLIO.Response.valid) {
    io.localMMUIOs(response_index).Response.valid := True
    io.localMMUIOs(response_index).Response.payload.assignAllByName(io.LastLevelCacheTLIO.Response.payload)
    io.LastLevelCacheTLIO.Response.ready := io.localMMUIOs(response_index).Response.ready
  }
}
