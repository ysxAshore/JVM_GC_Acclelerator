package hwgc_acc

import hwgc_top.{Config, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCLocalMMU(count: Int) extends Module with HWParameters {
  val io = new Bundle {
    val localMMUIOs = Vec.fill(count)(new LocalMMUIO)
    val LastLevelCacheTLIO = master(new LocalMMUIO)
    localMMUIOs.foreach(_.asSlave())
  }

  // defaults
  io.LastLevelCacheTLIO.Request.payload.clearAll()
  io.LastLevelCacheTLIO.RequestSize.valid := False
  io.LastLevelCacheTLIO.RequestSize.payload.clearAll()
  io.LastLevelCacheTLIO.Response.ready := False

  for (i <- 0 until count) {
    io.localMMUIOs(i).Request.ready := False
    io.localMMUIOs(i).ConherentRequsetSourceID.valid := False
    io.localMMUIOs(i).ConherentRequsetSourceID.payload.clearAll()
    io.localMMUIOs(i).Response.valid := False
    io.localMMUIOs(i).Response.payload.clearAll()
  }

  // sourceID -> input port
  val sourceid2port = RegInit(Vec(Seq.fill(LLCSourceMaxNum)(U(0, log2Up(count) bits))))

  // request arbitration
  val arbStart = RegInit(U(0, log2Up(count) + 1 bits))

  val allValid = Vec(Bool(), count)
  for (i <- 0 until count) {
    // 这里把 Request 和 RequestSize 绑在一起看
    allValid(i) := io.localMMUIOs(i).Request.valid
  }

  // rotatedValid[i] = allValid[(arbStart + i) % N]
  val rotatedValid = Vec(Bool(), count)
  for (i <- 0 until count) {
    val sum = arbStart + i
    val idx = Mux(sum >= count, sum - count, sum)

    var sel = allValid(0)
    for (j <- 1 until count) {
      sel = Mux(idx === j, allValid(j), sel)
    }
    rotatedValid(i) := sel
  }

  val indexCandidates = (0 until count).map { i =>
    (rotatedValid(i), U(i, log2Up(count) bits))
  }

  val offset = PriorityMux(indexCandidates)
  val chosen_index = ((arbStart + offset) % U(count)).resize(log2Up(count) bits)

  val hasRequest = rotatedValid.asBits.orR

  // 这里的 sourceID 是 LLC/下层给的，所以 valid 也要参与能否发请求
  val canIssue = hasRequest && io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid
  io.LastLevelCacheTLIO.Request.valid := canIssue

  when(hasRequest) {
    io.LastLevelCacheTLIO.Request.payload.RequestSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID.payload.resized
    io.LastLevelCacheTLIO.Request.payload.RequestVirtualAddr := io.localMMUIOs(chosen_index).Request.payload.RequestVirtualAddr
    io.LastLevelCacheTLIO.Request.payload.RequestType_isWrite := io.localMMUIOs(chosen_index).Request.payload.RequestType_isWrite
    io.LastLevelCacheTLIO.Request.payload.RequestData := io.localMMUIOs(chosen_index).Request.payload.RequestData
    io.LastLevelCacheTLIO.Request.payload.RequestWStrb := io.localMMUIOs(chosen_index).Request.payload.RequestWStrb
    io.LastLevelCacheTLIO.RequestSize.payload := io.localMMUIOs(chosen_index).RequestSize.payload
  }

  val reqFire = io.LastLevelCacheTLIO.Request.valid && io.LastLevelCacheTLIO.Request.ready

  when(hasRequest) {
    io.localMMUIOs(chosen_index).Request.ready := reqFire
    io.localMMUIOs(chosen_index).ConherentRequsetSourceID.valid := reqFire
    io.localMMUIOs(chosen_index).ConherentRequsetSourceID.payload := io.LastLevelCacheTLIO.ConherentRequsetSourceID.payload

    when(reqFire) {
      sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.payload.resized) := chosen_index
      arbStart := ((chosen_index + U(1)) % count).resize(log2Up(count) + 1)
    }
  }

  // response route by sourceID
  val response_index = sourceid2port(io.LastLevelCacheTLIO.Response.payload.ResponseSourceID.resized)

  when(io.LastLevelCacheTLIO.Response.valid) {
    io.localMMUIOs(response_index).Response.valid := True
    io.localMMUIOs(response_index).Response.payload.assignAllByName(io.LastLevelCacheTLIO.Response.payload)
    io.LastLevelCacheTLIO.Response.ready := io.localMMUIOs(response_index).Response.ready
  } otherwise {
    io.LastLevelCacheTLIO.Response.ready := False
  }
}

object GCLocalMMUVerilog extends App {
  Config.spinal.generateVerilog(new GCLocalMMU(1))
}