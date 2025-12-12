package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCLocalMMU extends Module with HWParameters with GCParameters {
  val io = new Bundle{
    val localMMUIOs = Vec.fill(LocalMMUTaskType.TaskTypeMax)(new LocalMMUIO)
    val LastLevelCacheTLIO = master(new LocalMMUIO)
    localMMUIOs.foreach(_.asSlave())
  }

  io.LastLevelCacheTLIO.Request.payload.clearAll()
  io.LastLevelCacheTLIO.Request.valid := False

  val arbStart = RegInit(U(0, LocalMMUTaskType.TaskTypeBitWidth bits))
  val allValid = Vec(Bool(), LocalMMUTaskType.TaskTypeMax)
  for(i <- 0 until LocalMMUTaskType.TaskTypeMax){
    allValid(i) := io.localMMUIOs(i).Request.valid
    io.localMMUIOs(i).Request.ready := False
    io.localMMUIOs(i).ConherentRequsetSourceID.valid := False
    io.localMMUIOs(i).ConherentRequsetSourceID.payload.clearAll()
    io.localMMUIOs(i).Response.valid := False
    io.localMMUIOs(i).Response.payload.clearAll()
  }

  val sourceid2port =  RegInit(Vec(Seq.fill(LLCSourceMaxNum)(U(0,log2Up(LocalMMUTaskType.TaskTypeMax) bits))))

  // 构造 rotatedValid[i] = allValid[(arbStart + i) % N]
  val rotatedValid = Vec(Bool(), LocalMMUTaskType.TaskTypeMax)
  for (i <- 0 until  LocalMMUTaskType.TaskTypeMax) {
    val sum = arbStart + i
    val idx = Mux(sum >= LocalMMUTaskType.TaskTypeMax, sum - LocalMMUTaskType.TaskTypeMax, sum) // safe mod N

    // mux: select allValid[idx]
    var sel = allValid(0)
    for (j <- 1 until LocalMMUTaskType.TaskTypeMax) {
      sel = Mux(idx === j, allValid(j), sel)
    }
    rotatedValid(i) := sel
  }

  // offset -> chosen_index
  val indexCandidates = (0 until LocalMMUTaskType.TaskTypeMax).map( i =>
    (rotatedValid(i), U(i, LocalMMUTaskType.TaskTypeBitWidth bits))
  )
  val offset = PriorityMux(indexCandidates)
  val chosen_index = (arbStart + offset).resize(LocalMMUTaskType.TaskTypeBitWidth bits)

  val hasRequest = rotatedValid.asBits.orR

  when(hasRequest && io.LastLevelCacheTLIO.Request.ready && io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid){
    arbStart := ((chosen_index + U(1)) % LocalMMUTaskType.TaskTypeMax).resize(LocalMMUTaskType.TaskTypeBitWidth bits)

    io.localMMUIOs(chosen_index).Request.ready := io.LastLevelCacheTLIO.Request.ready
    io.localMMUIOs(chosen_index).ConherentRequsetSourceID.valid := io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid
    io.localMMUIOs(chosen_index).ConherentRequsetSourceID.payload := io.LastLevelCacheTLIO.ConherentRequsetSourceID.payload

    io.LastLevelCacheTLIO.Request.valid := io.LastLevelCacheTLIO.ConherentRequsetSourceID.valid
    io.LastLevelCacheTLIO.Request.payload.RequestSourceID := io.LastLevelCacheTLIO.ConherentRequsetSourceID.payload
    io.LastLevelCacheTLIO.Request.payload.RequestVirtualAddr := io.localMMUIOs(chosen_index).Request.payload.RequestVirtualAddr
    io.LastLevelCacheTLIO.Request.payload.RequestType_isWrite := io.localMMUIOs(chosen_index).Request.payload.RequestType_isWrite
    io.LastLevelCacheTLIO.Request.payload.RequestData := io.localMMUIOs(chosen_index).Request.payload.RequestData

    sourceid2port(io.LastLevelCacheTLIO.ConherentRequsetSourceID.payload.resized) := chosen_index
  }

  val response_index = sourceid2port(io.LastLevelCacheTLIO.Response.payload.ResponseSourceID.resized)
  when(io.LastLevelCacheTLIO.Response.valid){
    io.localMMUIOs(response_index).Response.valid := True
    io.LastLevelCacheTLIO.Response.ready := io.localMMUIOs(response_index).Response.ready
    io.localMMUIOs(response_index).Response.payload.assignAllByName(io.LastLevelCacheTLIO.Response.payload)
  }.otherwise{
    io.LastLevelCacheTLIO.Response.ready := False
  }
}

object GCLocalMMUVerilog extends App{
  Config.spinal.generateVerilog(new GCLocalMMU())
}