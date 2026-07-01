package hwgc_acc

import hwgc_top.{Config, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCUnalignedMMUAdapter(
                             downstreamReturnsResponse: Boolean = true
                           ) extends Component with HWParameters {
  val io = new Bundle {
    val in  = slave(new LocalMMUIO)
    val out = master(new LocalMMUIO)
  }

  // defaults
  io.in.ConherentRequsetSourceID.valid := False
  io.in.ConherentRequsetSourceID.payload.clearAll()

  io.out.Request.payload.clearAll()

  io.in.Response.valid := False
  io.in.Response.payload.clearAll()

  // request state
  val busy             = RegInit(False)
  val splitReq         = RegInit(False)
  val sendBeat0Pending = RegInit(False)
  val sendBeat1Pending = RegInit(False)
  val gotOneResp       = RegInit(False)

  val size          = RegInit(U(0, LineBytesNumBitSize bits))
  val addr          = RegInit(U(0, MMUAddrWidth bits))
  val reqData       = RegInit(U(0, MMUDataWidth bits))
  val isWrite       = RegInit(False)
  val needResponse  = RegInit(False)
  val needDoCmpXchg = RegInit(False)

  val firstSourceID  = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val secondSourceID = RegInit(U(0, LLCSourceMaxNumBitSize bits))

  // first returned beat buffer, only used for split read response
  val bufferData    = RegInit(U(0, MMUDataWidth bits))
  val bufferIsBeat0 = RegInit(False)

  // final response buffer, used when in.Response.ready = False
  val respBufValid   = RegInit(False)
  val respBufPayload = Reg(cloneOf(io.in.Response.payload))
  respBufPayload.init(io.in.Response.payload.getZero)

  val sourceCount = 1 << LLCSourceMaxNumBitSize

  // Only used when downstream still returns out.Response for NeedResponse=False.
  // Counter instead of Bool, because split no-response requests may issue two beats
  // with the same SourceID before their responses are drained.
  val ignoredNoRespCounts =
    if (downstreamReturnsResponse) {
      Vec.fill(sourceCount)(Reg(UInt(2 bits)) init(0))
    } else {
      null
    }

  def ignoredNoRespSourceHit(id: UInt): Bool = {
    if (downstreamReturnsResponse) {
      ignoredNoRespCounts(id) =/= 0
    } else {
      False
    }
  }

  val hasIgnoredNoRespSource =
    if (downstreamReturnsResponse) {
      ignoredNoRespCounts.map(_ =/= 0).reduce(_ || _)
    } else {
      False
    }

  // derived values from latched request
  val offset      = addr(log2Up(LineBytesNum) - 1 downto 0)
  val alignedAddr = addr & ~U(LineBytesNum - 1, addr.getWidth bits)
  val crossBeat   = offset + size > LineBytesNum
  val firstBytes  = Mux(crossBeat, LineBytesNum - offset, size)
  val secondBytes = size - firstBytes

  val inOffsetNow      = io.in.Request.payload.RequestVirtualAddr(log2Up(LineBytesNum) - 1 downto 0)
  val inCrossBeatNow   = inOffsetNow + io.in.Request.payload.RequestSize > LineBytesNum
  val alignedAccessNow = inOffsetNow === 0 && !inCrossBeatNow

  // Chipyard still must aligned
  // val cmpxchgNow = io.in.Request.payload.NeedDoCmpxChg
  val cmpxchgNow      = False
  val directAccessNow = alignedAccessNow || cmpxchgNow

  // In true mode, do not accept a new transaction until old no-response responses
  // have been drained. This avoids SourceID reuse hazards without forming a comb loop.
  val canStartNewReq = !busy && !respBufValid
  val directReqNow   = canStartNewReq && io.in.Request.valid && directAccessNow

  val writeData0 = reqData |<< (offset << 3)
  val writeData1 = reqData |>> (firstBytes << 3)
  val mask0      = ((U(1, LineBytesNum bits) |<< firstBytes) - 1) |<< offset
  val mask1      = (U(1, LineBytesNum bits) |<< secondBytes) - 1

  val outSourceID = io.out.ConherentRequsetSourceID.payload

  // accept one new request
  io.in.Request.ready := canStartNewReq && (!directAccessNow || io.out.Request.ready)

  when(io.in.Request.fire) {
    size          := io.in.Request.payload.RequestSize
    addr          := io.in.Request.payload.RequestVirtualAddr
    reqData       := io.in.Request.payload.RequestData
    isWrite       := io.in.Request.payload.RequestType_isWrite
    needResponse  := io.in.Request.payload.NeedResponse
    needDoCmpXchg := io.in.Request.payload.NeedDoCmpxChg

    splitReq := inCrossBeatNow && !alignedAccessNow && !cmpxchgNow

    when(directAccessNow) {
      sendBeat0Pending := False
      sendBeat1Pending := False
    } otherwise {
      sendBeat0Pending := True
      sendBeat1Pending := inCrossBeatNow
    }

    gotOneResp := False
    busy := True
  }

  // request issuing logic
  val issueBeat0 = busy && sendBeat0Pending
  val issueBeat1 = busy && !sendBeat0Pending && sendBeat1Pending

  io.out.Request.valid := issueBeat0 || issueBeat1 || directReqNow

  when(directReqNow) {
    io.out.Request.payload.RequestVirtualAddr := io.in.Request.payload.RequestVirtualAddr
    io.out.Request.payload.RequestSourceID := outSourceID
    io.out.Request.payload.RequestData := io.in.Request.payload.RequestData
    io.out.Request.payload.RequestWStrb := io.in.Request.payload.RequestWStrb
    io.out.Request.payload.RequestType_isWrite := io.in.Request.payload.RequestType_isWrite
    io.out.Request.payload.RequestSize := io.in.Request.payload.RequestSize
    io.out.Request.payload.NeedResponse := io.in.Request.payload.NeedResponse
    io.out.Request.payload.NeedDoCmpxChg := io.in.Request.payload.NeedDoCmpxChg
  }

  when(issueBeat0) {
    io.out.Request.payload.RequestVirtualAddr := alignedAddr
    io.out.Request.payload.RequestSourceID := outSourceID
    io.out.Request.payload.NeedDoCmpxChg := needDoCmpXchg
    io.out.Request.payload.NeedResponse := needResponse

    when(isWrite) {
      io.out.Request.payload.RequestData := writeData0
      io.out.Request.payload.RequestWStrb := mask0
      io.out.Request.payload.RequestType_isWrite := True
    }
  }

  when(issueBeat1) {
    io.out.Request.payload.RequestVirtualAddr := alignedAddr + LineBytesNum
    io.out.Request.payload.RequestSourceID := outSourceID
    io.out.Request.payload.NeedDoCmpxChg := needDoCmpXchg
    io.out.Request.payload.NeedResponse := needResponse

    when(isWrite) {
      io.out.Request.payload.RequestData := writeData1
      io.out.Request.payload.RequestWStrb := mask1
      io.out.Request.payload.RequestType_isWrite := True
    }
  }

  when(io.out.Request.fire) {
    when(issueBeat0) {
      firstSourceID := outSourceID
      sendBeat0Pending := False
    }.elsewhen(issueBeat1) {
      secondSourceID := outSourceID
      sendBeat1Pending := False
    }.elsewhen(directReqNow) {
      firstSourceID := outSourceID
    }
  }

  // NeedResponse=False: transaction completes once the last request beat is issued.
  val noRespDirectDone =
    io.out.Request.fire && directReqNow && !io.in.Request.payload.NeedResponse

  val noRespAdaptedDone =
    io.out.Request.fire && !directReqNow && !needResponse &&
      (issueBeat1 || (issueBeat0 && !sendBeat1Pending))

  val noRespTxnDone = noRespDirectDone || noRespAdaptedDone

  when(noRespTxnDone) {
    busy := False
    splitReq := False
    gotOneResp := False
    sendBeat0Pending := False
    sendBeat1Pending := False
  }

  // response matching / dropping
  val respSourceID = io.out.Response.payload.ResponseSourceID
  val respIsBeat0  = respSourceID === firstSourceID
  val respIsBeat1  = respSourceID === secondSourceID

  val currentRespHit = Bool()
  currentRespHit := False

  when(busy && needResponse) {
    when(!splitReq) {
      currentRespHit := !sendBeat0Pending && respIsBeat0
    } otherwise {
      when(!gotOneResp) {
        currentRespHit :=
          !sendBeat0Pending &&
            (respIsBeat0 || (!sendBeat1Pending && respIsBeat1))
      } otherwise {
        currentRespHit :=
          !sendBeat0Pending &&
            !sendBeat1Pending &&
            Mux(bufferIsBeat0, respIsBeat1, respIsBeat0)
      }
    }
  }

  val ignoredRespHit = ignoredNoRespSourceHit(respSourceID)
  val firstSplitRespCanArrive = splitReq && !gotOneResp && !sendBeat0Pending && !respBufValid
  val finalRespCanArrive = (!splitReq && !sendBeat0Pending && !sendBeat1Pending && !respBufValid) ||
      (splitReq && gotOneResp && !sendBeat0Pending && !sendBeat1Pending && !respBufValid)

  val currentRespCanTakeRaw =
    busy && needResponse && currentRespHit &&
      (firstSplitRespCanArrive || finalRespCanArrive)

  val ignoredRespCanDrop =
    if (downstreamReturnsResponse) {
      ignoredRespHit
    } else {
      False
    }

  val currentRespCanTake = currentRespCanTakeRaw && !ignoredRespCanDrop

  io.out.Response.ready := currentRespCanTake || ignoredRespCanDrop

  val currentRespFire = io.out.Response.fire && currentRespCanTake
  val ignoredRespFire = io.out.Response.fire && ignoredRespCanDrop

  val noRespReqBeatFire =
    io.out.Request.fire &&
      ((directReqNow && !io.in.Request.payload.NeedResponse) ||
        (!directReqNow && !needResponse))

  if (downstreamReturnsResponse) {
    for (i <- 0 until sourceCount) {
      val incHit = noRespReqBeatFire && outSourceID === U(i, LLCSourceMaxNumBitSize bits)
      val decHit = ignoredRespFire && respSourceID === U(i, LLCSourceMaxNumBitSize bits)

      when(incHit && !decHit) {
        ignoredNoRespCounts(i) := ignoredNoRespCounts(i) + 1
      }.elsewhen(!incHit && decHit) {
        ignoredNoRespCounts(i) := ignoredNoRespCounts(i) - 1
      }
    }
  }

  val finalRespArrive = currentRespFire && (!splitReq || gotOneResp)
  val needUpResp      = needResponse
  val finalRespNow    = finalRespArrive && needUpResp

  // response bypass path
  val bypassRespPayload = cloneOf(io.in.Response.payload)
  bypassRespPayload.ResponseSourceID := io.out.Response.payload.ResponseSourceID
  bypassRespPayload.ResponseData := io.out.Response.payload.ResponseData

  when(currentRespFire) {
    when(!splitReq) {
      when(!isWrite) {
        bypassRespPayload.ResponseData := io.out.Response.payload.ResponseData |>> (offset << 3)
      }
    } otherwise {
      when(gotOneResp) {
        val beat0Data = Mux(bufferIsBeat0, bufferData, io.out.Response.payload.ResponseData)
        val beat1Data = Mux(bufferIsBeat0, io.out.Response.payload.ResponseData, bufferData)

        when(!isWrite) {
          val firstPart  = beat0Data |>> (offset << 3)
          val secondPart = beat1Data |<< (firstBytes << 3)
          bypassRespPayload.ResponseData := firstPart | secondPart
        }
      }
    }
  }

  // response output: buffered response has priority, otherwise bypass current one
  when(respBufValid) {
    io.in.Response.valid := True
    io.in.Response.payload := respBufPayload
  } otherwise {
    when(finalRespNow) {
      io.in.Response.valid := True
      io.in.Response.payload := bypassRespPayload
    }
  }

  // response state update
  when(respBufValid && io.in.Response.ready) {
    respBufValid := False
  }

  when(currentRespFire) {
    when(splitReq && !gotOneResp) {
      // first response of a split transaction: buffer it
      bufferData := io.out.Response.payload.ResponseData
      bufferIsBeat0 := respIsBeat0
      gotOneResp := True
    } otherwise {
      // this is the final response of current transaction
      when(needUpResp) {
        when(!respBufValid && !io.in.Response.ready) {
          respBufPayload := bypassRespPayload
          respBufValid := True
        }
      }

      busy := False
      splitReq := False
      gotOneResp := False
    }
  }
}

object GCUnalignedMMUAdapterVerilog extends App {
  Config.spinal.generateVerilog(new GCUnalignedMMUAdapter())
}
