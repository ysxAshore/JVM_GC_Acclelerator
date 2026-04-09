package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCUnalignedMMUAdapter extends Component with HWParameters with GCParameters {
  val io = new Bundle {
    val in  = slave(new LocalMMUIO)
    val out = master(new LocalMMUIO)
  }

  // --------------------------------------------------------------------------
  // defaults
  // --------------------------------------------------------------------------
  io.in.ConherentRequsetSourceID.valid := False
  io.in.ConherentRequsetSourceID.payload.clearAll()

  io.out.Request.payload.clearAll()

  io.out.RequestSize.valid := False
  io.out.RequestSize.payload.clearAll()

  io.in.Response.valid := False
  io.in.Response.payload.clearAll()

  // --------------------------------------------------------------------------
  // request state
  // --------------------------------------------------------------------------
  val busy             = RegInit(False)
  val splitReq         = RegInit(False)
  val sendBeat0Pending = RegInit(False)
  val sendBeat1Pending = RegInit(False)
  val gotOneResp       = RegInit(False)

  val size    = RegInit(U(0, LineBytesNumBitSize bits))
  val addr    = RegInit(U(0, MMUAddrWidth bits))
  val isWrite = RegInit(False)
  val reqData = RegInit(U(0, MMUDataWidth bits))

  val firstSourceID  = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val secondSourceID = RegInit(U(0, LLCSourceMaxNumBitSize bits))

  // first returned beat buffer (only used for split read response)
  val bufferData    = RegInit(U(0, MMUDataWidth bits))
  val bufferIsBeat0 = RegInit(False)

  // final response buffer (used when in.Response.ready = False)
  val respBufValid   = RegInit(False)
  val respBufPayload = Reg(cloneOf(io.in.Response.payload))
  respBufPayload.init(io.in.Response.payload.getZero)

  // --------------------------------------------------------------------------
  // derived values from latched request
  // --------------------------------------------------------------------------
  val offset      = addr(log2Up(LineBytesNum) - 1 downto 0)
  val alignedAddr = addr & ~U(LineBytesNum - 1, addr.getWidth bits)
  val crossBeat   = offset + size > LineBytesNum
  val firstBytes  = Mux(crossBeat, LineBytesNum - offset, size)
  val secondBytes = size - firstBytes

  val writeData0 = reqData |<< (offset << 3)
  val writeData1 = reqData |>> (firstBytes << 3)
  val mask0      = ((U(1, LineBytesNum bits) |<< firstBytes) - 1) |<< offset
  val mask1      = (U(1, LineBytesNum bits) |<< secondBytes) - 1

  // --------------------------------------------------------------------------
  // accept one new request
  // --------------------------------------------------------------------------
  io.in.Request.ready := !busy && !respBufValid

  val inOffsetNow    =
    io.in.Request.payload.RequestVirtualAddr(log2Up(LineBytesNum) - 1 downto 0)
  val inCrossBeatNow = inOffsetNow + io.in.RequestSize.payload > LineBytesNum

  when(io.in.Request.fire) {
    size    := io.in.RequestSize.payload
    addr    := io.in.Request.payload.RequestVirtualAddr
    reqData := io.in.Request.payload.RequestData
    isWrite := io.in.Request.payload.RequestType_isWrite

    splitReq         := inCrossBeatNow
    sendBeat0Pending := True
    sendBeat1Pending := inCrossBeatNow
    gotOneResp       := False
    busy             := True
  }

  // --------------------------------------------------------------------------
  // request issuing logic
  // --------------------------------------------------------------------------
  val issueBeat0 = busy && sendBeat0Pending
  val issueBeat1 = busy && !sendBeat0Pending && sendBeat1Pending

  io.out.Request.valid := issueBeat0 || issueBeat1

  when(issueBeat0) {
    io.out.Request.payload.RequestVirtualAddr := alignedAddr
    io.out.Request.payload.RequestSourceID := io.out.ConherentRequsetSourceID.payload
    when(isWrite) {
      io.out.Request.payload.RequestData := writeData0
      io.out.Request.payload.RequestWStrb := mask0
      io.out.Request.payload.RequestType_isWrite := True
    }
  }

  when(issueBeat1) {
    io.out.Request.payload.RequestVirtualAddr := alignedAddr + LineBytesNum
    io.out.Request.payload.RequestSourceID := io.out.ConherentRequsetSourceID.payload
    when(isWrite) {
      io.out.Request.payload.RequestData := writeData1
      io.out.Request.payload.RequestWStrb := mask1
      io.out.Request.payload.RequestType_isWrite := True
    }
  }

  when(io.out.Request.fire) {
    when(issueBeat0) {
      firstSourceID := io.out.ConherentRequsetSourceID.payload
      sendBeat0Pending := False
    } elsewhen(issueBeat1) {
      secondSourceID := io.out.ConherentRequsetSourceID.payload
      sendBeat1Pending := False
    }
  }

  // --------------------------------------------------------------------------
  // response bypass path
  // finalRespNow:
  //   - non-split: first response is already final
  //   - split:     only second response is final (gotOneResp already set)
  // --------------------------------------------------------------------------
  val finalRespArrive = io.out.Response.fire && (!splitReq || gotOneResp)
  val needUpResp = !isWrite
  val finalRespNow = finalRespArrive && needUpResp

  val bypassRespPayload = cloneOf(io.in.Response.payload)
  bypassRespPayload.ResponseSourceID := io.out.Response.payload.ResponseSourceID
  bypassRespPayload.ResponseData := io.out.Response.payload.ResponseData

  when(io.out.Response.fire) {
    when(!splitReq) {
      // single beat response
      when(!isWrite) {
        bypassRespPayload.ResponseData := io.out.Response.payload.ResponseData |>> (offset << 3)
      }
    } otherwise {
      // split response: only meaningful when second response arrives
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

  // --------------------------------------------------------------------------
  // response output: buffered response has priority, otherwise bypass current one
  // --------------------------------------------------------------------------
  when(respBufValid) {
    io.in.Response.valid := True
    io.in.Response.payload := respBufPayload
  } elsewhen(finalRespNow) {
    io.in.Response.valid := True
    io.in.Response.payload := bypassRespPayload
  }

  // Only accept out response when:
  //   - request(s) have all been issued
  //   - final response buffer is empty
  io.out.Response.ready := busy && !sendBeat0Pending && !sendBeat1Pending && !respBufValid

  // --------------------------------------------------------------------------
  // response state update
  // --------------------------------------------------------------------------

  // 1) buffered final response consumed by upstream
  when(respBufValid && io.in.Response.ready) {
    respBufValid := False
  }

  // 2) handle responses from out
  when(io.out.Response.fire) {
    when(splitReq && !gotOneResp) {
      // first response of a split transaction: buffer it
      bufferData := io.out.Response.payload.ResponseData
      bufferIsBeat0 := io.out.Response.payload.ResponseSourceID === firstSourceID
      gotOneResp := True
    } otherwise {
      // this is the final response of current transaction
      // if upstream can't take it this cycle, store it
      when(needUpResp) {
        when(!respBufValid && !io.in.Response.ready) {
          respBufPayload := bypassRespPayload
          respBufValid := True
        }
      }

      // transaction itself is completed once final response is received
      busy := False
      gotOneResp := False
    }
  }
}

object GCUnalignedMMUAdapterVerilog extends App{
  Config.spinal.generateVerilog(new GCUnalignedMMUAdapter())
}