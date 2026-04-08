package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps
class GCUnalignedMMUAdapter extends Component with HWParameters with GCParameters {
  val io = new Bundle {
    val in  = slave(new LocalMMUIO)
    val out = master(new LocalMMUIO)
  }

  // defaults
  io.in.ConherentRequsetSourceID.valid := False
  io.in.ConherentRequsetSourceID.payload.clearAll()

  io.out.RequestSize.valid := False
  io.out.RequestSize.payload.clearAll()

  // ----------------------------
  // request registers
  // ----------------------------
  val busy            = RegInit(False)
  val splitReq        = RegInit(False)
  val sendBeat0Pending = RegInit(False)
  val sendBeat1Pending = RegInit(False)
  val gotOneResp      = RegInit(False)

  val size    = RegInit(U(0, LineBytesNumBitSize bits))
  val addr    = RegInit(U(0, MMUAddrWidth bits))
  val isWrite = RegInit(False)
  val reqData = RegInit(U(0, MMUDataWidth bits))

  val firstSourceID  = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val secondSourceID = RegInit(U(0, LLCSourceMaxNumBitSize bits))

  val bufferData     = RegInit(U(0, MMUDataWidth bits))
  val bufferIsBeat0  = RegInit(False)

  // final response buffer
  val respOutValid   = RegInit(False)
  val respOutPayload = Reg(cloneOf(io.in.Response.payload))

  io.in.Response.valid := respOutValid
  io.in.Response.payload := respOutPayload

  when(respOutValid && io.in.Response.ready) {
    respOutValid := False
    busy := False
    gotOneResp := False
  }

  // ----------------------------
  // derived values from latched request
  // ----------------------------
  val offset      = addr(log2Up(LineBytesNum) - 1 downto 0)
  val alignedAddr = addr & ~U(LineBytesNum - 1, addr.getWidth bits)
  val crossBeat   = offset + size > LineBytesNum
  val firstBytes  = Mux(crossBeat, LineBytesNum - offset, size)
  val secondBytes = size - firstBytes

  val writeData0 = reqData |<< (offset << 3)
  val writeData1 = reqData |>> (firstBytes << 3)
  val mask0      = ((U(1, LineBytesNum bits) |<< firstBytes) - 1) |<< offset
  val mask1      = (U(1, LineBytesNum bits) |<< secondBytes) - 1

  // ----------------------------
  // accept one new request
  // ----------------------------
  io.in.Request.ready := !busy && !respOutValid

  val inOffsetNow    = io.in.Request.payload.RequestVirtualAddr(log2Up(LineBytesNum) - 1 downto 0)
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

  // ----------------------------
  // request issuing logic
  // ----------------------------
  val issueBeat0 = busy && sendBeat0Pending
  val issueBeat1 = busy && !sendBeat0Pending && sendBeat1Pending

  io.out.Request.valid := issueBeat0 || issueBeat1
  io.out.Request.payload.clearAll()

  when(issueBeat0) {
    io.out.Request.payload.RequestVirtualAddr := alignedAddr
    io.out.Request.payload.RequestSourceID := io.out.ConherentRequsetSourceID.payload
    when(isWrite) {
      io.out.Request.payload.RequestData  := writeData0
      io.out.Request.payload.RequestWStrb := mask0
      io.out.Request.payload.RequestType_isWrite := True
    }
  }

  when(issueBeat1) {
    io.out.Request.payload.RequestVirtualAddr := alignedAddr + LineBytesNum
    io.out.Request.payload.RequestSourceID := io.out.ConherentRequsetSourceID.payload
    when(isWrite) {
      io.out.Request.payload.RequestData  := writeData1
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

  // ----------------------------
  // response handling
  // only receive when all sub-requests have been issued,
  // and final response buffer is empty
  // ----------------------------
  io.out.Response.ready := busy && !sendBeat0Pending && !sendBeat1Pending && !respOutValid

  when(io.out.Response.fire) {
    when(!splitReq) {
      respOutPayload.assignAllByName(io.out.Response.payload)
      when(!isWrite) {
        respOutPayload.ResponseData := io.out.Response.payload.ResponseData |>> (offset << 3)
      }
      respOutValid := True
    } otherwise {
      val thisIsBeat0 = io.out.Response.payload.ResponseSourceID === firstSourceID

      when(!gotOneResp) {
        bufferData    := io.out.Response.payload.ResponseData
        bufferIsBeat0 := thisIsBeat0
        gotOneResp    := True
      } otherwise {
        val beat0Data = Mux(bufferIsBeat0, bufferData, io.out.Response.payload.ResponseData)
        val beat1Data = Mux(bufferIsBeat0, io.out.Response.payload.ResponseData, bufferData)

        respOutPayload.assignAllByName(io.out.Response.payload)

        when(!isWrite) {
          val firstPart = beat0Data |>> (offset << 3)
          val secondPart = beat1Data |<< (firstBytes << 3)
          respOutPayload.ResponseData := firstPart | secondPart
        }

        respOutValid := True
      }
    }
  }
}

object GCUnalignedMMUAdapterVerilog extends App{
  Config.spinal.generateVerilog(new GCUnalignedMMUAdapter())
}