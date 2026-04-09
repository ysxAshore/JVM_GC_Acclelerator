package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCCopy extends Component with HWParameters with GCParameters {
  val io = new Bundle {
    val ToCopy    = slave(new GCToCopy)
    val readMReq  = master(new LocalMMUIO)
    val writeMReq = master(new LocalMMUIO)
  }

  // ==========================================================================
  // constants / helpers
  // ==========================================================================
  val BeatBytes    = MMUDataWidth / 8
  val BeatBytesU32 = U(BeatBytes, 32 bits)
  val OffBits      = log2Up(BeatBytes)
  val RobPtrBits   = log2Up(GCCopyEntry)
  val BeatLenBits  = log2Up(BeatBytes + 1)

  def alignDown(x: UInt, width: Int): UInt = {
    x & ~U(BeatBytes - 1, width bits)
  }

  def genMask(byteLen: UInt, byteOffset: UInt): UInt = {
    val ret = UInt(BeatBytes bits)
    for (i <- 0 until BeatBytes) {
      ret(i) := (
        U(i, BeatLenBits bits) >= byteOffset.resize(BeatLenBits)
        ) && (
        U(i, BeatLenBits bits) < (byteOffset.resize(BeatLenBits) + byteLen.resize(BeatLenBits))
        )
    }
    ret
  }

  // ==========================================================================
  // task state
  // ==========================================================================
  val task_valid   = RegInit(False)
  val zeroTaskDone = RegInit(False)

  val srcPtr    = RegInit(U(0, GCElementWidth bits))
  val dstPtr    = RegInit(U(0, GCElementWidth bits))
  val totalSize = RegInit(U(0, 32 bits))

  val srcBase = RegInit(U(0, GCElementWidth bits))
  val srcOff  = RegInit(U(0, OffBits bits))

  // aligned read beats total / issued
  val readBeatCount = RegInit(U(0, 32 bits))
  val readReqIdx    = RegInit(U(0, 32 bits))

  // logical valid bytes appended into repack buffer
  val bytesAppended = RegInit(U(0, 32 bits))

  // logical bytes already sent to write side
  val bytesWritten  = RegInit(U(0, 32 bits))

  // ==========================================================================
  // reorder buffer (ROB) for out-of-order read responses
  // - request order allocates slots
  // - response uses SourceID to fill slot
  // - consume always from robConsumePtr in-order
  // ==========================================================================
  val robAllocPtr   = RegInit(U(0, RobPtrBits bits))
  val robConsumePtr = RegInit(U(0, RobPtrBits bits))
  val robCount      = RegInit(U(0, log2Up(GCCopyEntry + 1) bits))

  val robData      = Vec.fill(GCCopyEntry)(Reg(UInt(MMUDataWidth bits)) init(0))
  val robRespValid = Vec.fill(GCCopyEntry)(RegInit(False))

  // SourceID -> ROB slot
  val sourceId2RobSlot =
    Reg(Vec(Seq.fill(LLCSourceMaxNum)(U(0, RobPtrBits bits))))

  // ==========================================================================
  // repack buffer
  // - low bytes are always the next logical bytes to write
  // - width 2 beats is enough for "consume one beat + append one beat"
  // ==========================================================================
  val repackBuf        = RegInit(U(0, (2 * MMUDataWidth) bits))
  val repackValidBytes = RegInit(U(0, log2Up(2 * BeatBytes + 1) bits))

  val firstReadBeat = RegInit(False)

  // ==========================================================================
  // task accept / done
  // ==========================================================================
  io.ToCopy.Ready := !task_valid

  val taskDoneNow =
    task_valid &&
      (readReqIdx === readBeatCount) &&
      (robCount === 0) &&
      (repackValidBytes === 0) &&
      (bytesAppended === totalSize) &&
      (bytesWritten === totalSize)

  io.ToCopy.Done := zeroTaskDone || taskDoneNow

  val counter = RegInit(U(0, 64 bits))
  when(task_valid){
    counter := counter + 1
  }

  when(io.ToCopy.Valid && io.ToCopy.Ready) {
    srcPtr := io.ToCopy.SrcOopPtr
    dstPtr := io.ToCopy.DestOopPtr
    totalSize := io.ToCopy.Size

    srcBase := alignDown(io.ToCopy.SrcOopPtr, GCElementWidth)
    srcOff  := io.ToCopy.SrcOopPtr(OffBits - 1 downto 0)

    readReqIdx := 0
    bytesAppended := 0
    bytesWritten := 0

    robAllocPtr := 0
    robConsumePtr := 0
    robCount := 0

    repackBuf := 0
    repackValidBytes := 0
    firstReadBeat := True

    for (i <- 0 until GCCopyEntry) {
      robRespValid(i) := False
    }

    when(io.ToCopy.Size === 0) {
      task_valid := False
      zeroTaskDone := True
      readBeatCount := 0
    } otherwise {
      task_valid := True
      zeroTaskDone := False

      // readBeatCount = ceil((srcOff + size) / BeatBytes)
      readBeatCount :=
        ((io.ToCopy.SrcOopPtr(OffBits - 1 downto 0) +
          io.ToCopy.Size +
          U(BeatBytes - 1, 32 bits)) >> OffBits).resize(32)
    }
  } elsewhen(zeroTaskDone) {
    zeroTaskDone := False
  } elsewhen(taskDoneNow) {
    task_valid := False
  }

  // ==========================================================================
  // aligned read request generator
  // - always sends aligned full-beat reads
  // - request order allocates ROB slot
  // ==========================================================================
  val readCanAlloc =
    task_valid &&
      (readReqIdx =/= readBeatCount) &&
      (robCount =/= U(GCCopyEntry, robCount.getWidth bits))

  val readReqValid = readCanAlloc

  val curReadAddr =
    (srcBase + (readReqIdx.resize(GCElementWidth) |<< OffBits)).resize(MMUAddrWidth)

  io.readMReq.Request.valid := readReqValid
  io.readMReq.Request.payload.RequestVirtualAddr := curReadAddr
  io.readMReq.Request.payload.RequestSourceID := io.readMReq.ConherentRequsetSourceID.payload
  io.readMReq.Request.payload.RequestType_isWrite := False
  io.readMReq.Request.payload.RequestWStrb := 0
  io.readMReq.Request.payload.RequestData := 0

  io.readMReq.RequestSize.valid := readReqValid
  io.readMReq.RequestSize.payload := U(BeatBytes, LineBytesNumBitSize bits)

  val readReqFire = io.readMReq.Request.fire

  when(readReqFire) {
    val sid = io.readMReq.ConherentRequsetSourceID.payload.resized
    sourceId2RobSlot(sid) := robAllocPtr
    robRespValid(robAllocPtr) := False

    robAllocPtr := WrapInc(robAllocPtr, GCCopyEntry, U(1))
    readReqIdx := readReqIdx + 1
  }

  // ==========================================================================
  // read response handling (out-of-order)
  // - lookup slot by ResponseSourceID
  // - fill ROB slot
  // ==========================================================================
  val respSid  = io.readMReq.Response.payload.ResponseSourceID.resized
  val respSlot = sourceId2RobSlot(respSid)

  io.readMReq.Response.ready := task_valid && !robRespValid(respSlot)

  when(io.readMReq.Response.fire) {
    robData(respSlot) := io.readMReq.Response.payload.ResponseData
    robRespValid(respSlot) := True
  }

  // ==========================================================================
  // write side
  // - write address always aligned
  // - low bytes of repackBuf are the next logical bytes to send
  // ==========================================================================
  val curWriteByteAddr =
    (dstPtr + bytesWritten.resize(GCElementWidth)).resize(GCElementWidth)

  val curWriteAddrAligned =
    alignDown(curWriteByteAddr, GCElementWidth).resize(MMUAddrWidth)

  val curDstOff =
    curWriteByteAddr(OffBits - 1 downto 0)

  val remainWriteBytes =
    totalSize - bytesWritten

  val spaceThisBeat =
    BeatBytesU32 - curDstOff.resize(32)

  val thisBeatWriteLen32 =
    Mux(remainWriteBytes <= spaceThisBeat, remainWriteBytes, spaceThisBeat)

  val thisBeatWriteLen =
    thisBeatWriteLen32.resize(BeatLenBits)

  val repackLowBeat =
    repackBuf(MMUDataWidth - 1 downto 0)

  val writeReqValid =
    task_valid &&
      (bytesWritten =/= totalSize) &&
      (thisBeatWriteLen32 =/= 0) &&
      (repackValidBytes.resize(32) >= thisBeatWriteLen32)

  io.writeMReq.Request.valid := writeReqValid
  io.writeMReq.Request.payload.RequestVirtualAddr := curWriteAddrAligned
  io.writeMReq.Request.payload.RequestSourceID := io.writeMReq.ConherentRequsetSourceID.payload
  io.writeMReq.Request.payload.RequestType_isWrite := True
  io.writeMReq.Request.payload.RequestWStrb := genMask(thisBeatWriteLen, curDstOff)
  io.writeMReq.Request.payload.RequestData := (repackLowBeat |<< (curDstOff << 3)).resized

  io.writeMReq.RequestSize.valid := writeReqValid
  io.writeMReq.RequestSize.payload := thisBeatWriteLen.resize(LineBytesNumBitSize)

  val writeFire = io.writeMReq.Request.fire

  when(writeFire) {
    bytesWritten := bytesWritten + thisBeatWriteLen32
  }

  // ==========================================================================
  // consume from ROB head in-order, append into repackBuf
  // - only append logical valid bytes
  // - no tail garbage bytes are appended
  // ==========================================================================
  val consumeBytes32 =
    Mux(writeFire, thisBeatWriteLen32, U(0, 32 bits))

  val repackValidAfterConsume32 =
    repackValidBytes.resize(32) - consumeBytes32

  val repackBufAfterConsume =
    repackBuf |>> (consumeBytes32 << 3)

  val robHeadValid =
    (robCount =/= 0) && robRespValid(robConsumePtr)

  val robHeadData =
    robData(robConsumePtr)

  val firstAppendBytes32 =
    Mux(srcOff === 0, BeatBytesU32, BeatBytesU32 - srcOff.resize(32))

  val rawAppendBytes32 =
    Mux(firstReadBeat, firstAppendBytes32, BeatBytesU32)

  val remainToAppend32 =
    totalSize - bytesAppended

  val appendBytes32 =
    Mux(remainToAppend32 <= rawAppendBytes32, remainToAppend32, rawAppendBytes32)

  val appendData =
    Mux(firstReadBeat, robHeadData |>> (srcOff << 3), robHeadData)

  val robPopFire =
    task_valid &&
      robHeadValid &&
      (appendBytes32 =/= 0) &&
      ((repackValidAfterConsume32 + appendBytes32) <= U(2 * BeatBytes, 32 bits))

  when(robPopFire) {
    repackBuf :=
      repackBufAfterConsume |
        (appendData.resize(2 * MMUDataWidth bits) |<< (repackValidAfterConsume32 << 3))

    repackValidBytes :=
      (repackValidAfterConsume32 + appendBytes32).resize(repackValidBytes.getWidth)

    bytesAppended := bytesAppended + appendBytes32

    robRespValid(robConsumePtr) := False
    robConsumePtr := WrapInc(robConsumePtr, GCCopyEntry, U(1))

    when(firstReadBeat) {
      firstReadBeat := False
    }
  } elsewhen(writeFire) {
    repackBuf := repackBufAfterConsume
    repackValidBytes := repackValidAfterConsume32.resize(repackValidBytes.getWidth)
  }

  // ==========================================================================
  // robCount bookkeeping
  // - request alloc increments
  // - in-order consume decrements
  // ==========================================================================
  switch(Cat(readReqFire, robPopFire)) {
    is(B"10") {
      robCount := robCount + 1
    }
    is(B"01") {
      robCount := robCount - 1
    }
  }

  // ==========================================================================
  // write response is ignored
  // ==========================================================================
  io.writeMReq.Response.ready := True
}

object GCCopy2Verilog extends App {
  Config.spinal.generateVerilog(new GCCopy())
}
