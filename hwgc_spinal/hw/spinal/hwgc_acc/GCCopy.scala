package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/**
 * Combinational lookup port from Fetch into the active Copy store buffer.
 *
 * Request:
 *   valid / addr / size
 *
 * Response:
 *   stall = at least one requested byte belongs to the active destination
 *           range, has not committed to memory, and has not returned from the
 *           source read yet.
 *   mask  = byte lanes that can be forwarded immediately.
 *   data  = forwarded bytes in their request-relative lane positions.
 *
 * Fetch may:
 *   - bypass MMU when mask covers the complete request;
 *   - issue MMU and merge mask/data into the response for partial forwarding;
 *   - wait when stall is asserted.
 */
case class GCCopyForwardPort()
  extends Bundle
    with IMasterSlave
    with HWParameters
    with GCTopParameters {
  val valid = Bool()
  val addr  = UInt(MMUAddrWidth bits)
  val size  = UInt(LineBytesNumBitSize bits)

  val stall = Bool()
  val mask  = Bits(LineBytesNum bits)
  val data  = UInt(MMUDataWidth bits)

  override def asMaster(): Unit = {
    out(valid, addr, size)
    in(stall, mask, data)
  }
}

/*
  Copy implementation:
    1. aligned source reads are issued in order and may return out of order;
    2. returned beats remain in ROB slots as a forwarding store buffer;
    3. destination writes are issued in logical copy order;
    4. slots are released only after write acknowledgements;
    5. committedBytes is therefore a precise contiguous visibility frontier.
 */
class GCCopy extends Component with HWParameters with GCParameters with GCTopParameters {
  val io = new Bundle {
    val ToCopy    = slave(new GCToCopy)
    val readMReq  = master(new LocalMMUIO)
    val writeMReq = master(new LocalMMUIO)

    val FwdMain = slave(GCCopyForwardPort())
    val FwdPush = slave(GCCopyForwardPort())
    val FwdPre  = slave(GCCopyForwardPort())
  }

  val BeatBytes    = LineBytesNum
  val BeatBytesU32 = U(BeatBytes, 32 bits)
  val OffBits      = log2Up(BeatBytes)
  val RobPtrBits   = log2Up(GCCopyEntry)
  val BeatLenBits  = log2Up(BeatBytes + 1)
  val WriteRespCntBits = 2

  def alignDown(x: UInt, width: Int): UInt =
    x & ~U(BeatBytes - 1, width bits)

  def genMask(byteLen: UInt, byteOffset: UInt): UInt = {
    val ret = UInt(BeatBytes bits)
    for (i <- 0 until BeatBytes) {
      ret(i) :=
        (U(i, BeatLenBits bits) >= byteOffset.resize(BeatLenBits)) &&
          (U(i, BeatLenBits bits) <
            (byteOffset.resize(BeatLenBits) + byteLen.resize(BeatLenBits)))
    }
    ret
  }

  val taskValid   = RegInit(False)
  val zeroTaskDone = RegInit(False)

  val srcPtr    = RegInit(U(0, GCElementWidth bits))
  val dstPtr    = RegInit(U(0, GCElementWidth bits))
  val totalSize = RegInit(U(0, 32 bits))

  val srcBase = RegInit(U(0, GCElementWidth bits))
  val srcOff  = RegInit(U(0, OffBits bits))

  // Source-read issue state.
  val readBeatCount = RegInit(U(0, 32 bits))
  val readReqIdx    = RegInit(U(0, 32 bits))

  // Destination write issue and visibility frontiers.
  val writeIssueIdx = RegInit(U(0, 32 bits))
  val writeCommitIdx = RegInit(U(0, 32 bits))
  val committedBytes = RegInit(U(0, 32 bits))

  // ROB / forwarding store-buffer slots.
  val slotBusy             = Vec.fill(GCCopyEntry)(RegInit(False))
  val slotRespValid        = Vec.fill(GCCopyEntry)(RegInit(False))
  val slotForwardValid     = Vec.fill(GCCopyEntry)(RegInit(False))
  val slotWriteIssued      = Vec.fill(GCCopyEntry)(RegInit(False))
  val slotPendingWriteResp = Vec.fill(GCCopyEntry)(RegInit(U(0, WriteRespCntBits bits)))
  val slotReqIdx           = Vec.fill(GCCopyEntry)(RegInit(U(0, 32 bits)))
  val slotData             = Vec.fill(GCCopyEntry)(RegInit(U(0, MMUDataWidth bits)))

  val sourceId2RobSlot =
    Reg(Vec(Seq.fill(LLCSourceMaxNum)(U(0, RobPtrBits bits))))
  val sourceId2WriteSlot =
    Reg(Vec(Seq.fill(LLCSourceMaxNum)(U(0, RobPtrBits bits))))

  // Per-slot logical mapping, used by both write generation and forwarding.
  val slotLogicalStart = Vec(UInt(32 bits), GCCopyEntry)
  val slotValidStart   = Vec(UInt(32 bits), GCCopyEntry)
  val slotValidBytes   = Vec(UInt(32 bits), GCCopyEntry)
  val slotDstStart     = Vec(UInt(GCElementWidth bits), GCCopyEntry)

  for (i <- 0 until GCCopyEntry) {
    val idx = slotReqIdx(i)
    val logicalOffset = (idx.resize(32) << OffBits).resize(32)

    slotLogicalStart(i) :=
      Mux(idx === 0, U(0, 32 bits), logicalOffset - srcOff.resize(32))
    slotValidStart(i) :=
      Mux(idx === 0, srcOff.resize(32), U(0, 32 bits))

    val maxValid = BeatBytesU32 - slotValidStart(i)
    val remain = Mux(
      slotLogicalStart(i) < totalSize,
      totalSize - slotLogicalStart(i),
      U(0, 32 bits)
    )

    slotValidBytes(i) := Mux(remain <= maxValid, remain, maxValid)
    slotDstStart(i) := (dstPtr + slotLogicalStart(i).resize(GCElementWidth))
      .resize(GCElementWidth)
  }

  // --------------------------------------------------------------------------
  // Task command and completion
  // --------------------------------------------------------------------------
  io.ToCopy.cmd.ready := !taskValid

  val anySlotBusy = slotBusy.asBits.orR
  val taskDoneNow =
    taskValid &&
      (readReqIdx === readBeatCount) &&
      (writeCommitIdx === readBeatCount) &&
      !anySlotBusy

  io.ToCopy.Done := zeroTaskDone || taskDoneNow

  when(io.ToCopy.cmd.fire) {
    srcPtr    := io.ToCopy.cmd.payload.SrcOopPtr
    dstPtr    := io.ToCopy.cmd.payload.DestOopPtr
    totalSize := io.ToCopy.cmd.payload.Size

    srcBase := alignDown(io.ToCopy.cmd.payload.SrcOopPtr, GCElementWidth)
    srcOff  := io.ToCopy.cmd.payload.SrcOopPtr(OffBits - 1 downto 0)

    readReqIdx     := 0
    readBeatCount  := 0
    writeIssueIdx  := 0
    writeCommitIdx := 0
    committedBytes := 0

    for (i <- 0 until GCCopyEntry) {
      slotBusy(i)             := False
      slotRespValid(i)        := False
      slotForwardValid(i)     := False
      slotWriteIssued(i)      := False
      slotPendingWriteResp(i) := 0
      slotReqIdx(i)           := 0
      slotData(i)             := 0
    }

    taskValid := True

    when(io.ToCopy.cmd.payload.Size === 0) {
      zeroTaskDone := True
    }.otherwise {
      zeroTaskDone := False
      readBeatCount :=
        ((io.ToCopy.cmd.payload.SrcOopPtr(OffBits - 1 downto 0) +
          io.ToCopy.cmd.payload.Size +
          U(BeatBytes - 1, 32 bits)) >> OffBits).resize(32)
    }
  }.elsewhen(zeroTaskDone) {
    taskValid    := False
    zeroTaskDone := False
  }.elsewhen(taskDoneNow) {
    taskValid := False
  }

  // --------------------------------------------------------------------------
  // Source reads: in-order issue, out-of-order response
  // --------------------------------------------------------------------------
  val freeMask = Bits(GCCopyEntry bits)
  for (i <- 0 until GCCopyEntry) {
    freeMask(i) := !slotBusy(i)
  }

  val hasFreeSlot = freeMask.orR
  val allocOH      = OHMasking.first(freeMask)
  val allocSlot    = OHToUInt(allocOH).resize(RobPtrBits)

  val readCanAlloc =
    taskValid && (readReqIdx =/= readBeatCount) && hasFreeSlot
  val curReadAddr =
    (srcBase + (readReqIdx.resize(GCElementWidth) |<< OffBits))
      .resize(MMUAddrWidth)

  io.readMReq.Request.valid := readCanAlloc
  io.readMReq.Request.payload.RequestVirtualAddr := curReadAddr
  io.readMReq.Request.payload.RequestSourceID :=
    io.readMReq.ConherentRequsetSourceID.payload
  io.readMReq.Request.payload.RequestType_isWrite := False
  io.readMReq.Request.payload.RequestWStrb         := 0
  io.readMReq.Request.payload.RequestData          := 0
  io.readMReq.Request.payload.RequestSize :=
    U(BeatBytes, LineBytesNumBitSize bits)
  io.readMReq.Request.payload.NeedResponse  := True
  io.readMReq.Request.payload.NeedDoCmpxChg := False

  when(io.readMReq.Request.fire) {
    val sid = io.readMReq.ConherentRequsetSourceID.payload.resized

    sourceId2RobSlot(sid) := allocSlot

    slotBusy(allocSlot)             := True
    slotRespValid(allocSlot)        := False
    slotForwardValid(allocSlot)     := False
    slotWriteIssued(allocSlot)      := False
    slotPendingWriteResp(allocSlot) := U(0).resized
    slotReqIdx(allocSlot)           := readReqIdx

    readReqIdx := readReqIdx + 1
  }

  val readRespSid  = io.readMReq.Response.payload.ResponseSourceID.resized
  val readRespSlot = sourceId2RobSlot(readRespSid)

  io.readMReq.Response.ready :=
    taskValid && slotBusy(readRespSlot) && !slotForwardValid(readRespSlot)

  when(io.readMReq.Response.fire) {
    slotData(readRespSlot)         := io.readMReq.Response.payload.ResponseData
    slotRespValid(readRespSlot)    := True
    slotForwardValid(readRespSlot) := True
  }

  // --------------------------------------------------------------------------
  // In-order destination write issue
  // --------------------------------------------------------------------------
  val issueCandidates = Vec(Bool(), GCCopyEntry)
  for (i <- 0 until GCCopyEntry) {
    issueCandidates(i) :=
      slotBusy(i) &&
        slotRespValid(i) &&
        !slotWriteIssued(i) &&
        (slotReqIdx(i) === writeIssueIdx)
  }

  val hasIssueSlot = issueCandidates.asBits.orR
  val issueSlot =
    OHToUInt(OHMasking.first(issueCandidates.asBits)).resize(RobPtrBits)

  val writeActive = RegInit(False)
  val writeSecond = RegInit(False)
  val writeSlot   = RegInit(U(0, RobPtrBits bits))
  val needWrite1 = RegInit(False)

  val writeAddr0 = RegInit(U(0, MMUAddrWidth bits))
  val writeAddr1 = RegInit(U(0, MMUAddrWidth bits))
  val writeData0 = RegInit(U(0, MMUDataWidth bits))
  val writeData1 = RegInit(U(0, MMUDataWidth bits))
  val writeStrb0 = RegInit(U(0, BeatBytes bits))
  val writeStrb1 = RegInit(U(0, BeatBytes bits))
  val writeSize0 = RegInit(U(0, LineBytesNumBitSize bits))
  val writeSize1 = RegInit(U(0, LineBytesNumBitSize bits))

  when(taskValid && hasIssueSlot && !writeActive) {
    val data = slotData(issueSlot)
    val validStartInBeat = slotValidStart(issueSlot)
    val validBytes       = slotValidBytes(issueSlot)
    val shiftedData      = data |>> (validStartInBeat << 3)

    val dstByteAddr =
      dstPtr + slotLogicalStart(issueSlot).resize(GCElementWidth)
    val dstAddrAligned =
      alignDown(dstByteAddr, GCElementWidth).resize(MMUAddrWidth)
    val dstOff = dstByteAddr(OffBits - 1 downto 0)
    val space0 = BeatBytesU32 - dstOff.resize(32)
    val len0   = Mux(validBytes <= space0, validBytes, space0)
    val len1   = validBytes - len0

    writeSlot  := issueSlot
    writeAddr0 := dstAddrAligned
    writeData0 := (shiftedData |<< (dstOff << 3)).resized
    writeStrb0 := genMask(len0.resize(BeatLenBits), dstOff)
    writeSize0 := len0.resize(LineBytesNumBitSize)

    writeAddr1 := (dstAddrAligned + U(BeatBytes, MMUAddrWidth bits)).resized
    writeData1 := (shiftedData |>> (len0.resize(BeatLenBits) << 3)).resized
    writeStrb1 := genMask(len1.resize(BeatLenBits), U(0, OffBits bits))
    writeSize1 := len1.resize(LineBytesNumBitSize)

    needWrite1 := len1 =/= 0
    writeActive := True
    writeSecond := False

    // It remains forwarding-valid until acknowledgement and in-order commit.
    slotRespValid(issueSlot) := False
  }

  io.writeMReq.Request.valid := writeActive
  io.writeMReq.Request.payload.RequestVirtualAddr :=
    Mux(writeSecond, writeAddr1, writeAddr0)
  io.writeMReq.Request.payload.RequestSourceID :=
    io.writeMReq.ConherentRequsetSourceID.payload
  io.writeMReq.Request.payload.RequestType_isWrite := True
  io.writeMReq.Request.payload.RequestWStrb :=
    Mux(writeSecond, writeStrb1, writeStrb0)
  io.writeMReq.Request.payload.RequestData :=
    Mux(writeSecond, writeData1, writeData0)
  io.writeMReq.Request.payload.RequestSize :=
    Mux(writeSecond, writeSize1, writeSize0)
  io.writeMReq.Request.payload.NeedResponse  := True
  io.writeMReq.Request.payload.NeedDoCmpxChg := False

  val writeReqFire  = io.writeMReq.Request.fire
  val writeRespFire = io.writeMReq.Response.fire

  val writeRespSid  = io.writeMReq.Response.payload.ResponseSourceID.resized
  val writeRespSlot = sourceId2WriteSlot(writeRespSid)

  io.writeMReq.Response.ready := True

  when(writeReqFire) {
    val sid = io.writeMReq.ConherentRequsetSourceID.payload.resized
    sourceId2WriteSlot(sid) := writeSlot

    when(!writeSecond && needWrite1) {
      writeSecond := True
    }.otherwise {
      writeActive := False
      writeSecond := False
      needWrite1  := False

      slotWriteIssued(writeSlot) := True
      writeIssueIdx := writeIssueIdx + 1
    }
  }

  // Per-slot acknowledgement counters. One request and one response may occur
  // in the same cycle, including for different slots.
  for (i <- 0 until GCCopyEntry) {
    val inc = writeReqFire && writeSlot === U(i, RobPtrBits bits)
    val dec = writeRespFire && writeRespSlot === U(i, RobPtrBits bits)

    when(inc && !dec) {
      slotPendingWriteResp(i) := slotPendingWriteResp(i) + 1
    }.elsewhen(!inc && dec) {
      slotPendingWriteResp(i) := slotPendingWriteResp(i) - 1
    }
  }

  // --------------------------------------------------------------------------
  // In-order visibility commit
  // --------------------------------------------------------------------------
  val commitCandidates = Vec(Bool(), GCCopyEntry)
  for (i <- 0 until GCCopyEntry) {
    commitCandidates(i) :=
      slotBusy(i) &&
        slotWriteIssued(i) &&
        (slotPendingWriteResp(i) === 0) &&
        (slotReqIdx(i) === writeCommitIdx)
  }

  val hasCommitSlot = commitCandidates.asBits.orR
  val commitSlot =
    OHToUInt(OHMasking.first(commitCandidates.asBits)).resize(RobPtrBits)

  when(taskValid && hasCommitSlot) {
    committedBytes := committedBytes + slotValidBytes(commitSlot)
    writeCommitIdx := writeCommitIdx + 1

    slotBusy(commitSlot)             := False
    slotRespValid(commitSlot)        := False
    slotForwardValid(commitSlot)     := False
    slotWriteIssued(commitSlot)      := False
    slotPendingWriteResp(commitSlot) := U(0).resized
  }

  // --------------------------------------------------------------------------
  // Copy store-buffer lookup
  // --------------------------------------------------------------------------
  def driveForwardLookup(port: GCCopyForwardPort): Unit = {
    port.stall := False
    port.mask  := 0
    port.data  := 0

    val copyEnd      = (dstPtr + totalSize.resize(GCElementWidth)).resize(GCElementWidth)
    val committedEnd = (dstPtr + committedBytes.resize(GCElementWidth)).resize(GCElementWidth)

    for (byte <- 0 until BeatBytes) {
      val laneRequested =
        port.valid &&
          (U(byte, LineBytesNumBitSize bits) <
            port.size.resize(LineBytesNumBitSize))

      val reqByteAddr =
        (port.addr + U(byte, MMUAddrWidth bits)).resize(MMUAddrWidth)
      val reqByteAddrGc = reqByteAddr.resize(GCElementWidth)

      val inCopyRange =
        taskValid &&
          (reqByteAddrGc >= dstPtr) &&
          (reqByteAddrGc < copyEnd)

      val alreadyCommitted =
        inCopyRange && (reqByteAddrGc < committedEnd)

      val hitVec  = Vec(Bool(), GCCopyEntry)
      val byteVec = Vec(UInt(8 bits), GCCopyEntry)

      for (slot <- 0 until GCCopyEntry) {
        val slotEnd =
          (slotDstStart(slot) + slotValidBytes(slot).resize(GCElementWidth))
            .resize(GCElementWidth)

        hitVec(slot) :=
          slotBusy(slot) &&
            slotForwardValid(slot) &&
            (reqByteAddrGc >= slotDstStart(slot)) &&
            (reqByteAddrGc < slotEnd)

        val byteOffset =
          (reqByteAddrGc - slotDstStart(slot)).resize(OffBits)
        val sourceByteIndex =
          (slotValidStart(slot).resize(OffBits) + byteOffset).resize(OffBits)

        byteVec(slot) := slotData(slot).subdivideIn(8 bits)(sourceByteIndex)
      }

      val hit = hitVec.asBits.orR
      val forwardedByte = MuxOH(hitVec.asBits, byteVec)

      when(laneRequested && hit) {
        port.mask(byte) := True
        port.data(byte * 8 + 7 downto byte * 8) := forwardedByte
      }

      when(laneRequested && inCopyRange && !alreadyCommitted && !hit) {
        port.stall := True
      }
    }
  }

  driveForwardLookup(io.FwdMain)
  driveForwardLookup(io.FwdPush)
  driveForwardLookup(io.FwdPre)

  val counter = RegInit(U(0, 64 bits))
  when(taskValid) {
    counter := counter + 1
  }
}

object GCCopy2Verilog extends App {
  Config.spinal.generateVerilog(new GCCopy())
}