package hwgc_top

import spinal.core._
import spinal.core.sim.SimConfig
import spinal.lib._

import scala.language.postfixOps

object Config {
  def spinal = SpinalConfig(
    targetDirectory = "sim/vsrc",
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = HIGH
    )
  )
  def sim = SimConfig.withConfig(spinal).withFstWave
}

object WrapInc {
  def apply(value: UInt, n: Int, increment: UInt): UInt = {
    val width = log2Up(n max 2)

    if (isPow2(n)) {
      (value + increment)(width - 1 downto 0)
    } else {
      val sum = value.resize(width + 1) + increment.resize(width + 1)
      val ret = UInt(width bits)

      when(sum >= U(n, width + 1 bits)) {
        ret := (sum - U(n, width + 1 bits)).resize(width)
      }.otherwise {
        ret := sum.resize(width)
      }

      ret
    }
  }
}

object WrapDec {
  // value valid range: 0 ~ n-1
  // decrement valid range: 0 ~ n-1
  // result = (value - decrement) mod n
  def apply(value: UInt, n: Int, decrement: UInt): UInt = {
    require(n >= 1)

    if (n == 1) {
      U(0, 1 bits)
    } else {
      val width = log2Up(n)

      val v = value.resize(width)
      val d = decrement.resize(width)

      if (isPow2(n)) {
        (v - d)(width - 1 downto 0)
      } else {
        val noWrap = v >= d
        val subNoWrap = v - d
        val subWrap = (v.resize(width + 1) + U(n, width + 1 bits) - d.resize(width + 1)).resize(width)

        Mux(noWrap, subNoWrap, subWrap)
      }
    }
  }
}

trait HWParameters {
  // true is boolean, True is Bool
  val DebugEnable = true

  val MMUAddrWidth = 64
  val MMUDataWidth = 256

  val LineBytesNum = MMUDataWidth / 8
  val LineBytesNumBitSize = log2Up(LineBytesNum) + 1

  val LLCSourceMaxNum = 32
  val LLCSourceMaxNumBitSize = log2Up(LLCSourceMaxNum) + 1

  // helper: issueReq
  def issueReq(port: LocalMMUIO, addr: UInt, write: Bool, size: UInt, writeData: UInt, needResponse: Bool, needDoCmpXchg: Bool, reqIssued: Bool)(onResp: UInt => Unit): Unit = {

    // ensure default safe values (caller may have already set globals; safe to reassign)
    port.Request.valid := !reqIssued

    when(!reqIssued) {
      port.Request.payload.RequestVirtualAddr := addr
      port.Request.payload.RequestSourceID := port.ConherentRequsetSourceID.payload
      port.Request.payload.RequestType_isWrite := write
      port.Request.payload.RequestWStrb := getWstrb(size.resize(LineBytesNumBitSize))
      port.Request.payload.RequestData := writeData.resize(MMUDataWidth)
      port.Request.payload.RequestSize := size.resize(LineBytesNumBitSize)
      port.Request.payload.NeedResponse := needResponse
      port.Request.payload.NeedDoCmpxChg := needDoCmpXchg
    }

    when(port.Request.fire) {
      reqIssued := True
    }

    when(port.Response.fire) {
      val rd = port.Response.payload.ResponseData
      reqIssued := False
      onResp(rd) // callback to let caller handle response data
    }
  }

  def getWstrb(bytes: UInt): UInt = {
    val mask = Bits(LineBytesNum bits)
    mask := Mux(bytes >= U(LineBytesNum),
      B(LineBytesNum bits, default -> true),
      ((U(1) << bytes.resize(LineBytesNumBitSize)) - U(1)).asBits.resize(LineBytesNum)
    )
    mask.asUInt
  }
}

trait GCTopParameters {
  val GCElementWidth = 64 // 和MMUAddrWidth一样
}

class GCAllocCacheUpdate extends Bundle with GCTopParameters with HWParameters {
  val regionPtr = UInt(GCElementWidth bits)
  val region = UInt(GCElementWidth bits)
}

class LocalMMUIO extends Bundle with HWParameters with IMasterSlave {
  // 发出的访存请求
  val Request = master Stream new Bundle {
    val RequestSourceID = UInt(LLCSourceMaxNumBitSize bits)
    val RequestVirtualAddr = UInt(MMUAddrWidth bits)
    val RequestType_isWrite = Bool()
    val RequestData = UInt(MMUDataWidth bits)
    val RequestWStrb = UInt(LineBytesNum bits)
    val RequestSize = UInt(LineBytesNumBitSize bits)
    val NeedResponse = Bool()
    val NeedDoCmpxChg = Bool()
  }

  val ConherentRequsetSourceID = slave Flow UInt(LLCSourceMaxNumBitSize bits)

  val Response = slave Stream new Bundle {
    val ResponseData = UInt(MMUDataWidth bits)
    val ResponseSourceID = UInt(LLCSourceMaxNumBitSize bits)
  }

  override def asMaster(): Unit = {
    master(Request)
    slave(Response, ConherentRequsetSourceID)
  }
}

class Ctrl2Top extends Bundle with GCTopParameters with IMasterSlave {
  val ChunkSize = in UInt (32 bits)
  val AgeThreshold = in UInt (32 bits)
  val HeapRegionBias = in UInt (32 bits)
  val RegionAttrShiftBy = in UInt (32 bits)
  val HeapRegionShiftBy = in UInt (32 bits)
  val LogOfHRGrainBytes = in UInt (32 bits)
  val StepperOffset = in UInt (GCElementWidth bits)
  val YoungWordsBase = in UInt (GCElementWidth bits)
  val RegionAttrBase = in UInt (GCElementWidth bits)
  val PlabAllocatorPtr = in UInt (GCElementWidth bits)
  val RegionAttrBiasedBase = in UInt (GCElementWidth bits)
  val HeapRegionBiasedBase = in UInt (GCElementWidth bits)
  val ParScanThreadStatePtr = in UInt (GCElementWidth bits)
  val TaskQueue_Bottom = in UInt (32 bits)
  val TaskQueue_ElemsBase = in UInt (GCElementWidth bits)
  val HumongousReclaimCandidatesBoolBase = in UInt (GCElementWidth bits)
  val CardTablePtr = in UInt (GCElementWidth bits)
  val G1h = in UInt (GCElementWidth bits)
  val IntArrayKlassObj = in UInt (GCElementWidth bits)
  val ObjectKlass = in UInt (GCElementWidth bits)
  val LockPtr = in UInt (GCElementWidth bits)
  val Thread = in UInt (GCElementWidth bits)
  val DummyRegion = in UInt (GCElementWidth bits)
  val CompressedOopBase = in UInt (GCElementWidth bits)
  val CompressedKlassPointerBase = in UInt (GCElementWidth bits)
  val CompressedFlag = in UInt (32 bits)

  val Valid = in Bool ()
  val Ready = out Bool ()
  val Done = out Bool ()

  override def asMaster(): Unit = {
    out(Valid, ChunkSize, AgeThreshold, HeapRegionBias, RegionAttrShiftBy,
      HeapRegionShiftBy, LogOfHRGrainBytes, StepperOffset, YoungWordsBase,
      RegionAttrBase, PlabAllocatorPtr, RegionAttrBiasedBase, HeapRegionBiasedBase,
      ParScanThreadStatePtr, TaskQueue_Bottom, TaskQueue_ElemsBase, HumongousReclaimCandidatesBoolBase,
      CardTablePtr, G1h, IntArrayKlassObj, ObjectKlass, LockPtr, Thread, DummyRegion,
      CompressedOopBase, CompressedKlassPointerBase, CompressedFlag)
    in(Ready, Done)
  }
}

class GCTopIO extends Bundle {
  val mmu2llc = master(new LocalMMUIO)
  val ctrl2top = slave(new Ctrl2Top)
}