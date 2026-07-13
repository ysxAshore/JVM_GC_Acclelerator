package hwgc_acc

import hwgc_top.{Config, HWParameters, LocalMMUIO}

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

/*
 * 非对齐 MMU 请求适配器
 * 主要功能：
 * 1. 对齐请求以及CAS请求直接发送到下游；
 * 2. 未跨边界的非对齐请求转换为一个按 Line 对齐的请求；
 * 3. 跨 Line 的非对齐请求拆成两个请求 读请求返回两个响应后，重新拼接成原始数据；
 * 5. NeedResponse=False 时，请求发送完成即认为事务完成；如果下游仍为无响应请求返回响应，则记录并丢弃这些响应。
 *
 * @param downstreamReturnsResponse
 *   true：下游仍为 NeedResponse=False 的请求返回响应；
 *   false：下游保证不会返回Write这类响应。
 */
class GCUnalignedMMUAdapter(downstreamReturnsResponse: Boolean = true) extends Component with HWParameters {
  val io = new Bundle {
    val in  = slave(new LocalMMUIO)
    val out = master(new LocalMMUIO)
  }

  // 不向上游提供请求SourceID
  io.in.ConherentRequsetSourceID.valid := False
  io.in.ConherentRequsetSourceID.payload.clearAll()

  io.in.Response.valid := False
  io.in.Response.payload.clearAll()
  io.out.Request.payload.clearAll()

  val busy             = RegInit(False)
  val splitReq         = RegInit(False) // 当前事务是否被拆成两个下游请求发送
  val sendBeat0Pending = RegInit(False) // 第一个、第二个下游请求是否还在等待发送
  val sendBeat1Pending = RegInit(False)
  val gotOneResp       = RegInit(False) // 拆分事务已经得到了一个响应

  // 锁存上游请求信息 请求被接收后 后续的拆分以及响应的拼接均使用这些寄存器
  val size          = RegInit(U(0, LineBytesNumBitSize bits))
  val addr          = RegInit(U(0, MMUAddrWidth bits))
  val reqData       = RegInit(U(0, MMUDataWidth bits))
  val isWrite       = RegInit(False)
  val needResponse  = RegInit(False)
  val needDoCmpXchg = RegInit(False)

  // 记录拆分请求的SourceID 用于识别响应属于第一个还是第二个请求
  val firstSourceID  = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val secondSourceID = RegInit(U(0, LLCSourceMaxNumBitSize bits))

  // 保存拆分读事务中先返回的那个响应 以及这个响应是beat0还是beat1的
  val bufferData    = RegInit(U(0, MMUDataWidth bits))
  val bufferIsBeat0 = RegInit(False)

  // 最终的响应缓存 当io.in.Response.ready=False时 数据先会保存在这里 等待上游接收
  val respBufValid   = RegInit(False)
  val respBufPayload = Reg(cloneOf(io.in.Response.payload))
  respBufPayload.init(io.in.Response.payload.getZero)

  // 当下游受到NeedResponse=False请求时 仍会返回Response
  // 这些Response在这里丢弃 定义2bit记录同一SourceID下等待丢弃的响应数量
  val sourceCount = 1 << log2Up(LLCSourceMaxNum)
  val ignoredNoRespCounts =
    if (downstreamReturnsResponse) {
      Vec.fill(sourceCount)(RegInit(False))
    } else {
      null
    }

  // 判断指定 SourceID 是否存在等待丢弃的无响应请求 Response
  def ignoredNoRespSourceHit(id: UInt): Bool = {
    if (downstreamReturnsResponse) {
      ignoredNoRespCounts(id)
    } else {
      False
    }
  }

  // 地址计算部分
  val offset      = addr(log2Up(LineBytesNum) - 1 downto 0)
  val alignedAddr = addr & ~U(LineBytesNum - 1, addr.getWidth bits)
  val crossBeat   = offset + size > LineBytesNum // 从offset读size大小是否超过了一个line
  val firstBytes  = Mux(crossBeat, LineBytesNum - offset, size) // 第一个Line中实际访问的字节数
  val secondBytes = size - firstBytes

  // 直通请求 不用进行拆分
  //     对齐的访问 且 没有超过一个Line
  val inOffsetNow      = io.in.Request.payload.RequestVirtualAddr(log2Up(LineBytesNum) - 1 downto 0)
  val inCrossBeatNow   = inOffsetNow + io.in.Request.payload.RequestSize > LineBytesNum
  val alignedAccessNow = inOffsetNow === 0 && !inCrossBeatNow

  //     cas 直通请求
  // val cmpxchgNow = io.in.Request.payload.NeedDoCmpxChg
  val cmpxchgNow      = False
  val directAccessNow = alignedAccessNow || cmpxchgNow

  // 当前没有在处理事务 且 没有保存的ready未到时的Resp缓存才允许接收一个新的in请求
  // 下游保证一个 SourceID 对应的旧响应返回前， 不会将该 SourceID 分配给新的事务。
  // 因此，NeedResponse=False 请求发送完成后，可以立即接收新事务， 不需要等待 ignoredNoRespCounts 中记录的响应全部返回。
  // 尚未返回的无响应请求 Response 仍由 ignoredNoRespCounts 识别并丢弃 新事务会获得不同的 SourceID，因此不会与旧响应发生混淆
  val canStartNewReq = !busy && !respBufValid
  val directReqNow   = canStartNewReq && io.in.Request.valid && directAccessNow // 丢弃输入是否为能够发送的直通请求

  // 直通请求因为不会进入待发送的锁存器 所以必须在out ready时才能接收
  // 非直通请求 可以先锁存 所以不需要out ready
  io.in.Request.ready := canStartNewReq && (!directAccessNow || io.out.Request.ready)

  // 锁存上游请求信息
  when(io.in.Request.fire) {
    size          := io.in.Request.payload.RequestSize
    addr          := io.in.Request.payload.RequestVirtualAddr
    reqData       := io.in.Request.payload.RequestData
    isWrite       := io.in.Request.payload.RequestType_isWrite
    needResponse  := io.in.Request.payload.NeedResponse
    needDoCmpXchg := io.in.Request.payload.NeedDoCmpxChg

    // 跨 Line 且非 CAS 的请求需要拆成两个 beat
    splitReq := inCrossBeatNow && !cmpxchgNow

    // 直通请求 在 本周期 即可发送 不需要设置待发送状态
    when(directAccessNow) {
      sendBeat0Pending := False
      sendBeat1Pending := False
    } otherwise {
      // 所有 经过 适配的 非对齐请求 都需要 beat0发送
      sendBeat0Pending := True
      // 只有跨Line才需要Beat1
      sendBeat1Pending := inCrossBeatNow
    }

    gotOneResp := False
    busy := True
  }

  // 下游请求发送状态: 优先发送beat0
  val issueBeat0 = busy && sendBeat0Pending
  val issueBeat1 = busy && !sendBeat0Pending && sendBeat1Pending
  val outSourceID = io.out.ConherentRequsetSourceID.payload // 下游为当前请求分配或提供的SourceID

  io.out.Request.valid := issueBeat0 || issueBeat1 || directReqNow


  // 直通请求 直接管道设置
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

  // 拆分写数据和写掩码
  val writeData0 = reqData |<< (offset << 3)
  val writeData1 = reqData |>> (firstBytes << 3)
  val mask0      = ((U(1, LineBytesNum bits) |<< firstBytes) - 1) |<< offset
  val mask1      = (U(1, LineBytesNum bits) |<< secondBytes) - 1

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

  // 记录请求的sourceID 以便Resp的对应
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

  // 对于无响应事务 不需要等待Response 只需要发送完请求即可
  val noRespDirectDone = io.out.Request.fire && directReqNow && !io.in.Request.payload.NeedResponse
  val noRespAdaptedDone = io.out.Request.fire && !directReqNow && !needResponse && (issueBeat1 || (issueBeat0 && !sendBeat1Pending))
  val noRespTxnDone = noRespDirectDone || noRespAdaptedDone

  // 无响应事务发送完成 释放当前事务状态
  when(noRespTxnDone) { 
    busy := False
    splitReq := False
    gotOneResp := False
    sendBeat0Pending := False
    sendBeat1Pending := False
  }

  // 根据out的Resp SourceID 判断当前得到的是哪次Beat的响应
  val respSourceID = io.out.Response.payload.ResponseSourceID
  val respIsBeat0  = respSourceID === firstSourceID
  val respIsBeat1  = respSourceID === secondSourceID

  val currentRespHit = Bool()
  currentRespHit := False

  when(busy && needResponse) {
    when(!splitReq) { 
      // 未拆分事务 beat0必须已经发送 且SourceID匹配
      currentRespHit := !sendBeat0Pending && respIsBeat0
    } otherwise {
      // 没有收到过响应
      when(!gotOneResp) {
        // 接收Beat0: Beat0已经发送了 且 resp SourceID匹配
        // 接收Bear1: Beat1已经发送了 且 resp SourceID匹配
        currentRespHit := !sendBeat0Pending && (respIsBeat0 || (!sendBeat1Pending && respIsBeat1))
      } otherwise {
        // 已接受一个响应 那么此时必须两个请求都发送完成 且 当前响应必须是另外一个Beat
        currentRespHit := !sendBeat0Pending && !sendBeat1Pending && Mux(bufferIsBeat0, respIsBeat1, respIsBeat0)
      }
    }
  }

  // 当前响应是否是之前的NeedResponse=False请求
  val ignoredRespHit = ignoredNoRespSourceHit(respSourceID.resized)
  // 第一个Split请求是否可以接收: 只需要保证respBuf无效且Beat0已发送且没有得到过响应
  val firstSplitRespCanArrive = splitReq && !gotOneResp && !sendBeat0Pending && !respBufValid
  // 要么是不分裂的 请求都发出 且resp无效 要么是分裂的但已经得到了一次响应
  val finalRespCanArrive = (!splitReq && !sendBeat0Pending && !sendBeat1Pending && !respBufValid) ||
      (splitReq && gotOneResp && !sendBeat0Pending && !sendBeat1Pending && !respBufValid)

  // 当前事务的响应可以被接收
  val currentRespCanTakeRaw = busy && needResponse && currentRespHit && (firstSplitRespCanArrive || finalRespCanArrive)

  // 是否需要丢弃当前响应
  val ignoredRespCanDrop =
    if (downstreamReturnsResponse) {
      ignoredRespHit
    } else {
      False
    }

  // 最终该Resp SourceID请求是否可以被接收
  val currentRespCanTake = currentRespCanTakeRaw && !ignoredRespCanDrop

  // 要么可以接受 要么是被丢弃了
  io.out.Response.ready := currentRespCanTake || ignoredRespCanDrop

  val currentRespFire = io.out.Response.fire && currentRespCanTake
  val ignoredRespFire = io.out.Response.fire && ignoredRespCanDrop

  val noRespReqBeatFire = io.out.Request.fire && ((directReqNow && !io.in.Request.payload.NeedResponse) || (!directReqNow && !needResponse))

  // 统计 无响应请求 已经发送的Beat数量(noRespBearFire and ignoredRespFire)
  if (downstreamReturnsResponse) {
    for (i <- 0 until sourceCount) {
      val incHit = noRespReqBeatFire && outSourceID === U(i, LLCSourceMaxNumBitSize bits)
      val decHit = ignoredRespFire && respSourceID === U(i, LLCSourceMaxNumBitSize bits)

      when(incHit && !decHit) {
        ignoredNoRespCounts(i) := True
      }.elsewhen(!incHit && decHit) {
        ignoredNoRespCounts(i) := False
      }
    }
  }

  // 判断是否得到了事务的最终响应
  // 非拆分事务 当前响应就是最终响应
  // 拆分事务 在得到一个响应后的第二个响应Fire时才是最终响应
  val finalRespArrive = currentRespFire && (!splitReq || gotOneResp)
  val needUpResp      = needResponse
  val finalRespNow    = finalRespArrive && needUpResp // 当前Resp是否传递到上游

  // 构造发往上游的响应
  val bypassRespPayload = cloneOf(io.in.Response.payload)
  bypassRespPayload.ResponseSourceID := io.out.Response.payload.ResponseSourceID
  bypassRespPayload.ResponseData := io.out.Response.payload.ResponseData

  when(currentRespFire) {
    when(!splitReq) {
      when(!isWrite || needDoCmpXchg) { 
        // 恢复 对应的请求数据
        bypassRespPayload.ResponseData := io.out.Response.payload.ResponseData |>> (offset << 3)
      }
    } otherwise {
      // 已经缓存过一个响应
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

  // 向上游输出响应:已缓存的响应优先级高于本周期刚收到的旁路响应
  // respBuf有效时 从respBuf中得到数据
  when(respBufValid) {
    io.in.Response.valid := True
    io.in.Response.payload := respBufPayload
  } otherwise { // 无效时 从byPassRespPayload中得到数据
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
      // 要么没有分割 要么已经得到了一个响应 说明这是事务的最终响应
      // 此时如果需要给上游Resp, 则在respBuf无效且in.Response.ready没有时 可以保存在respBufValid中
      when(needUpResp) {
        when(!respBufValid && !io.in.Response.ready) {
          respBufPayload := bypassRespPayload
          respBufValid := True
        }
      }

      // 复位状态
      busy := False
      splitReq := False
      gotOneResp := False
    }
  }
}

object GCUnalignedMMUAdapterVerilog extends App {
  Config.spinal.generateVerilog(new GCUnalignedMMUAdapter())
}
