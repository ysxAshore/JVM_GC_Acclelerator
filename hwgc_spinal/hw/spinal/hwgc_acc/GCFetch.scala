package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

case class GcFetchData() extends Bundle with GCTopParameters with GCParameters {
  val task      = UInt(GCElementWidth bits)
  val oopType   = UInt(GCOopTypeWidth bits)
  val fromObj   = UInt(GCElementWidth bits)
  val markWord  = UInt(GCElementWidth bits)
  val klassPtr  = UInt(GCElementWidth bits)
  val srcLength = UInt(32 bits)
}

// ============================================================================
// GCFetch — Read tasks from TaskStack, fetch OOP/MarkWord via MMU,
//           and dispatch to ArrayProcess or OopProcess.
//
// Three independent MMU ports:
//   MainMreq — used by mainFsm
//   PushMreq — used by pushFsm
//   PreMreq  — used by preFsm
// ============================================================================
class GCFetch extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val MainMreq           = master(new LocalMMUIO)
    val PushMreq           = master(new LocalMMUIO)
    val PreMreq            = master(new LocalMMUIO)

    val toFetch            = slave(new GCToFetch)
    val gcWriteSrcOopPtr   = slave(new GCWriteSrcOopPtr) // srcOopPtr updated MarkWord forward Path
    val Trace2Fetch        = slave Stream UInt(GCElementWidth bits) // LastPush forward Path
    val CopyDone           = in Bool() // 在CopyDone之后Push的才可以读，以确保读到的是新数据

    val Fetch2ArrayProcess = master(new GCToProcessUnit)
    val Fetch2OopProcess   = master(new GCToProcessUnit)
    val ConfigIO           = slave(new GCFetchConfigIO)
    val DebugTimeStamp     = in UInt(64 bits)
  }

  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid  := False
    m.Request.payload.clearAll()
    m.Response.ready := False
  }

  def driveReadReq(m: LocalMMUIO, addr: UInt, sizeBytes: UInt): Unit = {
    m.Request.valid := True

    m.Request.payload.NeedResponse        := True
    m.Request.payload.NeedDoCmpxChg       := False
    m.Request.payload.RequestSize         := sizeBytes.resize(LineBytesNumBitSize)
    m.Request.payload.RequestWStrb        := U(0)
    m.Request.payload.RequestData         := U(0)
    m.Request.payload.RequestType_isWrite := False
    m.Request.payload.RequestSourceID     := m.ConherentRequsetSourceID.payload
    m.Request.payload.RequestVirtualAddr  := addr
  }

  clearMreq(io.MainMreq)
  clearMreq(io.PushMreq)
  clearMreq(io.PreMreq)

  io.toFetch.Pop.ready    := False
  io.toFetch.PrePop.ready := False
  io.Trace2Fetch.ready    := False

  io.Fetch2ArrayProcess.clearOut()
  io.Fetch2OopProcess.clearOut()

  def receiveTask(payload: UInt, data: GcFetchData): Unit = {
    when(payload(GCOopTagWidth - 1 downto 0) === U(PartialArrayTag, GCOopTagWidth bits)) {
      data.oopType := U(PartialArrayOop)
      data.task    := payload - U(PartialArrayTag)
    }.otherwise {
      data.oopType := U(NotArrayOop, GCOopTypeWidth bits)
      data.task    := payload - payload(GCOopTagWidth - 1 downto 0)
    }
  }

  def decodeReadOopResp(rd: UInt): UInt = Mux(
    io.ConfigIO.UseCompressedOop,
    (io.ConfigIO.CompressedOopBase + (rd(31 downto 0).resize(GCElementWidth) << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth),
    rd(GCElementWidth - 1 downto 0)
  )

  def fillMwKlassLen(rd: UInt, data: GcFetchData): Unit = {
    data.markWord := rd(GCElementWidth - 1 downto 0)
    data.klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

    data.srcLength := Mux(
      io.ConfigIO.UseCompressedKlassPointers,
      rd(GCElementWidth * 2 - 1 downto GCElementWidth + 32),
      rd(GCElementWidth * 2 + 31 downto GCElementWidth * 2)
    )
  }

  def dbg(msg: Seq[Any]): Unit =
    if (DebugEnable) report(Seq("[GCFetch<", io.DebugTimeStamp, ">] ") ++ msg ++ Seq("\n"))

  def driveProcessUnit(target: GCToProcessUnit, payload: GcFetchData): Unit = {
    target.cmd.valid             := True
    target.cmd.payload.Task      := payload.task
    target.cmd.payload.OopType   := payload.oopType
    target.cmd.payload.SrcOopPtr := payload.fromObj
    target.cmd.payload.MarkWord  := payload.markWord
    target.cmd.payload.KlassPtr  := payload.klassPtr
    target.cmd.payload.SrcLength := payload.srcLength
  }

  val oopReadSize = U(8, LineBytesNumBitSize bits)
  val mwReadSize  = Mux(io.ConfigIO.UseCompressedKlassPointers, U(16), U(20)).resize(LineBytesNumBitSize)

  val main_data = RegInit(GcFetchData().getZero)
  val push_data = RegInit(GcFetchData().getZero)

  // PreFetch ring buffer
  val preBuf      = Vec.fill(PreFetchBufferNum)(RegInit(GcFetchData().getZero))
  val preBufDone  = Vec.fill(PreFetchBufferNum)(RegInit(False))
  val preBufMwHit = Vec.fill(PreFetchBufferNum)(RegInit(False))

  val buf_top    = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_bottom = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_count  = RegInit(U(0, PreFetchBufferWidth + 1 bits))

  val buf_capacity = U(PreFetchBufferNum, PreFetchBufferWidth + 1 bits)
  val buf_free     = buf_capacity - buf_count

  val buf_work = RegInit(U(0, PreFetchBufferWidth bits))

  // pushFollowRem 反映 Fetch 内部剩余的 push-follow PrePop 计数
  // Fetch 使用此寄存器将这些 PrePop 结果放置在 preBuf 中普通预取条目之前
  val pushFollowRem = RegInit(U(0, 32 bits))

  def bufInc(ptr: UInt, step: UInt): UInt = WrapInc(ptr, PreFetchBufferNum, step).resize(PreFetchBufferWidth)
  def bufDec(ptr: UInt, step: UInt): UInt = WrapDec(ptr, PreFetchBufferNum, step).resize(PreFetchBufferWidth)

  def resetSlot(idx: UInt): Unit = {
    preBufDone(idx)  := False
    preBufMwHit(idx) := False
  }

  val mainIsIdle     = Bool()
  val mainIsWaitDone = Bool()

  val pushIsIdle     = Bool()

  val mainGotoReadOop = Bool()
  val mainGotoSend    = Bool()

  mainGotoReadOop := False
  mainGotoSend    := False

  val targetDone = Mux(
    main_data.oopType === U(NotArrayOop),
    io.Fetch2OopProcess.Done,
    io.Fetch2ArrayProcess.Done
  )

  val targetDoneSeen = RegInit(False)
  when(targetDone && !mainIsWaitDone) {
    targetDoneSeen := True
  }

  val copyDoneSeen = RegInit(False)
  when(io.CopyDone) {
    copyDoneSeen := True
  }

  // mainFsm 弹出了一个任务，该任务的 PreFetch 条目存在但尚未完成 在这种情况下，mainFsm 会等待 preFsm 完成该 buf_bottom 槽位
  val waitForPrefetch = RegInit(False)

  // MarkWord forwarding
  val fwdValid = RegInit(False)
  val fwdObj   = RegInit(U(0, preBuf(0).fromObj.getWidth bits))
  val fwdValue = RegInit(U(0, GCElementWidth bits))

  when(io.gcWriteSrcOopPtr.writeForward.valid) {
    fwdValid := True
    fwdObj   := io.gcWriteSrcOopPtr.writeForward.payload.srcOopPtr
    fwdValue := io.gcWriteSrcOopPtr.writeForward.payload.writeValue
  }

  for (i <- 0 until PreFetchBufferNum) {
    when(io.gcWriteSrcOopPtr.writeForward.valid && preBufDone(i) && preBuf(i).fromObj === io.gcWriteSrcOopPtr.writeForward.payload.srcOopPtr) {
      preBuf(i).markWord := io.gcWriteSrcOopPtr.writeForward.payload.writeValue
      preBufMwHit(i)     := True
    }
  }

  // Main StateMachine
  val mainFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State
    val READ_OOP_RESP = new State
    val READ_MW_REQ   = new State
    val READ_MW_RESP  = new State
    val SEND          = new State
    val WAIT_DONE     = new State

    IDLE.whenIsActive {
      val fetchPushFollowActive = pushFollowRem =/= U(0, 32 bits) // pushFollowRem != 0 表示 preFsm 正在处理 push-follow PrePop 条目 prohibit the pop

      // Push状态机空闲且不存在新的Push,Main任务也不在等待PrePop做完，且没有未处理完的PrePop
      io.toFetch.Pop.ready := pushIsIdle && !io.Trace2Fetch.valid && !waitForPrefetch && !fetchPushFollowActive

      when(io.toFetch.Pop.fire) {
        val popBase = io.toFetch.Pop.payload - io.toFetch.Pop.payload(GCOopTagWidth - 1 downto 0)
        val bottomValid = buf_count =/= U(0, buf_count.getWidth bits)
        val bottomHit   = bottomValid && preBuf(buf_bottom).task === popBase // prePop hit

        when(bottomHit && preBufDone(buf_bottom)) {
          main_data := preBuf(buf_bottom)

          resetSlot(buf_bottom)
          buf_bottom := bufInc(buf_bottom, U(1, PreFetchBufferWidth bits))
          buf_count  := buf_count - U(1, buf_count.getWidth bits)

          goto(SEND)

        }.elsewhen(bottomHit) {
          // The corresponding PreFetch entry exists, but preFsm has not finished
          // reading MarkWord/Klass/Length yet. Wait until that slot becomes done.
          waitForPrefetch := True

        }.otherwise {
          receiveTask(io.toFetch.Pop.payload, main_data)
          main_data.fromObj := U(0)
          goto(READ_OOP_REQ)
        }

      }.elsewhen(waitForPrefetch && buf_count =/= U(0, buf_count.getWidth bits) && preBufDone(buf_bottom)) {
        waitForPrefetch := False
        main_data       := preBuf(buf_bottom)

        resetSlot(buf_bottom)
        buf_bottom := bufInc(buf_bottom, U(1, PreFetchBufferWidth bits))
        buf_count  := buf_count - U(1, buf_count.getWidth bits)

        goto(SEND)
      }
    }

    READ_OOP_REQ.whenIsActive {
      when(main_data.oopType === U(PartialArrayOop)) {
        main_data.fromObj := main_data.task
        goto(READ_MW_REQ)
      }.otherwise {
        driveReadReq(io.MainMreq, main_data.task, oopReadSize)

        when(io.MainMreq.Request.fire) {
          goto(READ_OOP_RESP)
        }
      }
    }

    READ_OOP_RESP.whenIsActive {
      io.MainMreq.Response.ready := True

      when(io.MainMreq.Response.fire) {
        val rd = io.MainMreq.Response.payload.ResponseData
        main_data.fromObj := decodeReadOopResp(rd)
        goto(READ_MW_REQ)
      }
    }

    READ_MW_REQ.whenIsActive {
      driveReadReq(io.MainMreq, main_data.fromObj, mwReadSize)

      when(io.MainMreq.Request.fire) {
        goto(READ_MW_RESP)
      }
    }

    READ_MW_RESP.whenIsActive {
      io.MainMreq.Response.ready := True

      when(io.MainMreq.Response.fire) {
        val rd = io.MainMreq.Response.payload.ResponseData
        fillMwKlassLen(rd, main_data)
        goto(SEND)
      }
    }

    SEND.whenIsActive {
      val isOop = main_data.oopType === U(NotArrayOop)

      when(isOop) {
        driveProcessUnit(io.Fetch2OopProcess, main_data)
      }.otherwise {
        driveProcessUnit(io.Fetch2ArrayProcess, main_data)
      }

      val unitFire = Mux(isOop, io.Fetch2OopProcess.cmd.fire, io.Fetch2ArrayProcess.cmd.fire)
      when(unitFire) {
        goto(WAIT_DONE)

        dbg(Seq(
          "Dispatch Task=", main_data.task,
          " OopType=", main_data.oopType,
          " SrcOopPtr=", main_data.fromObj,
          " MarkWord=", main_data.markWord,
          " KlassPtr=", main_data.klassPtr,
          " success!"
        ))
      }
    }

    WAIT_DONE.whenIsActive {
      when(targetDone || targetDoneSeen) {
        targetDoneSeen := False
        goto(IDLE)
        dbg(Seq("Task=", main_data.task, " done"))
      }
    }

    always {
      when(mainGotoSend) {
        goto(SEND)
      }.elsewhen(mainGotoReadOop) {
        goto(READ_OOP_REQ)
      }
    }
  }

  // Push StateMachine
  // If mainFsm is idle, Trace2Fetch task can directly enter main_data.
  // Otherwise it is processed in push_data and later handed to mainFsm.
  val pushFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State
    val READ_OOP_RESP = new State
    val READ_MW_REQ   = new State
    val READ_MW_RESP  = new State
    val SEND          = new State

    def pushYieldOk: Bool = main_data.oopType === U(PartialArrayOop) || io.CopyDone || copyDoneSeen

    IDLE.whenIsActive {
      io.Trace2Fetch.ready := True

      when(io.Trace2Fetch.fire) {
        val payload = io.Trace2Fetch.payload

        when(mainIsIdle) {
          receiveTask(payload, main_data)
          main_data.fromObj := U(0)
          mainGotoReadOop := True

        }.otherwise {
          receiveTask(payload, push_data)
          push_data.fromObj := U(0)
          goto(READ_OOP_REQ)
        }
      }
    }

    READ_OOP_REQ.whenIsActive {
      when(push_data.oopType === U(PartialArrayOop)) {
        push_data.fromObj := push_data.task
        goto(READ_MW_REQ)

      }.otherwise {
        when(pushYieldOk) { // Push状态机在等待CopyDone时不发起OOP读取请求 and main_data is PartialArrayOop 不需要复制
          driveReadReq(io.PushMreq, push_data.task, oopReadSize)

          when(io.PushMreq.Request.fire) {
            goto(READ_OOP_RESP)
          }
        }
      }
    }

    READ_OOP_RESP.whenIsActive {
      io.PushMreq.Response.ready := True

      when(io.PushMreq.Response.fire) {
        val rd = io.PushMreq.Response.payload.ResponseData
        push_data.fromObj := decodeReadOopResp(rd)
        goto(READ_MW_REQ)
      }
    }

    READ_MW_REQ.whenIsActive {
      when(pushYieldOk) {
        driveReadReq(io.PushMreq, push_data.fromObj, mwReadSize)

        when(io.PushMreq.Request.fire) {
          goto(READ_MW_RESP)
        }
      }
    }

    READ_MW_RESP.whenIsActive {
      io.PushMreq.Response.ready := True

      when(io.PushMreq.Response.fire) {
        val rd = io.PushMreq.Response.payload.ResponseData

        copyDoneSeen := False

        when(mainIsIdle) {
          main_data.task    := push_data.task
          main_data.oopType := push_data.oopType
          main_data.fromObj := push_data.fromObj

          fillMwKlassLen(rd, main_data)

          mainGotoSend := True
          goto(IDLE)

        }.otherwise {
          fillMwKlassLen(rd, push_data)
          goto(SEND)
        }
      }
    }

    SEND.whenIsActive {
      when(mainIsIdle) {
        main_data    := push_data
        mainGotoSend := True
        goto(IDLE)
      }
    }
  }

  // ============================================================================
  // PreFetch StateMachine
  // Normal mode: PushCount == 0 且 pushFollowRem == 0 新的 PrePop 表项追加到 buf_top 位置。
  // Push-follow mode: PushCount != 0 或 pushFollowRem != 0 新压入的任务应当优先于旧的预取任务被处理。
  //                   因此，一个 push-follow 组中的第一个表项会插入到 当前 buf_bottom 之前，同时 buf_bottom 向后移动。
  // 如果剩余空间不足, 通过将 buf_top 向后移动，丢弃较旧的尾部表项
  // ============================================================================
  val preFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State
    val READ_OOP_RESP = new State
    val READ_MW_REQ   = new State
    val READ_MW_RESP  = new State

    IDLE.whenIsActive {
      val stackPushFollowActive = io.toFetch.PushCount =/= U(0, 32 bits)
      val fetchPushFollowActive = pushFollowRem =/= U(0, 32 bits)

      when(!stackPushFollowActive && !fetchPushFollowActive) {
        // Normal PrePop mode: append at buf_top.
        io.toFetch.PrePop.ready := !waitForPrefetch && buf_free =/= U(0, buf_free.getWidth bits)

        when(io.toFetch.PrePop.fire) {
          buf_work := buf_top
          resetSlot(buf_top)

          buf_top   := bufInc(buf_top, U(1, PreFetchBufferWidth bits))
          buf_count := buf_count + U(1, buf_count.getWidth bits)

          receiveTask(io.toFetch.PrePop.payload, preBuf(buf_top))
          goto(READ_OOP_REQ)
        }

      }.otherwise {
        // Push-follow mode:
        io.toFetch.PrePop.ready := !waitForPrefetch

        when(io.toFetch.PrePop.fire) {
          when(!stackPushFollowActive) {
            // Continue the current push-follow group.
            pushFollowRem := pushFollowRem - U(1, 32 bits)

            val idx = bufInc(buf_work, U(1, PreFetchBufferWidth bits))
            buf_work := idx

            resetSlot(idx)
            buf_count := buf_count + U(1, buf_count.getWidth bits)

            receiveTask(io.toFetch.PrePop.payload, preBuf(idx))
            goto(READ_OOP_REQ)

          }.otherwise {
            // Start a new push-follow group.
            val pushCount = Mux(
              io.toFetch.PushCount > U(PreFetchBufferNum, 32 bits),
              U(PreFetchBufferNum, 32 bits),
              io.toFetch.PushCount
            )

            val pushCountSmall = pushCount.resize(buf_count.getWidth)
            val idx = bufDec(buf_bottom, pushCountSmall)

            when(buf_free >= pushCountSmall) {
              buf_count := buf_count + U(1, buf_count.getWidth bits)

            }.otherwise {
              // Not enough room:
              val dropNum = pushCountSmall - buf_free
              buf_top := bufDec(buf_top, dropNum)

              // After dropping enough entries, insert the first new entry.
              buf_count := (buf_count + buf_free - pushCountSmall + U(1, buf_count.getWidth bits)).resized
            }

            buf_work      := idx
            buf_bottom    := idx
            pushFollowRem := pushCount - U(1, 32 bits)

            resetSlot(idx)
            receiveTask(io.toFetch.PrePop.payload, preBuf(idx))
            goto(READ_OOP_REQ)
          }
        }
      }
    }

    READ_OOP_REQ.whenIsActive {
      when(preBuf(buf_work).oopType === U(PartialArrayOop)) {
        preBuf(buf_work).fromObj := preBuf(buf_work).task
        goto(READ_MW_REQ)

      }.otherwise {
        driveReadReq(io.PreMreq, preBuf(buf_work).task, oopReadSize)

        when(io.PreMreq.Request.fire) {
          goto(READ_OOP_RESP)
        }
      }
    }

    READ_OOP_RESP.whenIsActive {
      io.PreMreq.Response.ready := True

      when(io.PreMreq.Response.fire) {
        val rd = io.PreMreq.Response.payload.ResponseData
        val newFromObj = decodeReadOopResp(rd)

        preBuf(buf_work).fromObj := newFromObj

        when(fwdValid && fwdObj === newFromObj) {
          preBuf(buf_work).markWord := fwdValue
          preBufMwHit(buf_work)     := True
          fwdValid := False
        }

        goto(READ_MW_REQ)
      }
    }

    READ_MW_REQ.whenIsActive {
      driveReadReq(io.PreMreq, preBuf(buf_work).fromObj, mwReadSize)

      when(io.PreMreq.Request.fire) {
        goto(READ_MW_RESP)
      }
    }

    READ_MW_RESP.whenIsActive {
      io.PreMreq.Response.ready := True

      when(io.PreMreq.Response.fire) {
        val rd = io.PreMreq.Response.payload.ResponseData
        val currentFromObj = preBuf(buf_work).fromObj

        val hitFwdNow = (fwdValid && currentFromObj === fwdObj) ||
          (io.gcWriteSrcOopPtr.writeForward.valid && currentFromObj === io.gcWriteSrcOopPtr.writeForward.payload.srcOopPtr)

        val finalMw = UInt(GCElementWidth bits)
        finalMw := rd(GCElementWidth - 1 downto 0)

        when(hitFwdNow) {
          when(io.gcWriteSrcOopPtr.writeForward.valid) {
            finalMw := io.gcWriteSrcOopPtr.writeForward.payload.writeValue
          }.otherwise {
            finalMw := fwdValue
            fwdValid := False
          }
        }.elsewhen(preBufMwHit(buf_work)) {
          finalMw := preBuf(buf_work).markWord
        }

        when(waitForPrefetch && mainIsIdle && buf_work === buf_bottom) {
          // The main pipeline is waiting for exactly this prefetch entry.
          // Directly forward the completed data to main_data and consume the slot.
          waitForPrefetch := False

          main_data.task     := preBuf(buf_bottom).task
          main_data.oopType  := preBuf(buf_bottom).oopType
          main_data.fromObj  := preBuf(buf_bottom).fromObj
          main_data.markWord := finalMw
          main_data.klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

          main_data.srcLength := Mux(
            io.ConfigIO.UseCompressedKlassPointers,
            rd(GCElementWidth * 2 - 1 downto GCElementWidth + 32),
            rd(GCElementWidth * 2 + 31 downto GCElementWidth * 2)
          )

          mainGotoSend := True

          resetSlot(buf_bottom)
          buf_bottom := bufInc(buf_bottom, U(1, PreFetchBufferWidth bits))
          buf_count  := buf_count - U(1, buf_count.getWidth bits)

        }.otherwise {
          preBuf(buf_work).markWord := finalMw
          preBuf(buf_work).klassPtr := rd(GCElementWidth * 2 - 1 downto GCElementWidth)

          preBuf(buf_work).srcLength := Mux(
            io.ConfigIO.UseCompressedKlassPointers,
            rd(GCElementWidth * 2 - 1 downto GCElementWidth + 32),
            rd(GCElementWidth * 2 + 31 downto GCElementWidth * 2)
          )

          preBufDone(buf_work) := True
        }

        goto(IDLE)
      }
    }
  }

  mainIsIdle     := mainFsm.isActive(mainFsm.IDLE)
  mainIsWaitDone := mainFsm.isActive(mainFsm.WAIT_DONE)

  pushIsIdle     := pushFsm.isActive(pushFsm.IDLE)
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(new GCFetch())
}