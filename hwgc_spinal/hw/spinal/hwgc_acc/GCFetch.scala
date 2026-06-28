package hwgc_acc

import hwgc_top.{Config, GCTopParameters, HWParameters, LocalMMUIO, WrapDec, WrapInc}

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

// Data bundle for a single fetch task
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
//
// No MMU arbitration inside GCFetch.
//
// Notes for the new GCTaskStack:
//
//   1. TaskStack Pop / PrePop may come from recentPushBuf or sync RAM.
//      GCFetch only sees Stream semantics, so it does not need to care about
//      the internal source.
//
//   2. TaskStack exposes PushCount to request push-follow PrePop.
//      During push-follow, GCFetch should keep Pop.ready low and let preFsm
//      consume PrePop first.
//
//   3. Push-follow may request up to PreFetchBufferNum entries.
//      Therefore preBuf uses explicit buf_count and allows full capacity,
//      instead of sacrificing one entry.
// ============================================================================
class GCFetch extends Module with HWParameters with GCTopParameters with GCParameters {
  val io = new Bundle {
    val MainMreq           = master(new LocalMMUIO)
    val PushMreq           = master(new LocalMMUIO)
    val PreMreq            = master(new LocalMMUIO)

    val toFetch            = slave(new GCToFetch)
    val gcWriteSrcOopPtr   = slave(new GCWriteSrcOopPtr)
    val Trace2Fetch        = slave Stream UInt(GCElementWidth bits)
    val CopyDone           = in Bool()

    val Fetch2ArrayProcess = master(new GCToProcessUnit)
    val Fetch2OopProcess   = master(new GCToProcessUnit)
    val ConfigIO           = slave(new GCFetchConfigIO)
    val DebugTimeStamp     = in UInt(64 bits)
  }

  // ============================================================================
  // MMU helper functions
  // ============================================================================

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

  io.Fetch2ArrayProcess.clearIn()
  io.Fetch2OopProcess.clearIn()

  // ============================================================================
  // Decode / fill helpers
  // ============================================================================

  def receiveTask(payload: UInt, data: GcFetchData): Unit = {
    when(payload(GCOopTagWidth - 1 downto 0) === U(PartialArrayTag, GCOopTagWidth bits)) {
      data.oopType := U(PartialArrayOop)
      data.task    := payload - U(PartialArrayTag)
    }.otherwise {
      data.oopType := U(NotArrayOop, GCOopTypeWidth bits)
      data.task    := payload - payload(GCOopTagWidth - 1 downto 0)
    }
  }

  def decodeReadOopResp(rd: UInt): UInt =
    Mux(
      io.ConfigIO.UseCompressedOop,
      (io.ConfigIO.CompressedOopBase + (rd(31 downto 0) << io.ConfigIO.CompressedOopShift)).resize(GCElementWidth),
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
    target.Valid     := True
    target.Task      := payload.task
    target.OopType   := payload.oopType
    target.SrcOopPtr := payload.fromObj
    target.MarkWord  := payload.markWord
    target.KlassPtr  := payload.klassPtr
    target.SrcLength := payload.srcLength
  }

  val oopReadSize = U(8, LineBytesNumBitSize bits)
  val mwReadSize  = Mux(io.ConfigIO.UseCompressedKlassPointers, U(16), U(20)).resize(LineBytesNumBitSize)

  // ============================================================================
  // Data registers
  // ============================================================================

  val main_data = RegInit(GcFetchData().getZero)
  val push_data = RegInit(GcFetchData().getZero)

  // ============================================================================
  // PreFetch ring buffer
  //
  // Changed for the new TaskStack:
  //
  //   Old style:
  //     buf_free = PreFetchBufferNum - 1 - buf_count
  //
  //   New style:
  //     buf_free = PreFetchBufferNum - buf_count
  //
  // Reason:
  //   GCFetch already has explicit buf_count, so it can distinguish full/empty
  //   even when buf_top == buf_bottom.
  //
  //   New GCTaskStack may send up to PreFetchBufferNum push-follow PrePop
  //   entries, so Fetch should be able to store all PreFetchBufferNum entries.
  // ============================================================================

  val preBuf      = Vec.fill(PreFetchBufferNum)(RegInit(GcFetchData().getZero))
  val preBufDone  = Vec.fill(PreFetchBufferNum)(RegInit(False))
  val preBufMwHit = Vec.fill(PreFetchBufferNum)(RegInit(False))

  val buf_top    = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_bottom = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_count  = RegInit(U(0, PreFetchBufferWidth + 1 bits))

  val buf_capacity = U(PreFetchBufferNum, PreFetchBufferWidth + 1 bits)
  val buf_free     = buf_capacity - buf_count

  val buf_work = RegInit(U(0, PreFetchBufferWidth bits))

  // pushFollowRem mirrors the remaining push-follow PrePop count inside Fetch.
  // TaskStack owns the actual PrePop generation; Fetch uses this register to
  // place those PrePop results before normal prefetch entries in preBuf.
  val pushFollowRem = RegInit(U(0, 32 bits))

  def bufInc(ptr: UInt, step: UInt): UInt =
    WrapInc(ptr, PreFetchBufferNum, step).resize(PreFetchBufferWidth)

  def bufDec(ptr: UInt, step: UInt): UInt =
    WrapDec(ptr, PreFetchBufferNum, step).resize(PreFetchBufferWidth)

  def resetSlot(idx: UInt): Unit = {
    preBufDone(idx)  := False
    preBufMwHit(idx) := False
  }

  // ============================================================================
  // State visibility wires
  // ============================================================================

  val mainIsIdle     = Bool()
  val mainIsWaitDone = Bool()

  val pushIsIdle     = Bool()

  // Cross-FSM command pulses
  val mainGotoIdle    = Bool()
  val mainGotoReadOop = Bool()
  val mainGotoSend    = Bool()

  mainGotoIdle    := False
  mainGotoReadOop := False
  mainGotoSend    := False

  // ============================================================================
  // Cross-pipeline signals
  // ============================================================================

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

  // mainFsm has popped a task whose PreFetch entry exists but is not done yet.
  // In this case mainFsm waits for preFsm to finish that exact buf_bottom slot.
  val waitForPrefetch = RegInit(False)

  // ============================================================================
  // MarkWord forwarding
  //
  // If OopProcess writes back srcOopPtr while prefetch has already fetched or is
  // fetching the same object, forward the new MarkWord into preBuf.
  // ============================================================================

  val fwdValid = RegInit(False)
  val fwdObj   = RegInit(U(0, preBuf(0).fromObj.getWidth bits))
  val fwdValue = RegInit(U(0, GCElementWidth bits))

  when(io.gcWriteSrcOopPtr.valid) {
    fwdValid := True
    fwdObj   := io.gcWriteSrcOopPtr.srcOopPtr
    fwdValue := io.gcWriteSrcOopPtr.writeValue
  }

  for (i <- 0 until PreFetchBufferNum) {
    when(io.gcWriteSrcOopPtr.valid && preBufDone(i) && preBuf(i).fromObj === io.gcWriteSrcOopPtr.srcOopPtr) {
      preBuf(i).markWord := io.gcWriteSrcOopPtr.writeValue
      preBufMwHit(i)     := True
    }
  }

  // ============================================================================
  // Main StateMachine
  //
  // mainFsm handles normal Pop tasks from TaskStack.
  //
  // Important changes for the new TaskStack:
  //
  //   1. Pop.ready is disabled while TaskStack reports PushCount != 0.
  //      During push-follow, PrePop should run before normal Pop.
  //
  //   2. preBuf hit requires buf_count != 0.
  //      This avoids stale preBuf entries being matched after the buffer is empty.
  //
  //   3. waitForPrefetch completion now consumes the preBuf slot.
  //      The old code loaded main_data but forgot to update buf_bottom/buf_count.
  // ============================================================================

  val mainFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State
    val READ_OOP_RESP = new State
    val READ_MW_REQ   = new State
    val READ_MW_RESP  = new State
    val SEND          = new State
    val WAIT_DONE     = new State

    IDLE.whenIsActive {
      // 注意：
      // Pop.ready 不能组合依赖 io.toFetch.PushCount。
      //
      // 原因：
      //   io.toFetch.PushCount 在 TaskStack 中由 pushCountForFetch 产生，
      //   而 pushCountForFetch 组合依赖 io.toFetch.Pop.fire，
      //   Pop.fire 又依赖 Fetch 输出的 Pop.ready。
      //
      // 如果这里用 PushCount 控制 Pop.ready，会形成：
      //
      //   Pop.ready -> Pop.fire -> PushCount -> Pop.ready
      //
      // 的组合逻辑环。
      //
      // TaskStack 内部已经会用 push_count / pushPrePopRem 阻止
      // push-follow 阶段启动 Pop，所以 Fetch 侧不需要再用 PushCount 禁 Pop。
      val fetchPushFollowActive = pushFollowRem =/= U(0, 32 bits)

      io.toFetch.Pop.ready :=
        pushIsIdle &&
          !io.Trace2Fetch.valid &&
          !waitForPrefetch &&
          !fetchPushFollowActive

      when(io.toFetch.Pop.fire) {
        val popBase = io.toFetch.Pop.payload - io.toFetch.Pop.payload(GCOopTagWidth - 1 downto 0)
        val bottomValid = buf_count =/= U(0, buf_count.getWidth bits)
        val bottomHit   = bottomValid && preBuf(buf_bottom).task === popBase

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
        // Handles the case where Pop.fire saw the preBuf entry before preFsm's
        // completion was visible. Now the entry is done, so consume it.
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

      val unitValid = Mux(isOop, io.Fetch2OopProcess.Valid, io.Fetch2ArrayProcess.Valid)
      val unitReady = Mux(isOop, io.Fetch2OopProcess.Ready, io.Fetch2ArrayProcess.Ready)

      when(unitValid && unitReady) {
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
      when(mainGotoIdle) {
        goto(IDLE)
      }.elsewhen(mainGotoSend) {
        goto(SEND)
      }.elsewhen(mainGotoReadOop) {
        goto(READ_OOP_REQ)
      }
    }
  }

  // ============================================================================
  // Push StateMachine
  //
  // pushFsm handles Trace2Fetch tasks.
  //
  // If mainFsm is idle, Trace2Fetch task can directly enter main_data.
  // Otherwise it is processed in push_data and later handed to mainFsm.
  // ============================================================================

  val pushFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State
    val READ_OOP_RESP = new State
    val READ_MW_REQ   = new State
    val READ_MW_RESP  = new State
    val SEND          = new State

    def pushYieldOk: Bool =
      main_data.oopType === U(PartialArrayOop) || io.CopyDone || copyDoneSeen

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
        when(pushYieldOk) {
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
  //
  // preFsm consumes TaskStack.PrePop and fills preBuf.
  //
  // Normal mode:
  //   PushCount == 0 and pushFollowRem == 0
  //   New PrePop entries are appended at buf_top.
  //
  // Push-follow mode:
  //   PushCount != 0 or pushFollowRem != 0
  //   New pushed tasks should be consumed before old prefetched tasks.
  //   Therefore the first entry of a push-follow group is inserted before
  //   current buf_bottom, and buf_bottom is moved backward.
  //
  // If there is not enough free space:
  //   Drop older tail entries by moving buf_top backward.
  //   This is acceptable because they are only speculative prefetched entries.
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
        io.toFetch.PrePop.ready :=
          !waitForPrefetch &&
            buf_free =/= U(0, buf_free.getWidth bits)

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
        // Ready can stay high even when preBuf is full, because we can discard
        // older speculative entries to make room for newly pushed tasks.
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
            //
            // TaskStack may request up to PreFetchBufferNum entries.
            // Fetch mirrors that limit.
            val pushCount = Mux(
              io.toFetch.PushCount > U(PreFetchBufferNum, 32 bits),
              U(PreFetchBufferNum, 32 bits),
              io.toFetch.PushCount
            )

            val pushCountSmall = pushCount.resize(buf_count.getWidth)
            val idx = bufDec(buf_bottom, pushCountSmall)

            when(buf_free >= pushCountSmall) {
              // Enough free slots for the whole push-follow group.
              // This cycle inserts the first entry.
              buf_count := buf_count + U(1, buf_count.getWidth bits)

            }.otherwise {
              // Not enough room:
              // Drop old speculative tail entries by moving buf_top backward.
              //
              // dropNum = pushCount - buf_free
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

        val hitFwdNow =
          (io.gcWriteSrcOopPtr.valid && currentFromObj === io.gcWriteSrcOopPtr.srcOopPtr) ||
            (fwdValid && currentFromObj === fwdObj)

        val finalMw = UInt(GCElementWidth bits)
        finalMw := rd(GCElementWidth - 1 downto 0)

        when(hitFwdNow) {
          when(io.gcWriteSrcOopPtr.valid) {
            finalMw := io.gcWriteSrcOopPtr.writeValue
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

  // ============================================================================
  // State visibility assignments
  // ============================================================================

  mainIsIdle     := mainFsm.isActive(mainFsm.IDLE)
  mainIsWaitDone := mainFsm.isActive(mainFsm.WAIT_DONE)

  pushIsIdle     := pushFsm.isActive(pushFsm.IDLE)
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(new GCFetch())
}