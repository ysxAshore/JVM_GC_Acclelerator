package hwgc_acc

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

import scala.language.postfixOps

// Data bundle for a single fetch task
case class GcFetchData() extends Bundle with GCParameters {
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
// ============================================================================
class GCFetch extends Module with HWParameters with GCParameters {
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

  // ------------------------------------------------------------------------
  // Helpers for MMU ports
  // ------------------------------------------------------------------------
  def clearMreq(m: LocalMMUIO): Unit = {
    m.Request.valid     := False
    m.Request.payload.clearAll()
    m.RequestSize.valid := False
    m.RequestSize.payload.clearAll()
    m.Response.ready    := False
  }

  def driveReadReq(m: LocalMMUIO, addr: UInt, sizeBytes: UInt): Unit = {
    m.Request.valid     := True
    m.RequestSize.valid := True

    m.Request.payload.RequestWStrb         := U(0)
    m.Request.payload.RequestData          := U(0)
    m.Request.payload.RequestType_isWrite  := False
    m.Request.payload.RequestSourceID      := m.ConherentRequsetSourceID.payload
    m.Request.payload.RequestVirtualAddr   := addr

    m.RequestSize.payload := sizeBytes.resize(LineBytesNumBitSize)
  }

  clearMreq(io.MainMreq)
  clearMreq(io.PushMreq)
  clearMreq(io.PreMreq)

  io.toFetch.Pop.ready    := False
  io.toFetch.PrePop.ready := False
  io.Trace2Fetch.ready    := False

  io.Fetch2ArrayProcess.clearIn()
  io.Fetch2OopProcess.clearIn()

  // Helpers
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

  // Data registers
  val main_data = RegInit(GcFetchData().getZero)
  val push_data = RegInit(GcFetchData().getZero)

  // PreFetch ring buffer
  val preBuf      = Vec.fill(PreFetchBufferNum)(RegInit(GcFetchData().getZero))
  val preBufDone  = Vec.fill(PreFetchBufferNum)(RegInit(False))
  val preBufMwHit = Vec.fill(PreFetchBufferNum)(RegInit(False))

  val buf_top    = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_bottom = RegInit(U(0, PreFetchBufferWidth bits))
  val buf_count  = RegInit(U(0, PreFetchBufferWidth + 1 bits))
  val buf_free   = U(PreFetchBufferNum - 1, PreFetchBufferWidth + 1 bits) - buf_count

  val buf_work = RegInit(U(0, PreFetchBufferWidth bits))

  val pushFollowRem = RegInit(U(0, 32 bits))

  def resetSlot(idx: UInt): Unit = {
    preBufDone(idx)  := False
    preBufMwHit(idx) := False
  }

  // State visibility wires
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

  // Cross-pipeline signals
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

  val waitForPrefetch = RegInit(False)

  // MarkWord forwarding
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
      io.toFetch.Pop.ready := pushIsIdle &&
                              !io.Trace2Fetch.valid &&
                              !waitForPrefetch &&
                              pushFollowRem === U(0)

      when(io.toFetch.Pop.fire) {
        val popBase =
          io.toFetch.Pop.payload -
          io.toFetch.Pop.payload(GCOopTagWidth - 1 downto 0)

        when(preBuf(buf_bottom).task === popBase && preBufDone(buf_bottom)) {
          buf_count  := buf_count - 1
          buf_bottom := buf_bottom + 1
          main_data  := preBuf(buf_bottom)
          goto(SEND)

        }.elsewhen(preBuf(buf_bottom).task === popBase) {
          waitForPrefetch := True

        }.otherwise {
          receiveTask(io.toFetch.Pop.payload, main_data)
          main_data.fromObj := U(0)
          goto(READ_OOP_REQ)
        }
      }.elsewhen(waitForPrefetch && preBufDone(buf_bottom)) {
        // 处理 io.toFetch.fire和io.preMreq.Response.fire同时有效 此时会同时设置了waitForPrefetch preBufDone
        waitForPrefetch := False
        main_data       := preBuf(buf_bottom)
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

  // Push StateMachine
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

  // PreFetch StateMachine
  val preFsm = new StateMachine {
    val IDLE          = new State with EntryPoint
    val READ_OOP_REQ  = new State
    val READ_OOP_RESP = new State
    val READ_MW_REQ   = new State
    val READ_MW_RESP  = new State

    IDLE.whenIsActive {
      when(io.toFetch.PushCount === U(0) && pushFollowRem === U(0)) {
        io.toFetch.PrePop.ready := buf_free =/= U(0)

        when(io.toFetch.PrePop.fire) {
          buf_work := buf_top
          resetSlot(buf_top)

          buf_top   := buf_top + 1
          buf_count := buf_count + 1

          receiveTask(io.toFetch.PrePop.payload, preBuf(buf_top))
          goto(READ_OOP_REQ)
        }

      }.otherwise {
        io.toFetch.PrePop.ready := True

        when(io.toFetch.PrePop.fire) {
          when(io.toFetch.PushCount === U(0)) {
            pushFollowRem := pushFollowRem - 1

            val idx = buf_work + 1
            buf_work := idx

            resetSlot(idx)
            buf_count := buf_count + 1

            receiveTask(io.toFetch.PrePop.payload, preBuf(idx))
            goto(READ_OOP_REQ)

          }.otherwise {
            val pushCount = Mux(
              io.toFetch.PushCount > PreFetchBufferNum,
              U(PreFetchBufferNum),
              io.toFetch.PushCount
            )

            val idx = WrapDec(buf_bottom, PreFetchBufferNum, pushCount)

            when(buf_free >= pushCount) {
              buf_count := buf_count + 1
            }.otherwise {
              buf_top   := WrapDec(buf_top, PreFetchBufferNum, pushCount - buf_free)
              buf_count := (buf_count + buf_free - pushCount + 1).resized
            }

            buf_work      := idx
            buf_bottom    := idx
            pushFollowRem := pushCount - 1

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
          (io.gcWriteSrcOopPtr.valid &&
           currentFromObj === io.gcWriteSrcOopPtr.srcOopPtr) ||
          (fwdValid &&
           currentFromObj === fwdObj)

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

          buf_bottom := buf_bottom + 1
          buf_count  := buf_count - 1
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

  // State visibility assignments
  mainIsIdle     := mainFsm.isActive(mainFsm.IDLE)
  mainIsWaitDone := mainFsm.isActive(mainFsm.WAIT_DONE)

  pushIsIdle     := pushFsm.isActive(pushFsm.IDLE)
}

object GCFetchVerilog extends App {
  Config.spinal.generateVerilog(new GCFetch())
}