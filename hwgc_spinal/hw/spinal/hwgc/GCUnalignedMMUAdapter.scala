package hwgc

import spinal.core._
import spinal.lib._

import scala.language.postfixOps

class GCUnalignedMMUAdapter extends Module with HWParameters with GCParameters {
  val io = new Bundle {
    val in = slave(new LocalMMUIO)
    val out = master(new LocalMMUIO)
  }
  io.in.Request.ready := False
  io.in.Response.valid := False
  io.in.Response.payload.clearAll()
  io.in.ConherentRequsetSourceID.valid := False
  io.in.ConherentRequsetSourceID.payload.clearAll()

  io.out.Request.valid := False
  io.out.Request.payload.assignAllByName(io.in.Request.payload)
  io.out.RequestSize.valid := False
  io.out.RequestSize.payload.clearAll()
  io.out.Response.ready := False

  val size = RegInit(U(0, 8 bits))
  val addr = RegInit(U(0, MMUAddrWidth bits))
  val isWrite = RegInit(False)
  val reqData = RegInit(U(0, MMUDataWidth bits))

  val offset = addr(log2Up(LineBytesNum) - 1 downto 0)
  val alignedAddr = addr & ~U(LineBytesNum - 1, addr.getWidth bits)
  val crossBeat = offset + size > LineBytesNum
  val firstBytes = Mux(crossBeat, LineBytesNum - offset, size)
  val secondBytes = size - firstBytes

  object overall_state extends SpinalEnum {
    val s_idle, s_send1, s_send2, s_wait1, s_wait2 = newElement()
  }
  val state = RegInit(overall_state.s_idle)

  val bufferData = RegInit(U(0, MMUDataWidth bits))
  val writeData0 = reqData |<< (offset << 3)
  val writeData1 = reqData |>> (firstBytes << 3)
  val mask0 = ((U(1, LineBytesNum bits) |<< firstBytes) - 1) |<< offset
  val mask1 = (U(1, LineBytesNum bits) |<< secondBytes) - 1
  val firstSourceID = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val secondSourceID = RegInit(U(0, LLCSourceMaxNumBitSize bits))
  val hasReceived = RegInit(False) // 0 表示 buffer 缓存的是 第一次的; 1 表示缓存的第二次

  switch(state){
    is(overall_state.s_idle){
      when(io.in.Request.valid){
        size := io.in.RequestSize.payload
        addr := io.in.Request.payload.RequestVirtualAddr
        reqData := io.in.Request.payload.RequestData
        isWrite := io.in.Request.payload.RequestType_isWrite

        state := overall_state.s_send1
      }
    }

    is(overall_state.s_send1){
      io.out.Request.valid := True
      io.out.Request.payload.RequestVirtualAddr := alignedAddr
      io.in.Request.ready := io.out.Request.ready

      when(isWrite){
        io.out.Request.payload.RequestData := writeData0
        io.out.Request.payload.RequestWStrb := mask0
      }

      when(io.out.Request.fire){
        when(crossBeat){
          state := overall_state.s_send2
          firstSourceID := io.out.ConherentRequsetSourceID.payload
        }.otherwise{
          state := overall_state.s_wait1
        }
      }
    }

    is(overall_state.s_send2){
      io.out.Request.valid := True
      io.out.Request.payload.RequestVirtualAddr := alignedAddr + LineBytesNum

      when(isWrite){
        io.out.Request.payload.RequestData := writeData1
        io.out.Request.payload.RequestWStrb := mask1
      }

      when(io.out.Request.fire){
        secondSourceID := io.out.ConherentRequsetSourceID.payload
        state := overall_state.s_wait1
      }
    }

    is(overall_state.s_wait1){
      io.out.Response.ready := True
      when(io.out.Response.fire){
        when(crossBeat){
          state := overall_state.s_wait2
          bufferData := io.out.Response.payload.ResponseData
          hasReceived := Mux(io.out.Response.payload.ResponseSourceID === firstSourceID, False, True)
        }.otherwise{
          val shifted = io.out.Response.payload.ResponseData |>> (offset << 3)

          io.in.Response.valid := True
          io.in.Response.payload.assignAllByName(io.out.Response.payload)

          when(!isWrite){
            io.in.Response.payload.ResponseData := shifted
          }

          when(io.in.Response.ready){
            state := overall_state.s_idle
          }
        }
      }
    }

    is(overall_state.s_wait2){
      io.out.Response.ready := True
      when(io.out.Response.fire){
        io.in.Response.valid := True
        io.in.Response.payload.assignAllByName(io.out.Response.payload)
        when(!isWrite){
          val first = Mux(hasReceived, io.out.Response.payload.ResponseData, bufferData) |>> (offset << 3)
          val second = Mux(hasReceived, bufferData, io.out.Response.payload.ResponseData) |<< (firstBytes << 3)
          val finalData = first | second

          io.in.Response.valid := True
          io.in.Response.payload.ResponseData := finalData
        }

        when(io.in.Response.ready){
          state := overall_state.s_idle
        }
      }
    }
  }
}

object GCUnalignedMMUAdapterVerilog extends App{
  Config.spinal.generateVerilog(new GCUnalignedMMUAdapter())
}