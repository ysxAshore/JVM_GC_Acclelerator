package hwgc_top

import hwgc_acc.GCAccTop
import spinal.core._
import spinal.lib.StreamArbiterFactory

import scala.language.postfixOps

class GCTop extends Module with GCTopParameters{
  val io = new GCTopIO

  val accs = Seq.fill(1)(new GCAccTop)

  // StreamArbiterFactory 做 Stream 输出的 多选器
  // roundRobin 轮询, 使用on(Seq())
  val cacheUpdateArb = StreamArbiterFactory.roundRobin.on(
    accs.map(_.io.CacheUpdateOut)
  )

  // 把多选器输出广播到加速器输入
  for (acc <- accs) {
    acc.io.CacheUpdateIn.valid := cacheUpdateArb.valid
    acc.io.CacheUpdateIn.payload := cacheUpdateArb.payload
  }

  cacheUpdateArb.ready := True

  accs(0).io.config <> io.ctrl2top
  io.mmu2llc <> accs(0).io.mmu2llc
}

object GCTopVerilog extends App{
  Config.spinal.generateVerilog(new GCTop())
}