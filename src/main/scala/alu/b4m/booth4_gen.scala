package alu.b4m

import alu._
import chisel3._

// stage 0
// generate Booth code
class booth4_data(width: Int = 32) extends Bundle {
  val a = UInt(width.W)
  val zero = Vec(width / 2, Bool())
  val times2 = Vec(width / 2, Bool())
  val neg = Vec(width / 2, Bool())
}

class booth4_gen(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val in = Input(new b4m_in(width))
    val out = Output(new booth4_data(width))
  })

  //val reg_data = RegInit(0.U.asTypeOf(new b4m_in(width)))
  val data = Wire(new b4m_in(width))
  val processed = Wire(new booth4_data(width))

  processed.a := data.a
  val b_ext = Wire(UInt((width + 1).W))
  val booth = Wire(Vec(width / 2, UInt(3.W)))
  b_ext := data.b ## 0.U(1.W)

  for (i <- 0 until width / 2) {
    booth(i) := b_ext(i * 2 + 2, i * 2)
    processed.zero(i) := booth(i) === "b000".U || booth(i) === "b111".U
    processed.times2(i) := booth(i) === "b011".U || booth(i) === "b100".U
    processed.neg(i) := booth(i)(2)
  }

  //reg_data := io.in
  data := io.in
  io.out := processed
}
