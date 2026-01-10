package alu.b4m

import chisel3._
import chisel3.util._

// stage 1
// generate partial product
class pp_data(width: Int = 32) extends Bundle {
  val pp = Vec(width / 2, UInt((width * 2 + 2).W))
}

class pp_gen(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val in = Input(new booth4_data(width))
    val out = Output(new pp_data(width))
  })

  val reg_data = RegInit(0.U.asTypeOf(new booth4_data(width)))
  val processed = Wire(new pp_data(width))

  val A = Wire(UInt((width * 2 + 2).W))
  val A2 = Wire(UInt((width * 2 + 2).W))
  val A2_neg = Wire(UInt((width * 2 + 2).W))
  val A_neg = Wire(UInt((width * 2 + 2).W))

  // 2 bit for protect sign
  A := Fill(width + 2, reg_data.a(width - 1)) ## reg_data.a
  A2 := Fill(width + 1, reg_data.a(width - 1)) ## reg_data.a ## 0.U(1.W)
  A_neg := -A
  A2_neg := -A2

  for (i <- 0 until width / 2) {
    processed.pp(i) := Mux(reg_data.zero(i), 0.U((width * 2).W), Mux(reg_data.neg(i),
      Mux(reg_data.times2(i), A2_neg, A_neg),
      Mux(reg_data.times2(i), A2, A))) << i * 2
  }

  reg_data  := io.in
  io.out := processed
}
