package alu.b4m64

import alu._
import chisel3._
import chisel3.util._

class ed_data extends Bundle {
  val pp = Vec(33, UInt(128.W))
  val final_sign = Vec(2, Bool())
  val hl = Bool() // 0 = low, 1 = high
}

class enc_dec extends Module {
  val io = IO(new Bundle {
    val in = Input(new b4m64_in())
    val out = Output(new ed_data())
  })

  val data = Wire(new b4m64_in())
  val processed = Wire(new ed_data())

  val a = Mux(io.in.sign(0), Fill(2, data.a(63)), Fill(2, 0.U(1.W))) ## data.a
  val a2 = Mux(io.in.sign(0), data.a(63), 0.U(1.W)) ## data.a ## 0.U(1.W)
  val a_neg = Mux(io.in.sign(0), Fill(2, ~data.a(63)), Fill(2, 1.U(1.W))) ## ~data.a
  val a2_neg = Mux(io.in.sign(0), ~data.a(63), 1.U(1.W)) ## ~data.a ## 1.U(1.W)
  val b_extend = data.b ## 0.U(1.W)
  val booth4 = Wire(Vec(32, UInt(3.W)))
  val zero = Wire(Vec(32, Bool()))
  val one = Wire(Vec(32, Bool()))
  val two = Wire(Vec(32, Bool()))
  val neg = Wire(Vec(32, Bool()))
  val pp = Wire(Vec(33, UInt(128.W)))
  val final_sign = Wire(Vec(2, Bool()))

  // signed * signed = signed,
  // unsigned * unsigned = unsigned,
  // signed * unsigned = signed,
  // sign = sign_a xnor sign_b
  final_sign(0) := data.sign(0)
  final_sign(1) := Mux(data.sign(0), Mux(data.sign(1), data.a(63) ^ data.b(63), data.a(63)), false.B)

  processed.pp := pp
  processed.final_sign := final_sign
  processed.a := data.a
  processed.b := data.b
  processed.hl := data.hl

  for (i <- 0 until 32) {
    booth4(i) := b_extend(i * 2 + 2, i * 2)
    zero(i) := booth4(i) === "b000".U | booth4(i) === "b111".U
    one(i) := booth4(i)(0) ^ booth4(i)(1)
    two(i) := booth4(i) === "b011".U | booth4(i) === "b100".U
    neg(i) := booth4(i)(2)
  }

  pp(0) := Mux(neg(0),
    Mux(one(0), Fill(62, a_neg(65)) ## a_neg, Mux(two(0),
      Fill(62, a2_neg(65)) ## a2_neg,
      Fill(128, 1.U(1.W)))),
    Mux(one(0), Fill(62, a(65)) ## a, Mux(two(0),
      Fill(62, a2(65)) ## a2,
      Fill(128, 0.U(1.W)))))

  pp(1) := Mux(neg(1),
    Mux(one(1), Fill(60, a_neg(65)) ## a_neg, Mux(two(1),
      Fill(60, a2_neg(65)) ## a2_neg,
      Fill(126, 1.U(1.W)))),
    Mux(one(1), Fill(60, a(65)) ## a, Mux(two(1),
      Fill(60, a2(65)) ## a2,
      Fill(126, 0.U(1.W))))) ## 0.U(1.W) ## neg(0)

  for (i <- 2 until 31) {
    pp(i) := Mux(neg(i),
      Mux(one(i), Fill(62 - i * 2, a_neg(65)) ## a_neg, Mux(two(i),
        Fill(62 - i * 2, a2_neg(65)) ## a2_neg,
        Fill(128 - i * 2, 1.U(1.W)))),
      Mux(one(i), Fill(62 - i * 2, a(65)) ## a, Mux(two(i),
        Fill(62 - i * 2, a2(65)) ## a2,
        Fill(128 - i * 2, 0.U(1.W))))) ## 0.U(1.W) ## neg(i - 1) ## 0.U(((i - 1) * 2).W)
  }

  pp(31) := Mux(neg(31),
    Mux(one(31),a_neg, Mux(two(31),
      a2_neg,
      Fill(66, 1.U(1.W)))),
    Mux(one(31), a, Mux(two(31),
      a2,
      Fill(66, 0.U(1.W))))) ## 0.U(1.W) ## neg(30) ## 0.U(60.W)

  // if unsigned * unsigned, need to + a << 64 to correct the unsign
  pp(32) := Mux(neg(31) & ~data.sign(1), data.a, 0.U(64.W)) ## 0.U(1.W) ## neg(31) ## 0.U(62.W)

  data := io.in
  io.out := processed
}
