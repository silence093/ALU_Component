package alu.b4m64

import alu._
import chisel3._
import chisel3.util._

class fs_data extends Bundle {
  val product = UInt(128.W)
  val hl = Bool() // 0 = low, 1 = high
}

class final_sum extends Module {
  val io = IO(new Bundle {
    val in = Input(new tc_data())
    val out = Output(new fs_data())
  })

  val data = RegInit(0.U.asTypeOf(new tc_data()))
  val processed = Wire(new fs_data())

  val adder = Module(new BKA(128))
  adder.io.a := data.final_i0
  adder.io.b := data.final_i1
  processed.product := Mux(data.final_sign(0), data.final_sign(1) ## adder.io.sum(126,0), adder.io.sum)
  processed.hl := data.hl

  data := io.in
  io.out := processed
}
