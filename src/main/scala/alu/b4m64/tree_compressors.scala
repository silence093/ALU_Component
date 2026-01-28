package alu.b4m64

import alu._
import chisel3._
import chisel3.util._

class tc_data extends Bundle {
  val final_i0 = UInt(128.W)
  val final_i1 = UInt(128.W)
  val final_sign = Vec(2, Bool())
  val hl = Bool() // 0 = low, 1 = high
}

class tree_compressors extends Module {
  val io = IO(new Bundle {
    val in = Input(new ed_data())
    val out = Output(new tc_data())
  })

  val data = RegInit(0.U.asTypeOf(new ed_data()))
  val processed = Wire(new tc_data())

  val label_1 = Seq.fill(8)(Module(new compressor42(128)))
  val label_2 = Seq.fill(4)(Module(new compressor42(128)))
  val label_3 = Seq.fill(2)(Module(new compressor42(128)))
  val label_4 = Module(new compressor42(128))
  val label_5 = Module(new CSA(128))
  //val label_MULW = Module(new CSA(64))

  processed.final_i0 := label_5.io.sum
  processed.final_i1 := label_5.io.carry
  processed.final_sign := data.final_sign
  processed.hl := data.hl

  for (i <- 0 until 8) {
    label_1(i).io.i0 := data.pp(i * 4 + 0)
    label_1(i).io.i1 := data.pp(i * 4 + 1)
    label_1(i).io.i2 := data.pp(i * 4 + 2)
    label_1(i).io.i3 := data.pp(i * 4 + 3)
  }

  for (i <- 0 until 4) {
    label_2(i).io.i0 := label_1(i * 2 + 0).io.o0
    label_2(i).io.i1 := label_1(i * 2 + 0).io.o1
    label_2(i).io.i2 := label_1(i * 2 + 1).io.o0
    label_2(i).io.i3 := label_1(i * 2 + 1).io.o1
  }

  for (i <- 0 until 2) {
    label_3(i).io.i0 := label_2(i * 2 + 0).io.o0
    label_3(i).io.i1 := label_2(i * 2 + 0).io.o1
    label_3(i).io.i2 := label_2(i * 2 + 1).io.o0
    label_3(i).io.i3 := label_2(i * 2 + 1).io.o1
  }

  label_4.io.i0 := label_3(0).io.o0
  label_4.io.i1 := label_3(0).io.o1
  label_4.io.i2 := label_3(1).io.o0
  label_4.io.i3 := label_3(1).io.o1

  label_5.io.a := label_4.io.o0
  label_5.io.b := label_4.io.o1
  label_5.io.c := data.pp(32)

  data := io.in
  io.out := processed
}
