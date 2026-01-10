package alu

import chisel3._

class common {

}

class gp_gen extends Module {
  val io = IO(new Bundle {
    val a = Input(Bool())
    val b = Input(Bool())
    val g = Output(Bool())
    val p = Output(Bool())
  })

  io.g := io.a & io.b
  io.p := io.a ^ io.b
}


class compressor42(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val i0 = Input(UInt(width.W))
    val i1 = Input(UInt(width.W))
    val i2 = Input(UInt(width.W))
    val i3 = Input(UInt(width.W))
    val o0 = Output(UInt((width + 1).W))
    val o1 = Output(UInt((width + 1).W))
  })

  // the 4-2 compressor is a combination of 2 CSAs
  val csa1 = Module(new CSA)
  val csa2 = Module(new CSA)
  csa1.io.a := io.i0
  csa1.io.b := io.i1
  csa1.io.c := io.i2
  csa2.io.a := io.i3
  csa2.io.b := csa1.io.sum
  csa2.io.c := csa1.io.carry
  io.o0 := csa2.io.sum
  io.o1 := csa2.io.carry
}