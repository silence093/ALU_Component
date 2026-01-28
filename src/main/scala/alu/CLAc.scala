// Carry-Lookahead Adder with carry

package alu

import chisel3._

class CLAc(width:Int = 32) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val sum = Output(UInt(width.W))
    val cin = Input(UInt(width.W))
    val cout = Output(UInt(width.W))
  })

  val g = Wire(Vec(width, Bool()))
  val p = Wire(Vec(width, Bool()))
  val c = Wire(Vec(width + 1, Bool()))
  val sum = Wire(Vec(width, Bool()))

  io.sum := sum.asUInt
  c(0) := io.cin
  io.cout := c(width)

  for (i <- 0 until width) {
    val pg = Module(new gp_gen())
    pg.io.a := io.a(i)
    pg.io.b := io.b(i)
    g(i) := pg.io.g
    p(i) := pg.io.p
  }

  // generate carry
  for (i <- 1 until width+1) {
    c(i) := g(i-1) | (c(i-1) & p(i-1))
  }

  // generate sum
  for (i <- 0 until width) {
    sum(i) := p(i) ^ c(i)
  }
}

//object top extends App {
//
//  val packageName = this.getClass.getPackage.getName
//  emitVerilog(new CLA(), Array("--target-dir", s"generated/$packageName"))
//
//  println("Done")
//}
