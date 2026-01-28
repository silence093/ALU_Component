// Based on CLA

package alu

import chisel3._

class Complement(width:Int = 32) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val a_comp = Output(UInt(width.W))
  })

  val g = Wire(Vec(width, Bool()))
  val p = Wire(Vec(width, Bool()))
  val c = Wire(Vec(width, Bool()))
  val sum = Wire(Vec(width, Bool()))

  io.a_comp := sum.asUInt
  c(0) := 1.U(1.W)

  for (i <- 0 until width) {
    val pg = Module(new gp_gen())
    pg.io.a := ~io.a(i)
    pg.io.b := 0.U
    g(i) := pg.io.g
    p(i) := pg.io.p
  }

  // generate carry
  for (i <- 1 until width) {
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
//  emitVerilog(new Complement(), Array("--target-dir", s"generated/$packageName"))
//
//  println("Done")
//}
