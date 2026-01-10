// Kogge-Stone Adder

package alu

import chisel3._
import chisel3.util._
import scala.math._

class KSA(width:Int = 32, needCarry:Boolean = false) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val sum = Output(UInt(width.W))
    val cin = if (needCarry) Some(Input(UInt(width.W))) else None
    val cout = if (needCarry) Some(Output(UInt(width.W))) else None
  })

  val g = Wire(Vec(width, Bool()))
  val p = Wire(Vec(width, Bool()))
  val c = Wire(Vec(width + 1, Bool()))
  val sum = Wire(Vec(width, Bool()))

  io.sum := sum.asUInt
  if (needCarry) c(0) := io.cin.get else c(0) := false.B
  if (needCarry) io.cout.foreach(c => c := c(width)) else None

  val gp_gens = Seq.fill(width)(Module(new gp_gen()))
  for (i <- 0 until width) {
    gp_gens(i).io.a := io.a(i)
    gp_gens(i).io.b := io.b(i)
    g(i) := gp_gens(i).io.g
    p(i) := gp_gens(i).io.p
  }

  val G = Wire(Vec(log2Ceil(width) + 1, Vec(width, Bool())))
  val P = Wire(Vec(log2Ceil(width) + 1, Vec(width, Bool())))

  println(s"level 0: ")
  for (i <- 0 until width) {
    G(0)(i) := g(i)
    P(0)(i) := p(i)
    print(s"copy gp($i) to GP(0)($i), ")
  }
  println("")

  for (i <- 0 until  log2Ceil(width)) {
    val level = i + 1
    val step = pow(2, i).toInt
    println(s"level $level(step: $step): ")
    for (j <- 0 until  step) {
      G(level)(j) := G(level - 1)(j)
      P(level)(j) := P(level - 1)(j)
      print(s"copy gp(${level - 1})($j) to GP($level)($j), ")
    }
    for (j <- step until  width) {
      G(level)(j) := G(level - 1)(j) | P(level - 1)(j) & G(level - 1)(j - step)
      P(level)(j) := P(level - 1)(j) & P(level - 1)(j - step)
      print(s"gp(${j - step}) o gp($j)->GP($j), ")
    }
    println("")
  }

  // generate carry
  for (i <- 1 to width)
    c(i) := G(log2Ceil(width) - 1)(i - 1)

  // generate sum
  for (i <- 0 until width)
    sum(i) := p(i) ^ c(i)
}

//object top extends App {
//
//  val packageName = this.getClass.getPackage.getName
//  emitVerilog(new KSA(8), Array("--target-dir", s"generated/$packageName"))
//  println("Done")
//}
