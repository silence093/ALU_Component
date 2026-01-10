// Brent-Kung Adder

package alu

import chisel3._
import chisel3.util._
import scala.math._

class BKA(width:Int = 32, needCarry:Boolean = false) extends Module {
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
  if (needCarry) io.cout.foreach(cout => cout := c(width)) else None

  val gp_gens = Seq.fill(width)(Module(new gp_gen()))
  for (i <- 0 until width) {
    gp_gens(i).io.a := io.a(i)
    gp_gens(i).io.b := io.b(i)
    g(i) := gp_gens(i).io.g
    p(i) := gp_gens(i).io.p
  }

  val G = Wire(Vec(log2Ceil(width) * 2, Vec(width, Bool())))
  val P = Wire(Vec(log2Ceil(width) * 2, Vec(width, Bool())))

  println(s"level 0: ")
  for (i <- 0 until width) {
    G(0)(i) := g(i)
    P(0)(i) := p(i)
    print(s"copy gp($i) to GP($i), ")
  }
  println("")

  println(s"up-sweep, ${log2Ceil(width)} labels")
  for (i <- 1 to log2Ceil(width)) {
    val level = i
    val step = pow(2, i - 1).toInt
    println(s"level $level(step: $step): ")
    for (j <- 0 until width) {
      if (j >= step && ((j + 1) % pow(2, step).toInt == 0)) {
        G(level)(j) := G(level - 1)(j) | P(level - 1)(j) & G(level - 1)(j - step)
        P(level)(j) := P(level - 1)(j) & P(level - 1)(j - step)
        print(s"gp(${j - step}) o gp($j)->GP($j), ")
      }
      else {
        G(level)(j) := G(level - 1)(j)
        P(level)(j) := P(level - 1)(j)
        print(s"copy gp($j) to GP($j), ")
      }
    }
    println("")
  }

  println(s"down-sweep, ${log2Ceil(width) - 1} labels")
  for (i <- 1 until log2Ceil(width)) {
    val level = i + log2Ceil(width)
    println(s"level $level: ")
    val step = width / 2 /pow(2, i - 1).toInt
    for (j <- 0 until width) {
      if (j >= step && ((j + 1 - step / 2) % step == 0)) {
        G(level)(j) := G(level - 1)(j) | P(level - 1)(j) & G(level - 1)(j - step / 2)
        P(level)(j) := P(level - 1)(j) & P(level - 1)(j - step / 2)
        print(s"gp(${level - 1})(${j - step / 2}) o gp(${level - 1})($j)->GP($j), ")
      }
      else {
        G(level)(j) := G(level - 1)(j)
        P(level)(j) := P(level - 1)(j)
        print(s"copy gp(${level - 1})($j) to GP($level)($j), ")
      }
    }
    println("")
  }

  // generate carry
  for (i <- 0 until width)
    c(i + 1) := G(log2Ceil(width) * 2 - 1)(i)

  // generate sum
  for (i <- 0 until width)
    sum(i) := p(i) ^ c(i)
}

//object top extends App {
//
//  val packageName = this.getClass.getPackage.getName
//  emitVerilog(new BKA(16), Array("--target-dir", s"generated/$packageName"))
//  println("Done")
//}
