// Brent-Kung Adder with carry

package alu

import chisel3._
import chisel3.util._
import scala.math._

class BKAc(width:Int = 32) extends Module {
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
    println(s"copy gp($i) to GP($i),")
  }
  println("")

  println(s"up-sweep, ${log2Ceil(width)} labels")
  for (i <- 1 to log2Ceil(width)) {
    val level = i
    val step = pow(2, i).toInt
    println(s"level $level(step: $step): ")
    for (j <- 0 until width) {
      if ((j + 1) >= step && ((j + 1) % step == 0)) {
        G(level)(j) := G(level - 1)(j) | P(level - 1)(j) & G(level - 1)(j - step / 2)
        P(level)(j) := P(level - 1)(j) & P(level - 1)(j - step / 2)
        println(s"gp($j) o gp(${j - step / 2}) -> GP($j),")
      }
      else {
        G(level)(j) := G(level - 1)(j)
        P(level)(j) := P(level - 1)(j)
        println(s"copy gp($j) to GP($j),")
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
        println(s"gp(${level - 1}) o gp(${level - 1})(${j - step / 2})($j) -> GP($j),")
      }
      else {
        G(level)(j) := G(level - 1)(j)
        P(level)(j) := P(level - 1)(j)
        println(s"copy gp(${level - 1})($j) to GP($level)($j),")
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
//  emitVerilog(new BKAc(16), Array("--target-dir", s"generated/$packageName"))
//  println("Done")
//}
