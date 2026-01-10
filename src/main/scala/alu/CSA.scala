// Carry Save Adder

package alu

import chisel3._

class CSA(width:Int = 32) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(width.W))
    val b = Input(UInt(width.W))
    val c = Input(UInt(width.W))
    val sum = Output(UInt(width.W))
    val carry = Output(UInt(width.W))
  })

  val sum = io.a ^ io.b ^ io.c
  val carry = Wire(Vec(width, Bool()))

  carry(0) := false.asBool
  for (i <- 1 until width) {
    carry(i) := ((io.a(i - 1) & io.b(i - 1)) |  (io.a(i - 1) & io.c(i - 1)) | (io.b(i - 1) & io.c(i - 1))).asBool
  }

  io.sum := sum
  io.carry := carry.asUInt
}

//object top extends App {
//
//  val packageName = this.getClass.getPackage.getName
//  emitVerilog(new CSA(8), Array("--target-dir", s"generated/$packageName"))
//  println("Done")
//}
