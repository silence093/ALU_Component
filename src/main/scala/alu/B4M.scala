// Booth Radix-4 Multiplier

package alu

import alu.b4m._
import chisel3._
import chisel3.util._

import scala.math._

class b4m_in(width: Int = 32) extends Bundle {
  val a = UInt(width.W)
  val b = UInt(width.W)
  val sign = Vec(2, Bool())
}

class b4m_out(width: Int = 32) extends Bundle {
  val product = UInt(width.W)
}

class B4M(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new b4m_in(width)))
    val out = Decoupled(new b4m_out(width))
    val flush = Input(Bool())
  })


  val zero = Wire(Bool())
  val one_a = Wire(Bool())
  val one_b = Wire(Bool())
  val neg_a = Wire(Bool())
  val neg_b = Wire(Bool())
  val special = Wire(Bool())
  val special_product = Wire(UInt((width * 2).W))

  zero := io.in.bits.a === 0.U | io.in.bits.b === 0.U
  one_a := io.in.bits.a === 1.U
  one_b := io.in.bits.b === 1.U
  neg_a := io.in.bits.a === -1.S.asUInt
  neg_b := io.in.bits.b === -1.S.asUInt
  special := zero | one_a | one_b | neg_a | neg_b
  special_product := MuxCase(0.U(width.W), Seq(
    zero -> 0.U((width * 2).W),
    one_a -> io.in.bits.b,
    one_b -> io.in.bits.a,
    neg_a -> (-io.in.bits.b.asSInt).asUInt,
    neg_b -> (-io.in.bits.a.asSInt).asUInt
  ))

  // 创建 stage 链
  val booth4 = Module(new booth4_gen(width))
  val pp = Module(new pp_gen(width))
  val wallace = Module(new wallace_gen(width))

  // The three states
  object State extends ChiselEnum {
    val Booth4, PP, Wallace = Value
  }
  import State._
  // The state register
  val stateReg = RegInit(Booth4)
  // Next state logic
  switch (stateReg) {
    is (Booth4) {
      when(io.flush | special) {
        stateReg := Booth4
      }.elsewhen (io.in.valid) {
        stateReg := PP
      }
    }
    is (PP) {
      when(io.flush) {
        stateReg := Booth4
      }.otherwise {
        stateReg := Wallace
      }
    }
    is (Wallace) {
      when(io.flush | io.out.ready) {
        stateReg := Booth4
      }
    }
  }

  io.in.ready := stateReg === Booth4
  io.out.valid := stateReg === Wallace | special

  booth4.io.in <> io.in.bits
  pp.io.in <> booth4.io.out
  wallace.io.in <> pp.io.out
  //io.out.bits <> wallace.io.out
  io.out.bits.product := Mux(special, special_product, wallace.io.out.product)
}

object top extends App {

  val packageName = this.getClass.getPackage.getName
  emitVerilog(new B4M(), Array("--target-dir", s"generated/$packageName"))
  println("Done")
}
