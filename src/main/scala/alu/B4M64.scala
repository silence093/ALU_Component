// Booth Radix-4 Multiplier for 64-bit

package alu

import alu.b4m64._
import chisel3._
import chisel3.util._

class b4m64_in extends Bundle {
  val a = UInt(64.W)
  val b = UInt(64.W)
  val sign = Vec(2, Bool())
  val hl = Bool() // 0 = low, 1 = high
}

class b4m64_out extends Bundle {
  val product = UInt(64.W)
}

class B4M64 extends Module {
  val io = IO(new Bundle {
    val in  = Flipped(Decoupled(new b4m64_in()))
    val out = Decoupled(new b4m64_out())
    val flush = Input(Bool())
  })

  val previous = Wire(Bool())
  val zero = Wire(Bool())
  val one_a = Wire(Bool())
  val one_b = Wire(Bool())
  val neg_a = Wire(Bool())
  val neg_b = Wire(Bool())
  val special = Wire(Bool())
  val special_product = Wire(UInt(128.W))
  val previous_data = RegInit(0.U.asTypeOf(new b4m64_in()))
  val previous_product = RegInit(0.U(128.W))
  val one_product = Wire(UInt(64.W))
  val comp = Module(new Complement(64))

  // The three states
  object State extends ChiselEnum {
    val EncodeDecode, TreeCommpressors, FinalSum = Value
  }
  import State._
  // The state register
  val stateReg = RegInit(EncodeDecode)

  previous := stateReg === EncodeDecode &
    (io.in.bits.a === previous_data.a & io.in.bits.b === previous_data.b & (io.in.bits.sign === previous_data.sign | ~io.in.bits.hl))
  zero := io.in.bits.a === 0.U(64.W) | io.in.bits.b === 0.U(64.W)
  one_a := io.in.bits.a === 1.U(64.W)
  one_b := io.in.bits.b === 1.U(64.W)
  neg_a := io.in.bits.a === -1.S(64.W).asUInt
  neg_b := io.in.bits.b === -1.S(64.W).asUInt
  special := zero | one_a | one_b | neg_a | neg_b | previous
  special_product := MuxCase(0.U(64.W), Seq(
    previous -> previous_product,
    zero -> 0.U(64.W),
    (one_a | one_b) -> one_product,
    (neg_a | neg_b) -> comp.io.a_comp
  ))

  one_product := Mux(one_a, io.in.bits.b, Mux(one_b, io.in.bits.b, 0.U(64.W)))

  comp.io.a := MuxCase(0.U(64.W), Seq(
    neg_a -> io.in.bits.a,
    neg_b -> io.in.bits.b,
  ))

  // enc_dec(generate pp) -> tree_compressors -> final_sum

  val ed = Module(new enc_dec())
  val tc = Module(new tree_compressors())
  val fs = Module(new final_sum())

  // Next state logic
  switch (stateReg) {
    is (EncodeDecode) {
      when(io.flush | special) {
        stateReg := EncodeDecode
      }.elsewhen (io.in.valid) {
        stateReg := TreeCommpressors
        previous_data.a := io.in.bits.a
        previous_data.b := io.in.bits.b
        previous_data.sign := io.in.bits.sign
        previous_data.hl := io.in.bits.hl
      }
    }
    is (TreeCommpressors) {
      when(io.flush) {
        stateReg := EncodeDecode
      }.otherwise {
        stateReg := FinalSum
      }
    }
    is (FinalSum) {
      when(io.flush | io.out.ready) {
        stateReg := EncodeDecode
        previous_product := fs.io.out.product
      }
    }
  }

  io.in.ready := stateReg === EncodeDecode
  io.out.valid := stateReg === FinalSum | special

  ed.io.in <> io.in.bits
  tc.io.in <> ed.io.out
  fs.io.in <> tc.io.out
  io.out.bits.product := Mux(special, Mux(io.in.bits.hl, special_product(127, 64), special_product(63, 0)),
    Mux(fs.io.out.hl, fs.io.out.product(127, 64), fs.io.out.product(63, 0)))
}

//object top extends App {
//
//  val packageName = this.getClass.getPackage.getName
//  emitVerilog(new B4M(), Array("--target-dir", s"generated/$packageName"))
//  println("Done")
//}
