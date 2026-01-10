package alu.b4m

import alu._
import chisel3._

import scala.math.pow

// stage 2
// generate product by Wallace tree
class wallace_data(width: Int = 32) extends Bundle {
  val product = UInt((width * 2).W)
}

class wallace_gen(width: Int = 32) extends Module {
  val io = IO(new Bundle {
    val in = Input(new pp_data(width))
    val out = Output(new wallace_data(width))
  })

  val reg_data = RegInit(0.U.asTypeOf(new pp_data(width)))
  val processed = Wire(new wallace_data(width))

  val pp = reg_data.pp
  var compressors = Seq.fill(width / 2 / 4)(Module(new compressor42(width * 2 + 2)))

  for (i <- 0 until width / 2 / 4) {
    compressors(i).io.i0 := pp(i * 4 + 0)
    compressors(i).io.i1 := pp(i * 4 + 1)
    compressors(i).io.i2 := pp(i * 4 + 2)
    compressors(i).io.i3 := pp(i * 4 + 3)
  }

  for (i <- 1 until width / 2 / 4 - 1) {
    val previous = compressors
    compressors = Seq.fill(width / 2 / 4 / pow(2, i).toInt)(Module(new compressor42(width * 2 + 2)))
    for (j <- 0 until width / 2 / 4 / pow(2, i).toInt) {
      compressors(j).io.i0 := previous(j * 2).io.o0(width * 2 + 1, 0)
      compressors(j).io.i1 := previous(j * 2).io.o1(width * 2 + 1, 0)
      compressors(j).io.i2 := previous(j * 2 + 1).io.o0(width * 2 + 1, 0)
      compressors(j).io.i3 := previous(j * 2 + 1).io.o1(width * 2 + 1, 0)
    }
  }

  val cla = Module(new CLA(width * 2 + 2))
  cla.io.a := compressors(0).io.o0
  cla.io.b := compressors(0).io.o1
  processed.product := cla.io.sum(width * 2 + 1) ## cla.io.sum(width * 2 - 2, 0)

  reg_data  := io.in
  io.out := processed
}
