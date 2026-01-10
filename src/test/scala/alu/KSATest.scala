package alu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class KSATest extends AnyFreeSpec with Matchers with ChiselSim {

  "Test" in {
    simulate(new KSA()) { dut =>
      for (i <- 0 to  1000 by 50) {
        dut.io.a.poke(i.U(32.W))
        for (j <- 0 to  1000 by 50) {
          dut.io.b.poke(j.U(32.W))
          dut.clock.step()
          dut.io.sum.expect((i + j).U(32.W))
          val a = dut.io.a.peek().litValue
          val b = dut.io.b.peek().litValue
          val sum = dut.io.sum.peek().litValue
          println(s"pass: $a + $b = $sum")
        }
      }
    }
    println("Test done")
  }
}
