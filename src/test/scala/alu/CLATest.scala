package alu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class CLATest extends AnyFreeSpec with Matchers with ChiselSim {

  "Test" in {
    simulate(new CLA()) { dut =>
      for (i <- 0 to  1000 by 50) {
        for (j <- 0 to  1000 by 50) {
          dut.io.a.poke(i.U)
          dut.io.b.poke(j.U)
          dut.clock.step()
          dut.io.sum.expect((i+j).U(32.W))
          println("Test pass: " + i + "+" + j + "=" + (i + j))
        }
      }
    }
    println("Test done")
  }
}
