package alu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

class CSATest extends AnyFreeSpec with Matchers with ChiselSim {

  "Test" in {
    simulate(new CSA()) { dut =>
      for (i <- 0 to  1000 by 50) {
        for (j <- 0 to  1000 by 50) {
          for (k <- 0 to  1000 by 50) {
            dut.io.a.poke(i.U(32.W))
            dut.io.b.poke(j.U(32.W))
            dut.io.c.poke(k.U(32.W))
            dut.clock.step()
            val a = dut.io.a.peek().litValue
            val b = dut.io.b.peek().litValue
            val c = dut.io.c.peek().litValue
            val sum = dut.io.sum.peek().litValue
            val carry = dut.io.carry.peek().litValue
            assert(a + b + c == sum + carry, s",$a + $b + $c should be ${a + b + c}, but ${sum + carry}")
            println(s"pass: $a + $b + $c = ${sum + carry}")
          }
        }
      }
    }
    println("Test done")
  }
}
