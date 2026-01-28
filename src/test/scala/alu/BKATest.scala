package alu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.util.Random

class BKATest extends AnyFreeSpec with Matchers with ChiselSim {

  "Test" in {
    def toUInt(x: Int): Long = x & 0xFFFFFFFFL

    simulate(new BKA(32)) { dut =>
      for (i <- 1 to 100) {
        val a = Random.nextInt()
        val b = Random.nextInt()
        val expect = a + b
        dut.io.a.poke(toUInt(a).U(32.W))
        dut.io.b.poke(toUInt(b).U(32.W))
        val sum = dut.io.sum.peek().litValue
        println(s"Random test $i pass: $a + $b = $sum")
        dut.io.sum.expect(toUInt(expect).U(32.W))
        dut.clock.step()
      }
    }
    println("Test done")
  }
}
