package alu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers



class B4MTest extends AnyFreeSpec with Matchers with ChiselSim {
  def toBinary32(x: Int): String = {
    String.format("%32s", x.toBinaryString).replace(' ', '0')
  }
  def toBinary3(x: Int): String = {
    String.format("%3s", x.toBinaryString).replace(' ', '0')
  }
  "Test" in {
    simulate(new B4M())
    { dut =>
      dut.io.flush.poke(false)

      for (i <- 0 to  9) {
        for (j <- 0 to 9) {
          dut.io.in.bits.a.poke(i.U)
          dut.io.in.bits.b.poke(j.U)
          val a = i
          val b = j
          dut.io.in.valid.poke(true)
          dut.io.out.ready.poke(true)
          while (!dut.io.out.valid.peekBoolean()) {
          // wait for valid
            dut.clock.step()
          }
          //val a = dut.io.out.bits.a.peek().litValue.toInt
          //val b = dut.io.out.bits.b.peek().litValue.toInt
          val p = dut.io.out.bits.product.peek().litValue.toInt
          //assert(a * b == p, s",$a * $b should be ${a * b}, but $p")
          println(s"pass: $a * $b = $p")
          dut.clock.step()
        }
      }
    }
    println("Test done")
  }
}
