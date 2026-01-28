package alu

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers

import scala.util._
import scala.math._

class B4M64Test extends AnyFreeSpec with Matchers with ChiselSim {
  "Test" in {
    def toHexStr(x: Long) : String = "h" + x.toHexString
    def unsignedRepresentation128(x: BigInt) : BigInt = if (x < 0) (BigInt(1) << 128) + x else x
    def unsignedRepresentation64(x: BigInt) : BigInt = if (x < 0) (BigInt(1) << 64) + x else x
    def expectStr(x: BigInt) : String = f"${unsignedRepresentation128(x)}%032x"

    simulate(new B4M64())
    { dut =>
      dut.io.flush.poke(false)

      println(s"signed * signed tests:")
      for (i <- 1 to 100) {
        dut.io.in.bits.sign(0).poke(true)
        dut.io.in.bits.sign(1).poke(true)
        dut.io.in.valid.poke(true)
        dut.io.out.ready.poke(true)
        val a = Random.nextLong(1024)
        val b = Random.nextLong(1024)
        val expect = BigInt(a) * BigInt(b)
        val eStr = "0x" + expectStr(expect)
        val _a = toHexStr(a)
        val _b = toHexStr(b)
        dut.io.in.bits.a.poke(_a.U(64.W))
        dut.io.in.bits.b.poke(_b.U(64.W))
        dut.io.in.bits.hl.poke(1)
        while (!dut.io.out.valid.peekBoolean()) {
          // wait for valid
          dut.clock.step()
        }
        var p = "0x" + f"${dut.io.out.bits.product.peek().litValue.toLong}%016x"
        dut.clock.step()

        dut.io.in.valid.poke(true)
        dut.io.out.ready.poke(true)
        dut.io.in.bits.hl.poke(0)
        while (!dut.io.out.valid.peekBoolean()) {
          // wait for valid
          dut.clock.step()
        }
        p = p + f"${dut.io.out.bits.product.peek().litValue.toLong}%016x"
        dut.clock.step()

        println(s"Random test $i pass: $a(0x${a.toHexString}) * $b(0x${b.toHexString}) = $p")
        assert(p == eStr, "error")
      }

      println(s"unsigned * unsigned tests:")
      for (i <- 1 to 100) {
        dut.io.in.bits.sign(0).poke(false)
        dut.io.in.bits.sign(1).poke(false)
        dut.io.in.valid.poke(true)
        dut.io.out.ready.poke(true)
        val a = Random.nextLong()
        val b = Random.nextLong()
        val a1 : BigInt = if (a < 0) (BigInt(1) << 64) + a else a
        val b1 : BigInt = if (b < 0) (BigInt(1) << 64) + b else b
        val expect = a1 * b1
        val eStr = "0x" + f"$expect%032x"
        val _a = toHexStr(a)
        val _b = toHexStr(b)
        dut.io.in.bits.a.poke(_a.U(64.W))
        dut.io.in.bits.b.poke(_b.U(64.W))
        dut.io.in.bits.hl.poke(1)
        while (!dut.io.out.valid.peekBoolean()) {
          // wait for valid
          dut.clock.step()
        }
        var p = "0x" + f"${dut.io.out.bits.product.peek().litValue.toLong}%016x"
        dut.clock.step()

        dut.io.in.valid.poke(true)
        dut.io.out.ready.poke(true)
        dut.io.in.bits.hl.poke(0)
        while (!dut.io.out.valid.peekBoolean()) {
          // wait for valid
          dut.clock.step()
        }
        p = p + f"${dut.io.out.bits.product.peek().litValue.toLong}%016x"
        dut.clock.step()

        println(s"Random test $i pass: $a1(0x${a.toHexString}) * $b1(0x${b.toHexString}) = $p")
        assert(p == eStr, "error")
      }

      println(s"signed * unsigned tests:")
      dut.io.in.bits.sign(0).poke(true)
      dut.io.in.bits.sign(1).poke(false)
      for (i <- 1 to 100) {
        dut.io.in.valid.poke(true)
        dut.io.out.ready.poke(true)
        val a = Random.nextLong()
        val b = Random.nextLong()
        val b1 : BigInt = if (b < 0) (BigInt(1) << 64) + b else b
        val expect = BigInt(a) * b1
        val eStr = "0x" + expectStr(expect)
        val _a = toHexStr(a)
        val _b = toHexStr(b)
        dut.io.in.bits.a.poke(_a.U(64.W))
        dut.io.in.bits.b.poke(_b.U(64.W))
        dut.io.in.bits.hl.poke(1)
        while (!dut.io.out.valid.peekBoolean()) {
          // wait for valid
          dut.clock.step()
        }
        var p = "0x" + f"${dut.io.out.bits.product.peek().litValue.toLong}%016x"
        dut.clock.step()

        dut.io.in.valid.poke(true)
        dut.io.out.ready.poke(true)
        dut.io.in.bits.hl.poke(0)
        while (!dut.io.out.valid.peekBoolean()) {
          // wait for valid
          dut.clock.step()
        }
        p = p + f"${dut.io.out.bits.product.peek().litValue.toLong}%016x"
        dut.clock.step()

        println(s"Random test $i pass: $a(0x${a.toHexString}) * $b1(0x${b.toHexString}) = $p")
        assert(p == eStr, "error")
      }

    }
  }
}
