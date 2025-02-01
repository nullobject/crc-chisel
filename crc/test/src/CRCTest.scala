/*
 *   __   __     __  __     __         __
 *  /\ "-.\ \   /\ \/\ \   /\ \       /\ \
 *  \ \ \-.  \  \ \ \_\ \  \ \ \____  \ \ \____
 *   \ \_\\"\_\  \ \_____\  \ \_____\  \ \_____\
 *    \/_/ \/_/   \/_____/   \/_____/   \/_____/
 *   ______     ______       __     ______     ______     ______
 *  /\  __ \   /\  == \     /\ \   /\  ___\   /\  ___\   /\__  _\
 *  \ \ \/\ \  \ \  __<    _\_\ \  \ \  __\   \ \ \____  \/_/\ \/
 *   \ \_____\  \ \_____\ /\_____\  \ \_____\  \ \_____\    \ \_\
 *    \/_____/   \/_____/ \/_____/   \/_____/   \/_____/     \/_/
 *
 * https://joshbassett.info
 * https://twitter.com/nullobject
 * https://github.com/nullobject
 *
 * Copyright (c) 2025 Joshua Bassett
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

package crc

import chisel3.simulator.EphemeralSimulator._
import org.scalatest._
import flatspec.AnyFlatSpec
import matchers.should.Matchers

class CRCTest extends AnyFlatSpec with Matchers {
  def readBits(dut: CRC, n: Int) = {
    var bits = 0
    for (i <- (n - 1) to 0 by -1) {
      val bit = dut.io.out.peek().litValue
      dut.clock.step()
      bits |= (bit.toInt << i)
    }
    bits
  }

  def writeBits(dut: CRC, d: Int, n: Int) = {
    for (i <- (n - 1) to 0 by -1) {
      val bit = (d & 1 << i) != 0
      dut.io.in.poke(bit)
      dut.clock.step()
    }
  }

  def writeString(dut: CRC, s: String) = {
    s.foreach { c => writeBits(dut, c, 8) }
  }

  it should "calculate the CRC" in {
    simulate(new CRC(16, 0x1021)) { dut =>
      dut.io.en.poke(true)
      writeString(dut, "foo")
      dut.io.debug.expect(0xaf96)
      dut.io.en.poke(false)
      readBits(dut, 16) shouldBe 0xaf96
    }
  }

  it should "check the CRC" in {
    simulate(new CRC(16, 0x1021)) { dut =>
      dut.io.en.poke(true)
      writeString(dut, "foo")
      writeBits(dut, 0xaf96, 16)
      dut.io.debug.expect(0)
      dut.io.en.poke(false)
      readBits(dut, 16) shouldBe 0
    }
  }
}
