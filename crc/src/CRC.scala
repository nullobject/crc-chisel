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
 * Copyright (c) 2021 Josh Bassett
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

import chisel3._

/**
 * Calculates the CRC for a stream of data.
 *
 * @param n The number of CRC bits to calculate.
 * @param g The generator polynomial.
 *
 * @see [[https://crccalc.com/]]
 */
class CRC(n: Int, g: Int) extends Module {
  val io = IO(new Bundle {
    /** Enable */
    val en = Input(Bool())
    /** Input */
    val in = Input(Bool())
    /** Output */
    val out = Output(Bool())
    /** Debug */
    val debug = Output(UInt(n.W))
  })

  // Returns true if the bit is set at the index for the given value
  def isBitSet(value: Int, i: Int): Boolean = (value & (1 << i)) != 0

  // Linear feedback shift register
  val lfsr = Reg(Vec(n, Bool()))

  // XOR the input bit with the last bit in the LFSR
  val bit = Mux(io.en, io.in ^ lfsr.last, false.B)

  // Load the first bit
  lfsr(0) := bit

  // Shift the LFSR bits
  for (i <- 0 until n - 1) {
    if (isBitSet(g, i + 1))
      lfsr(i + 1) := lfsr(i) ^ bit
    else
      lfsr(i + 1) := lfsr(i)
  }

  // Output
  io.out := Mux(io.en, io.in, lfsr.last)
  io.debug := lfsr.asUInt

  // Debug
  if (sys.env.get("DEBUG").contains("1")) {
    printf(p"CRC(data: ${lfsr} 0x${Hexadecimal(lfsr.asUInt)})\n")
  }
}
