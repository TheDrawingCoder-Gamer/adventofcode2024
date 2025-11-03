package gay.menkissing.advent
package y2015

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable
import gay.menkissing.common.*

// this should not have been this hard???
object Day04 extends Problem:
  type Input = String
  type Output = Int
  def parse(str: String): String = str.trim

  // ALWAYS roll your own crypto
  // NEVER have it safety checked
  object MD5:
    val s: IArray[Int] =
      IArray(
        7, 12, 17, 22, 5, 9, 14, 20, 4, 11, 16, 23, 6, 10, 15, 21
      )

    inline val longSize = java.lang.Long.BYTES

    val k: IArray[Int] =
      val arr = Array.ofDim[Int](64)

      (0 until 64).foreach: i =>
        // to long so it doesn't turn numbers above Int.MaxValue into Int.MaxValue, then toInt to discard upper bits
        arr(i) =
          math.floor(math.pow(2, 32) * math.abs(math.sin(i + 1))).toLong.toInt

      arr.asInstanceOf[IArray[Int]]

    // this is the most unidiomatic scala i've ever written
    def calcBytes(in: Array[Byte]): Array[Byte] =
      val len = in.length

      // apparently anything tangentially related to cryptography gives me a lobotomy
      // this part is correct
      var a0 = 0x67452301

      // should be fine
      // bytes.append(0x80.toByte)
      // apparently appending 1 by 1 is EVIL and should be avoided at all costs on arrays
      // oopsies!
      // while (bytes.length % 64) != 56 do bytes.append(0.toByte)
      val bytesNeeded =
        val moduloLen = (len + 1) % 64
        val diff = 56 - moduloLen
        if diff < 0 then 64 + diff else diff
      val bytes = Array.ofDim[Byte](len + 1 + bytesNeeded + longSize)

      // we can safely assume that we only have 64 bytes because
      // our input is in the Int range, and that has a max of
      // 4294967296 which is only 10 digits
      // and our input is only 8 or so bytes
      // adding the 0x80 byte and the length bytes, thats 10 + 8 + 1 + 8, which is far less than 64
      // further, we can even safely assume we will be less than 32 bytes, meaning we can optimize
      // our length assignment to only one, as 32 * 8 = 256, and anything less will be in ubyte range
      in.copyToArray(bytes)
      bytes(len) = 0x80.toByte
      // the padding is already empty, no need to re empty it
      bytes(bytes.length - longSize) = (len * 8).toByte

      var a = a0
      var b = 0xefcdab89
      var c = 0x98badcfe
      var d = 0x10325476

      var i = 0
      while i < 64 do
        // expanding it out instead of destructuring halfed run time
        var f = 0
        var g = 0

        if i <= 15 then
          f = (b & c) | (~b & d)
          g = i
        else if i <= 31 then
          f = (d & b) | (~d & c)
          g = 5 * i + 1
        else if i <= 47 then
          f = b ^ c ^ d
          g = 3 * i + 5
        else
          f = c ^ (b | ~d)
          g = 7 * i

        g = g & 0x0f
        val hold = d
        d = c
        c = b
        // apparently i don't understand little endian. Forced to use byte buffers due to severe lobotomy
        b =
          a + f + k(i) + bytes(g * 4).toUInt + (bytes(g * 4 + 1).toUInt << 8) +
            (bytes(g * 4 + 2).toUInt << 16) + (bytes(g * 4 + 3).toUInt << 24)
        b = Integer.rotateLeft(b, s(i & 3 | i >> 2 & ~3))
        b += c

        a = hold
        i += 1
      a0 += a

      Array(
        a0.toByte,
        (a0 >>> 8).toByte,
        (a0 >>> 16).toByte
        /*
        (a0 >>> 24).toByte,
        b0.toByte,
        (b0 >>> 8).toByte,
        (b0 >>> 16).toByte,
        (b0 >>> 24).toByte,
        c0.toByte,
        (c0 >>> 8).toByte,
        (c0 >>> 16).toByte,
        (c0 >>> 24).toByte,
        d0.toByte,
        (d0 >>> 8).toByte,
        (d0 >>> 16).toByte,
        (d0 >>> 24).toByte
         */
      )
    def calc(in: String, p2: Boolean): Boolean =
      val bites = calcBytes(in.getBytes("UTF-8"))
      bites(0) == 0 && bites(1) == 0 &&
      (if p2 then bites(2) == 0 else (bites(2) & 0xf0) == 0)

  def part1(input: String): Int =
    var i = 1
    while true do
      if MD5.calc(input + i.toString, false) then return i
      i += 1
    ???
  def part2(input: String): Int =
    var i = 1
    while true do
      if MD5.calc(input + i.toString, true) then return i
      i += 1
    ???

  lazy val input: String = FileIO.getInput(2015, 4)
