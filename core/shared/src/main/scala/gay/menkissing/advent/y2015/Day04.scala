package gay.menkissing.advent
package y2015

import gay.menkissing.common.{Direction2D, Vec2i}

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable

// this should not have been this hard???
object Day04 extends Problem[String, Int]:
  def parse(str: String): String = str.trim

  // this was hard, next time import an npm lib
  def padL(str: String, c: Char, n: Int): String =
    (c.toString * (n - str.length)) + str

  def padHex(hexPart: Int): String = padL(hexPart.toHexString, '0', 8).grouped(2).toList.reverse.mkString("")

  // NEVER roll your own crypto
  // after being done I looked up other solutions
  // and they all just used builtins
  object MD5:
    val s: IArray[Int] =
      IArray(
        7, 12, 17, 22,
        5,  9, 14, 20,
        4, 11, 16, 23,
        6, 10, 15, 21,
      )

    val k: IArray[Int] =
      val arr = Array.fill(64)(0)

      (0 until 64).foreach: i =>
        // to long so it doesn't turn numbers above Int.MaxValue into Int.MaxValue, then toInt to discard upper bits
        arr(i) = math.floor(math.pow(2, 32) * math.abs(math.sin(i + 1))).toLong.toInt

      IArray.unsafeFromArray(arr)

    def littleEndianLong(l: Long): List[Byte] =
      (0 until 8).map: x =>
        ((l >> (x * 8)) & 255).toByte
      .toList

    def calc(in: String): String =
      val bytes = in.getBytes("US-ASCII").toBuffer
      val len = bytes.length.toLong

      // apparently anything tangentially related to cryptography gives me a lobotomy
      // this part is correct
      var a0 = 0x67452301
      var b0 = 0xefcdab89
      var c0 = 0x98badcfe
      var d0 = 0x10325476

      // should be fine
      bytes.append(0x80.toByte)
      while (bytes.length % 64) != 56 do
        bytes.append(0.toByte)

      // yes the length is in bits because fuck u thats why
      bytes.appendAll(littleEndianLong(len * 8))


      assert((bytes.length % 64) == 0)

      bytes.grouped(64).foreach: chunk =>
        val buf = ByteBuffer.wrap(chunk.toArray)
        buf.order(ByteOrder.LITTLE_ENDIAN)
        val ibuf = buf.asIntBuffer()
        var a = a0
        var b = b0
        var c = c0
        var d = d0

        (0 until 64).foreach: i =>
          var (f, g) =
            if i <= 15 then
              ((b & c) | (~b & d), i)
            else if i <= 31 then
              ((d & b) | (~d & c), 5 * i + 1)
            else if i <= 47 then
              (b ^ c ^ d, 3 * i + 5)
            else
              (c ^ (b | ~d), 7 * i)

          g = g & 0x0f
          val hold = d
          d = c
          c = b
          // apparently i don't understand little endian. Forced to use byte buffers due to severe lobotomy
          b = a + f + k(i) + ibuf.get(g)
          b = Integer.rotateLeft(b, s(i & 3 | i >> 2 & ~3))
          b += c

          a = hold
        a0 += a
        b0 += b
        c0 += c
        d0 += d

      padHex(a0) + padHex(b0) + padHex(c0) + padHex(d0)

  def part1(input: String): Int =
    Iterator.iterate(1)(_ + 1).find(it => MD5.calc(input + it.toString).startsWith("00000")).get

  def part2(input: String): Int =
    Iterator.iterate(1)(_ + 1).find(it => MD5.calc(input + it.toString).startsWith("000000")).get


  lazy val input: String = FileIO.getInput(2015, 4)
