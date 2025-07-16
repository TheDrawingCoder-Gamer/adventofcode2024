package gay.menkissing.hash

import gay.menkissing.common.toUInt
import scala.scalajs.js
import js.annotation.*
import js.typedarray.Uint8Array
import js.JSConverters.*


import java.nio.{ByteBuffer, ByteOrder}


object MD5:
  val s: IArray[Int] =
    IArray(
      7, 12, 17, 22,
      5, 9, 14, 20,
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
      (l >> (x * 8)).toByte
    .toList
  def calc(in: Array[Byte]): Array[Byte] =
    val bytes = in.toBuffer
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


    (0 until (bytes.length / 64)).foreach: j =>
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
        b = a + f + k(i)
          + bytes(j * 64 + g * 4).toUInt
          + (bytes(j * 64 + g * 4 + 1).toUInt << 8)
          + (bytes(j * 64 + g * 4 + 2).toUInt << 16)
          + (bytes(j * 64 + g * 4 + 3).toUInt << 24)
        b = Integer.rotateLeft(b, s(i & 3 | i >> 2 & ~3))
        b += c

        a = hold
      a0 += a
      b0 += b
      c0 += c
      d0 += d

    Array(
      a0.toByte, (a0 >> 8).toByte, (a0 >> 16).toByte, (a0 >> 24).toByte,
      b0.toByte, (b0 >>> 8).toByte, (b0 >>> 16).toByte, (b0 >>> 24).toByte,
      c0.toByte, (c0 >>> 8).toByte, (c0 >>> 16).toByte, (c0 >>> 24).toByte,
      d0.toByte, (d0 >>> 8).toByte, (d0 >>> 16).toByte, (d0 >>> 24).toByte,
    )