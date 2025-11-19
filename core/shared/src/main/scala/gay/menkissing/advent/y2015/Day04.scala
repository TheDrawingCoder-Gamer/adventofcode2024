package gay.menkissing.advent
package y2015

import gay.menkissing.common.*

// this should not have been this hard???
object Day04 extends Problem:
  type Input = String
  type Output = Int
  def parse(str: String): String = str.trim

  // ALWAYS roll your own crypto
  // NEVER have it safety checked
  object MD5:
    val s11 = 7
    val s12 = 12
    val s13 = 17
    val s14 = 22
    val s21 = 5
    val s22 = 9
    val s23 = 14
    val s24 = 20
    val s31 = 4
    val s32 = 11
    val s33 = 16
    val s34 = 23
    val s41 = 6
    val s42 = 10
    val s43 = 15
    val s44 = 21

    inline val longSize = java.lang.Long.BYTES

    def ff(a: Int, b: Int, c: Int, d: Int, x: Int, s: Int, ac: Int): Int =
      val n = a + ((b & c) | (~b & d)) + x + ac
      Integer.rotateLeft(n, s) + b
    def gg(a: Int, b: Int, c: Int, d: Int, x: Int, s: Int, ac: Int): Int =
      val n = a + ((b & d) | (c & ~d)) + x + ac
      Integer.rotateLeft(n, s) + b
    def hh(a: Int, b: Int, c: Int, d: Int, x: Int, s: Int, ac: Int): Int =
      val n = a + ((b ^ c) ^ d) + x + ac
      Integer.rotateLeft(n, s) + b
    def ii(a: Int, b: Int, c: Int, d: Int, x: Int, s: Int, ac: Int): Int =
      val n = a + (c ^ (b | ~d)) + x + ac
      Integer.rotateLeft(n, s) + b

    // this is the most unidiomatic scala i've ever written
    def calcBytes(in: Array[Byte]): Int =
      val len = in.length

      // apparently anything tangentially related to cryptography gives me a lobotomy
      // this part is correct
      var a0 = 0x67452301

      // should be fine
      // bytes.append(0x80.toByte)
      // apparently appending 1 by 1 is EVIL and should be avoided at all costs on arrays
      // oopsies!
      // while (bytes.length % 64) != 56 do bytes.append(0.toByte)
      val bytes = Array.ofDim[Byte](64)

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

      def getLittleInt(idx: Int): Int =
        bytes(idx * 4).toUInt + (bytes(idx * 4 + 1).toUInt << 8) +
          (bytes(idx * 4 + 2).toUInt << 16) + (bytes(idx * 4 + 3).toUInt << 24)

      val x0 = getLittleInt(0)
      val x1 = getLittleInt(1)
      val x2 = getLittleInt(2)
      val x3 = getLittleInt(3)
      val x4 = getLittleInt(4)
      val x5 = getLittleInt(5)
      val x6 = getLittleInt(6)
      val x7 = getLittleInt(7)
      val x8 = getLittleInt(8)
      val x9 = getLittleInt(9)
      val x10 = getLittleInt(10)
      val x11 = getLittleInt(11)
      val x12 = getLittleInt(12)
      val x13 = getLittleInt(13)
      val x14 = getLittleInt(14)
      val x15 = getLittleInt(15)

      // this segment copy pasted from java.base bc i aint writing all that :joy:
      /* Round 1 */
      a = ff(a, b, c, d, x0, s11, 0xd76aa478); /* 1 */
      d = ff(d, a, b, c, x1, s12, 0xe8c7b756); /* 2 */
      c = ff(c, d, a, b, x2, s13, 0x242070db); /* 3 */
      b = ff(b, c, d, a, x3, s14, 0xc1bdceee); /* 4 */
      a = ff(a, b, c, d, x4, s11, 0xf57c0faf); /* 5 */
      d = ff(d, a, b, c, x5, s12, 0x4787c62a); /* 6 */
      c = ff(c, d, a, b, x6, s13, 0xa8304613); /* 7 */
      b = ff(b, c, d, a, x7, s14, 0xfd469501); /* 8 */
      a = ff(a, b, c, d, x8, s11, 0x698098d8); /* 9 */
      d = ff(d, a, b, c, x9, s12, 0x8b44f7af); /* 10 */
      c = ff(c, d, a, b, x10, s13, 0xffff5bb1); /* 11 */
      b = ff(b, c, d, a, x11, s14, 0x895cd7be); /* 12 */
      a = ff(a, b, c, d, x12, s11, 0x6b901122); /* 13 */
      d = ff(d, a, b, c, x13, s12, 0xfd987193); /* 14 */
      c = ff(c, d, a, b, x14, s13, 0xa679438e); /* 15 */
      b = ff(b, c, d, a, x15, s14, 0x49b40821); /* 16 */

      /* Round 2 */
      a = gg(a, b, c, d, x1, s21, 0xf61e2562); /* 17 */
      d = gg(d, a, b, c, x6, s22, 0xc040b340); /* 18 */
      c = gg(c, d, a, b, x11, s23, 0x265e5a51); /* 19 */
      b = gg(b, c, d, a, x0, s24, 0xe9b6c7aa); /* 20 */
      a = gg(a, b, c, d, x5, s21, 0xd62f105d); /* 21 */
      d = gg(d, a, b, c, x10, s22, 0x2441453); /* 22 */
      c = gg(c, d, a, b, x15, s23, 0xd8a1e681); /* 23 */
      b = gg(b, c, d, a, x4, s24, 0xe7d3fbc8); /* 24 */
      a = gg(a, b, c, d, x9, s21, 0x21e1cde6); /* 25 */
      d = gg(d, a, b, c, x14, s22, 0xc33707d6); /* 26 */
      c = gg(c, d, a, b, x3, s23, 0xf4d50d87); /* 27 */
      b = gg(b, c, d, a, x8, s24, 0x455a14ed); /* 28 */
      a = gg(a, b, c, d, x13, s21, 0xa9e3e905); /* 29 */
      d = gg(d, a, b, c, x2, s22, 0xfcefa3f8); /* 30 */
      c = gg(c, d, a, b, x7, s23, 0x676f02d9); /* 31 */
      b = gg(b, c, d, a, x12, s24, 0x8d2a4c8a); /* 32 */

      /* Round 3 */
      a = hh(a, b, c, d, x5, s31, 0xfffa3942); /* 33 */
      d = hh(d, a, b, c, x8, s32, 0x8771f681); /* 34 */
      c = hh(c, d, a, b, x11, s33, 0x6d9d6122); /* 35 */
      b = hh(b, c, d, a, x14, s34, 0xfde5380c); /* 36 */
      a = hh(a, b, c, d, x1, s31, 0xa4beea44); /* 37 */
      d = hh(d, a, b, c, x4, s32, 0x4bdecfa9); /* 38 */
      c = hh(c, d, a, b, x7, s33, 0xf6bb4b60); /* 39 */
      b = hh(b, c, d, a, x10, s34, 0xbebfbc70); /* 40 */
      a = hh(a, b, c, d, x13, s31, 0x289b7ec6); /* 41 */
      d = hh(d, a, b, c, x0, s32, 0xeaa127fa); /* 42 */
      c = hh(c, d, a, b, x3, s33, 0xd4ef3085); /* 43 */
      b = hh(b, c, d, a, x6, s34, 0x4881d05); /* 44 */
      a = hh(a, b, c, d, x9, s31, 0xd9d4d039); /* 45 */
      d = hh(d, a, b, c, x12, s32, 0xe6db99e5); /* 46 */
      c = hh(c, d, a, b, x15, s33, 0x1fa27cf8); /* 47 */
      b = hh(b, c, d, a, x2, s34, 0xc4ac5665); /* 48 */

      /* Round 4 */
      a = ii(a, b, c, d, x0, s41, 0xf4292244); /* 49 */
      d = ii(d, a, b, c, x7, s42, 0x432aff97); /* 50 */
      c = ii(c, d, a, b, x14, s43, 0xab9423a7); /* 51 */
      b = ii(b, c, d, a, x5, s44, 0xfc93a039); /* 52 */
      a = ii(a, b, c, d, x12, s41, 0x655b59c3); /* 53 */
      d = ii(d, a, b, c, x3, s42, 0x8f0ccc92); /* 54 */
      c = ii(c, d, a, b, x10, s43, 0xffeff47d); /* 55 */
      b = ii(b, c, d, a, x1, s44, 0x85845dd1); /* 56 */
      a = ii(a, b, c, d, x8, s41, 0x6fa87e4f); /* 57 */
      d = ii(d, a, b, c, x15, s42, 0xfe2ce6e0); /* 58 */
      c = ii(c, d, a, b, x6, s43, 0xa3014314); /* 59 */
      b = ii(b, c, d, a, x13, s44, 0x4e0811a1); /* 60 */
      a = ii(a, b, c, d, x4, s41, 0xf7537e82); /* 61 */
      d = ii(d, a, b, c, x11, s42, 0xbd3af235); /* 62 */
      c = ii(c, d, a, b, x2, s43, 0x2ad7d2bb); /* 63 */
      b = ii(b, c, d, a, x9, s44, 0xeb86d391); /* 64 */

      a0 += a

      a0
    def calc(in: String, p2: Boolean): Boolean =
      val bites = calcBytes(in.getBytes("US-ASCII"))
      if p2 then (bites & 0x00ffffff) == 0
      else (bites & 0x00f0ffff) == 0

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

  def input: String = FileIO.getInput(2015, 4)
