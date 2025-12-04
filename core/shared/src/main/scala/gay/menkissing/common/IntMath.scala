package gay.menkissing.common

import annotation.tailrec

extension (self: Byte)
  inline def toUInt: Int =
    // copied implementation of java, but this is inline so better???
    self & 255

extension [T](self: T)(using as: AsNumber[T])
  // eucledian remainder ?
  // infix def erem(that: T): T = integral.emod(self, that)

  infix def choose(that: T): BigInt = chooseL(self.toLong, that.toLong)

def chooseL(n: Long, k: Long): BigInt =
  require(n >= 0 && k >= 0)
  if k == 0L || k == n then BigInt(1)
  else if k > n then BigInt(0)
  else if n - k > k then chooseL(n, n - k)
  else
    @tailrec def loop(lo: Long, hi: Long, prod: BigInt): BigInt =
      if lo > hi then prod
      else loop(lo + 1L, hi - 1L, BigInt(lo) * BigInt(hi) * prod)
    if ((n - k) & 1) == 1 then loop(k + 1, n - 1L, BigInt(n)) / (n - k).!
    else loop(k + 1, n, BigInt(1)) / (n - k).!

extension (self: Int)
  def digits: Int = math.log10(self.toDouble).toInt + 1

  def binaryDigits: Int = logBaseN(self.toDouble, 2.0).toInt

  infix def rem(that: Int): Int =
    val mod = self % that
    if mod < 0 then mod + math.abs(that) else mod

  infix def ceilDiv(that: Int): Int =
    math.ceil(self.toDouble / that.toDouble).toInt
  infix def floorDiv(that: Int): Int = math.floorDiv(self, that)

extension (self: Long)
  def ! : BigInt =
    @tailrec def loop(lo: Long, hi: Long, prod: BigInt): BigInt =
      if lo > hi then prod
      else loop(lo + 1L, hi - 1L, BigInt(lo) * BigInt(hi) * prod)
    if self < 0 then throw new IllegalArgumentException(self.toString)
    else if self == 0 then BigInt(1)
    else if (self & 1) == 1 then loop(1L, self - 1L, BigInt(self))
    else loop(2L, self - 1L, BigInt(self))

  def digits: Int = math.log10(self.toDouble).toInt + 1
  def binaryDigits: Int = logBaseN(self.toDouble, 2.0).toInt
  infix def rem(that: Long): Long =
    self - math.abs(that) * math.floorDiv(self, math.abs(that))

object Digits:
  def unapply(s: Long): Option[Int] = Some(s.digits)
  def unapply(s: Int): Option[Int] = Some(s.digits)
