package gay.menkissing.common

// import spire.implicits.*
import spire.math.Integral
import spire.syntax.all.*
import algebra.ring.TruncatedDivision

extension (self: Byte)
  inline def toUInt: Int =
    // copied implementation of java, but this is inline so better???
    self & 255

extension [T](self: T)(using integral: Integral[T])
  // eucledian remainder ?
  // infix def erem(that: T): T = integral.emod(self, that)

  def fallingFactorial(n: T): T =
    if n == integral.zero then integral.one
    else
      var r = integral.one
      var i = integral.zero
      while i < n do
        r *= self - i
        i += integral.one
      r

def choose(n: Int, k: Int): Int = n.fallingFactorial(k) / k.!

extension (self: Int)
  def digits: Int = math.log10(self.toDouble).toInt + 1

  // do not trust the factorial. it will hurt you if u try to do any number that is like...
  // idk, double digits? LOL
  def ! : Int =
    if self <= 0 then 1
    else (1 to self).product

  def fallingFactorial(n: Int): Int =
    if n == 0 then 1
    else (0 until n).map(f => self - f).product

  def binaryDigits: Int = logBaseN(self.toDouble, 2.0).toInt

  infix def rem(that: Int): Int =
    val mod = self % that
    if mod < 0 then mod + that else mod

  infix def ceilDiv(that: Int): Int =
    math.ceil(self.toDouble / that.toDouble).toInt
  infix def floorDiv(that: Int): Int = math.floorDiv(self, that)

extension (self: Long)
  def digits: Int = math.log10(self.toDouble).toInt + 1
  def binaryDigits: Int = logBaseN(self.toDouble, 2.0).toInt
  infix def rem(that: Long): Long =
    self - math.abs(that) * math.floorDiv(self, math.abs(that))

object Digits:
  def unapply(s: Long): Option[Int] = Some(s.digits)
  def unapply(s: Int): Option[Int] = Some(s.digits)
