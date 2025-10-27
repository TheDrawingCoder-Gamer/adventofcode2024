package gay.menkissing.common

import scala.math.Integral.Implicits.*



extension (self: Byte)
  inline def toUInt: Int =
    // copied implementation of java, but this is inline so better???
    self & 255

extension[T](self: T)(using integral: Integral[T])
  // eucledian remainder ?
  infix def erem(that: T): T =
    val mod = integral.rem(self, that)
    if integral.compare(mod, integral.zero) < 0 then mod + that else mod


extension (self: Int) {
  def digits: Int = {
    math.log10(self.toDouble).toInt + 1
  }

  def binaryDigits: Int = {
    logBaseN(self.toDouble, 2.0).toInt
  }

  infix def rem(that: Int): Int = {
    val mod = self % that
    if mod < 0 then mod + that else mod
  }
  
  infix def ceilDiv(that: Int): Int = {
    math.ceil(self.toDouble / that.toDouble).toInt
  }
  infix def floorDiv(that: Int): Int = {
    math.floor(self.toDouble / that.toDouble).toInt
  }
}

extension (self: Long) {
  def digits: Int = {
    math.log10(self.toDouble).toInt + 1
  }
  def binaryDigits: Int = {
    logBaseN(self.toDouble, 2.0).toInt
  }
  infix def rem(that: Long): Long = {
    self - math.abs(that) * math.floorDiv(self, math.abs(that))
  }
}

object Digits {
  def unapply(s: Long): Option[Int] = {
    Some(s.digits)
  }
  def unapply(s: Int): Option[Int] = {
    Some(s.digits)
  }
}