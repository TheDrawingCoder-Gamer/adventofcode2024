package gay.menkissing.common

extension (self: Int) {
  def digits: Int = {
    math.log10(self.toDouble).toInt + 1
  }

  def binaryDigits: Int = {
    logBaseN(self.toDouble, 2.0).toInt
  }

  infix def rem(that: Int): Int = {
    // these don't cancel out due to floor div
    self - that.abs * (self.toDouble / that.toDouble.abs).floor.toInt
  }
  
  infix def ceilDiv(that: Int): Int = {
    math.ceil(self.toDouble / that.toDouble).toInt
  }
}

extension (self: Long) {
  def digits: Int = {
    math.log10(self.toDouble).toInt + 1
  }
  def binaryDigits: Int = {
    logBaseN(self.toDouble, 2.0).toInt
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