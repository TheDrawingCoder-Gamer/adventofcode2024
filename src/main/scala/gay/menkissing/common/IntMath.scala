package gay.menkissing.common

extension (self: Int) {
  def digits: Int = {
    math.log10(self.toDouble).toInt + 1
  }
}

extension (self: Long) {
  def digits: Int = {
    math.log10(self.toDouble).toInt + 1
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