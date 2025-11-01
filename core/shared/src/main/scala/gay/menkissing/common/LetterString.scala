package gay.menkissing.common

// Given a lowercase string of alpha characters, returns an Int
// Maxes out at 6 characters long
object LetterString:
  def apply(str: String): Int =
    str.foldLeft(0):
      case (acc, l) => (acc << 5) + (l - 'a' + 1)

  def unapply(n: Int): String =
    val stringBuilder = new StringBuilder()
    (0 until 6).foreach: i =>
      val num = (n >> (5 * i)) & 0b11111
      if num > 0 then stringBuilder.append((num - 1 + 'a').toChar)
    stringBuilder.mkString

  // this should NOT be what u should be using all the time
  // if u need this all the time its probably better to just use
  // a normal string
  def charAt(n: Int, len: Int)(i: Int): Char =
    (((n >> (5 * (i + (6 - len)))) & 0b11111) - 1 + 'a').toChar
