package gay.menkissing.advent
package y2015

object Day11 extends Problem:
  type Input = String
  type Output = String

  def parse(str: String): String = str.trim

  val iolBytes = "iol".getBytes("US-ASCII").map(it => (it - 'a').toByte)
  val shortenedAlpha = "abcdefghjkmnpqrstuvwxyz".getBytes("US-ASCII")

  def test(input: Array[Byte]): Boolean =

    !input.exists(iolBytes.contains) && locally:
      val ls =
        input.zipWithIndex.sliding(2).flatMap:
          case Array((a, i), (b, j)) => Option.when(a == b)(a, List(i, j))
        .toArray
      ls.flatMap(x => ls.map(y => (x, y))).exists:
        case ((a, i), (b, j)) => a != b && i.intersect(j).isEmpty
    && input.sliding(3).exists:
      case Array(a, b, c) => c - b == 1 && b - a == 1

  def advance(arr: Array[Byte]): Array[Byte] =
    var i = arr.length - 1
    while i >= 0 do
      arr(i) = (arr(i) + 1).toByte
      if arr(i) > 25 then
        arr(i) = 0
        i -= 1
      else
        /*
        arr(i).toChar match
          case 'i' => arr(i) = 'j'
          case 'o' => arr(i) = 'p'
          case 'l' => arr(i) = 'm'
          case _   => ()
         */
        i = -1

    arr

  def toBytes(str: String): Array[Byte] =
    str.map(it => (it - 'a').toByte).toArray
  def fromBytes(bytes: Array[Byte]): String =
    new String(bytes.map(it => (it + 'a').toByte), "US-ASCII")
  def part1(input: String): String =
    val bytes =
      Iterator.iterate(advance(toBytes(input)))(advance).find(test).get
    fromBytes(bytes)

  def part2(input: String): String =
    val bytes =
      Iterator.iterate(advance(toBytes(input)))(advance)
        .dropWhile(it => !test(it)).drop(1).find(test).get
    fromBytes(bytes)

  def input: String = FileIO.getInput(2015, 11)
