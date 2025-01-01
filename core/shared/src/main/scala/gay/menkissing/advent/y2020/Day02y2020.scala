package gay.menkissing.advent
package y2020

object Day02y2020 extends Problem[List[Day02y2020.PolicyEntry], Int]:
  case class PolicyEntry(left: Int, right: Int, of: Char, password: String)

  override def parse(str: String): List[PolicyEntry] =
    str.linesIterator.map:
      case s"$lower-$upper $char: $password" =>
        assert(char.length == 1)
        PolicyEntry(lower.toInt, upper.toInt, char.head, password.trim())
    .toList

  override def part1(input: List[PolicyEntry]): Int =
    input.count: entry =>
      (entry.left to entry.right).contains(entry.password.count(_ == entry.of))

  override def part2(input: List[PolicyEntry]): Int =
    input.count: entry =>
      val l = entry.password(entry.left - 1) == entry.of
      val r = entry.password(entry.right - 1) == entry.of
      l ^ r

  override lazy val input: String = FileIO.getContentsOf("y2020/day02.txt")
