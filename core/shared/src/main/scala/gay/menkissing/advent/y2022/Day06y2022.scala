package gay.menkissing.advent
package y2022

object Day06y2022 extends Problem[String, Int]:
  def parse(str: String): String = str

  def process(input: String, size: Int): Int =
    input.sliding(size).indexWhere: it =>
      it.combinations(2).forall(i => i.charAt(0) != i.charAt(1))
    + size

  def part1(str: String): Int = process(str, 4)
  def part2(str: String): Int = process(str, 14)

  lazy val input = FileIO.getInput(2022, 6)
