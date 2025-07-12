package gay.menkissing.advent
package y2020

object Day01 extends Problem[List[Int], Int]:
  override def parse(str: String): List[Int] =
    str.linesIterator.map(_.toInt).toList

  override def part1(input: List[Int]): Int =
    input.combinations(2).flatMap { case List(x, y) =>
      Option.when(x + y == 2020)(x * y)
    }.next()

  override def part2(input: List[Int]): Int =
    input.combinations(3).flatMap { case List(x, y, z) =>
      Option.when(x + y + z == 2020)(x * y * z)
    }.next()

  override lazy val input: String = FileIO.getInput(2020, 1)
