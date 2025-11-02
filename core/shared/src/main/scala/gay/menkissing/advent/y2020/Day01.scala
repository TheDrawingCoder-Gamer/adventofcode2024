package gay.menkissing.advent
package y2020

import gay.menkissing.common.*, ArityN.*
import cats.Show
object Day01 extends Problem:
  type Input = List[Int]
  type Output = Int
  override def parse(str: String): List[Int] =
    str.linesIterator.map(_.toInt).toList

  override def part1(input: List[Int]): Int =
    input.combinationsN[2].flatMap:
      case (x, y) => Option.when(x + y == 2020)(x * y)
    .next()

  override def part2(input: List[Int]): Int =
    input.combinationsN[3].flatMap:
      case (x, y, z) => Option.when(x + y + z == 2020)(x * y * z)
    .next()

  override lazy val input: String = FileIO.getInput(2020, 1)
