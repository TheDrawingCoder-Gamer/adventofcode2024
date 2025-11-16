package gay.menkissing.advent
package y2020

import cats.*
import cats.implicits.*

import gay.menkissing.common.*, ArityN.*

object Day09 extends Problem:
  type Input = Vector[Long]
  type Output = Long

  lazy val input = FileIO.getInput(2020, 9)

  def parse(input: String): Vector[Long] =
    input.linesIterator.map(_.toLong).toVector

  def weakNum(input: Vector[Long]): Long =
    input.sliding(26).findMap: v =>
      val init = v.init
      val last = v.last
      Option.when(init.combinationsN[2].forall(_ + _ != last))(last)
    .get

  def part1(input: Vector[Long]): Long = weakNum(input)

  def part2(input: Vector[Long]): Long =
    val num = weakNum(input)

    val (min, max) =
      Monad[Id].tailRecM((0, 1, input(0))): (min, max, sum) =>
        sum.compareTo(num) match
          case -1 => Left((min, max + 1, sum + input(max)))
          case 1  => Left((min + 1, max, sum - input(min)))
          case 0  => Right((min, max))

    val nums = (min until max).map(input)
    nums.min + nums.max
