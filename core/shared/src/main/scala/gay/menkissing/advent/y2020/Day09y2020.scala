package gay.menkissing.advent
package y2020

import cats.implicits.*
import cats.syntax.all.*

import gay.menkissing.common.*

object Day09y2020 extends Problem[Vector[Long], Long]:

  lazy val input = FileIO.getInput(2020, 9)

  def parse(input: String): Vector[Long] =
    input.linesIterator.map(_.toLong).toVector

  def weakNum(input: Vector[Long]): Long =
    input.sliding(26).findMap:
      case Unsnoc(init, last) =>
        Option.when {
          init.combinations(2).forall:
            case Vector(l, r) => l + r != last
        }(last)
    .get


  def part1(input: Vector[Long]): Long = weakNum(input)

  def part2(input: Vector[Long]): Long =
    val num = weakNum(input)


    val (min, max) = unfoldedMap((0, 1, input(0))): (min, max, sum) =>
      sum.compareTo(num) match
        case -1 => Right((min, max + 1, sum + input(max)))
        case  1 => Right((min + 1, max, sum - input(min)))
        case  0 => Left((min, max))

    val nums = (min until max).map(input)
    nums.min + nums.max
