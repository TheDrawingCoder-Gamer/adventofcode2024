package gay.menkissing.advent
package y2020

import cats.*
import cats.implicits.*
import collection.mutable
import gay.menkissing.common.*, ArityN.*

object Day10 extends Problem:
  type Input = List[Int]
  type Output = Long

  override def parse(str: String): List[Int] =
    str.linesIterator.map(_.toInt).toList

  override def part1(input: List[Int]): Long =
    // Only "correct" way to use all adapters in ascending order is by sorting them
    val goodInput = input.sorted
    val (d1s, d3s) =
      goodInput.prepended(0).slidingN[2].foldLeft((0L, 0L)):
        case ((d1s, d3s), (l, r)) =>
          val diff = r - l
          diff match
            case 1 => (d1s + 1L, d3s)
            case 3 => (d1s, d3s + 1L)
            case _ => (d1s, d3s)
    d1s * (d3s + 1L)

  def part2(input: List[Int]): Long =

    val maxPlug = input.max + 3
    val goodInput = input.sorted.prepended(0)

    val dict = mutable.HashMap[Int, Long](0 -> 1L).withDefault(_ => 0L)

    goodInput.foreach: x =>
      dict(x + 1) += dict(x)
      dict(x + 2) += dict(x)
      dict(x + 3) += dict(x)

    dict(maxPlug)

  def input: String = FileIO.getInput(2020, 10)
