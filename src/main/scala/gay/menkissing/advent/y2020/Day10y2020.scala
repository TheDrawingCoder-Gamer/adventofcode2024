package gay.menkissing.advent
package y2020

import cats.data.Chain
import cats.syntax.all.*

object Day10y2020 extends Problem[List[Int], Long]:
  override def parse(str: String): List[Int] =
    str.linesIterator.map(_.toInt).toList

  override def part1(input: List[Int]): Long =
    // Only "correct" way to use all adapters in ascending order is by sorting them
    val goodInput = input.sorted
    val (d1s, d3s) = goodInput.prepended(0).sliding(2).foldLeft((0L, 0L)):
      case ((d1s, d3s), List(l, r)) =>
        val diff = r - l
        diff match
          case 1 => (d1s + 1L, d3s)
          case 3 => (d1s, d3s + 1L)
          case _ => (d1s, d3s)
    d1s * (d3s + 1L)

  override def part2(input: List[Int]): Long =

    val maxPlug = input.max
    val goodInput = input.sorted
    val possibleSkips = goodInput.sliding4.map:
      case (x, y, z, w) =>
        if w - x <= 3 then
          3
        else if z - x <= 3 then
          2
        else if y - x <= 3 then
          1
        else 0
    val zippedSkips = goodInput.zip(possibleSkips)
    def isValid(ls: Vector[Int]): Boolean =
      ls.prepended(0).sliding2.forall:
        case (l, r) => r - l <= 3

    def go(curLs: Vector[Int]): Long =
      curLs.indices.map: idx =>
        val (l, r) = curLs.splitAt(idx)
        val next = l ++ r.tail
        if isValid(next) then
          1L + go(next)
        else
          0L
      .sum

    go(goodInput.toVector) + 1L
    ???


  override lazy val input: String = FileIO.getInput(2020, 10, true)

