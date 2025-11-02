package gay.menkissing.advent
package y2015

import cats.Show

object Day02 extends Problem:
  type Input = List[(Int, Int, Int)]

  type Output = Int

  def parse(str: String): List[(Int, Int, Int)] =
    str.linesIterator.map:
      case s"${x}x${y}x${z}" => (x.toInt, y.toInt, z.toInt)
    .toList

  def part1(input: List[(Int, Int, Int)]): Int =
    input.map: (l, w, h) =>
      val lw = l * w
      val wh = w * h
      val hl = h * l
      val total = 2 * lw + 2 * wh + 2 * hl
      val smallest = math.min(math.min(lw, wh), hl)
      total + smallest
    .sum

  def part2(input: List[(Int, Int, Int)]): Int =
    input.map: (l, w, h) =>
      val lwp = 2 * l + 2 * w
      val whp = 2 * w + 2 * h
      val hlp = 2 * h + 2 * l
      val ribbon = math.min(math.min(lwp, whp), hlp)
      val bow = l * w * h
      bow + ribbon
    .sum

  lazy val input: String = FileIO.getInput(2015, 2)
