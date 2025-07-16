package gay.menkissing.advent
package y2015

import cats.*
import cats.syntax.all.*
import gay.menkissing.common.bfsFoldl

object Day09 extends Problem[Map[(String, String), Int], Int]:
  def parse(str: String): Map[(String, String), Int] =
    str.linesIterator.map:
      case s"$x to $y = $n" =>
        ((x, y), n.toInt)
    .toMap


  def calc(input: Map[(String, String), Int])(using Monoid[Int]): Set[Int] =
    val locs = input.keys.flatMap((a, b) => List(a, b)).toSet

    locs.map: x =>
      bfsFoldl((x, Set(x), 0)): (curPos, visited, dist) =>
        val distances =
          input.flatMap:
                 case ((l, r), v) if l == curPos =>
                   Some((r, v))
                 case ((l, r), v) if r == curPos =>
                   Some((l, v))
                 case _ => None
               .filter((k, v) => !visited.contains(k))

        if distances.isEmpty then
          Right(dist)
        else
          Left(distances.map((x, y) => (x, visited + x, dist + y)))

  def part1(input: Map[(String, String), Int]): Int =
    // accidentally started to copy 2024 Day21
    given Monoid[Int]:
      def empty: Int = Int.MaxValue

      def combine(a: Int, b: Int): Int = a min b
    calc(input).min





  def part2(input: Map[(String, String), Int]): Int =
    given Monoid[Int]:
      def empty: Int = Int.MinValue

      def combine(a: Int, b: Int): Int = a max b
    calc(input).max
  lazy val input: String = FileIO.getInput(2015, 9)
