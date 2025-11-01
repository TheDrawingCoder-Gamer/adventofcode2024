package gay.menkissing.advent
package y2021

import cats.implicits.*
import gay.menkissing.common.ArityN.*
import cats.Show

object Day01 extends Problem:
  type Input = List[Int]
  type Output = Int
  def showOutput: Show[Int] = summon
  override def parse(str: String): List[Int] =
    str.linesIterator.map(_.toInt).toList

  def measureScans(scans: List[Int]): Int =
    scans.slidingN[2].count: (l, r) =>
      l < r
  override def part1(input: List[Int]): Int = measureScans(input)

  def slidingSums(scans: List[Int]): List[Int] =
    scans.sliding(3).map(_.sum).toList

  override def part2(input: List[Int]): Int = measureScans(slidingSums(input))

  override lazy val input: String = FileIO.getInput(2021, 1)
