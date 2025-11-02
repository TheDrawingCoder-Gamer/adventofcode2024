package gay.menkissing.advent
package y2022

import cats.implicits.*
import gay.menkissing.common.*, ArityN.*
import cats.Show

object Day06 extends Problem:
  type Input = String
  type Output = Int

  def parse(str: String): String = str

  def process(input: String, size: Int): Int =
    input.toVector.sliding(size).indexWhere: it =>
      it.combinationsN[2].forall(i => i(0) != i(1))
    + size

  def part1(str: String): Int = process(str, 4)
  def part2(str: String): Int = process(str, 14)

  lazy val input = FileIO.getInput(2022, 6)
