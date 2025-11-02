package gay.menkissing.advent
package y2024

import gay.menkissing.common.ArityN.*
import cats.Show

object Day02 extends Problem:

  type Input = List[List[Int]]
  type Output = Int
  override lazy val input: String = FileIO.getInput(2024, 2)

  override def parse(str: String): List[List[Int]] =
    str.linesIterator.map(_.split(' ').map(_.toInt).toList).toList

  def testSafe(report: List[Int]): Boolean =
    val inc = Integer.signum(report.tail.head - report.head)
    if inc == 0 then false
    else
      report.slidingN[2].forall { case (x, y) =>
        val diff = y - x
        Math.abs(diff) >= 1 && Math.abs(diff) <= 3 &&
        Integer.signum(diff) == inc
      }

  // a valid "final" configuration must:
  // be all increasing or all decreasing
  // two adjacent levels differ by at least one or at most three
  // this implies...
  // that all numbers are unique
  // that the list is strictly monotonic
  // that the largest number is at most 3 * length of the list + smallest number
  // (assuming its at the end)

  def testSafeDampener(report: List[Int]): Boolean =
    // I HATE THAT THIS IS INEFFICIENT
    testSafe(report) ||
      (
        report.indices.exists: idx =>
          val newReport = report.take(idx) ++ report.drop(idx + 1)
          testSafe(newReport)
      )

  override def part1(input: List[List[Int]]): Int = input.count(testSafe)

  override def part2(input: List[List[Int]]): Int =
    input.count(testSafeDampener)
