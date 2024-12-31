package gay.menkissing.advent
package y2022

import cats.implicits.*
import cats.{Applicative, Traverse}

object Day04y2022 extends Problem[List[Day04y2022.Jobs], Int] {
  case class Jobs(left: Range, right: Range) {
    def useless: Boolean = {
      val size = symdiff(left.toSet, right.toSet).size
      val expected = (left.size max right.size) - (left.size min right.size)
      size == expected
    }

    def anyMatch: Boolean = {
      val size = symdiff(left.toSet, right.toSet).size
      val expected = left.size + right.size
      // if any match between expected will be too high
      size != expected
    }
  }

  object Jobs {
    def parse(input: String): Jobs = {
      val parts = input.split(',')
      val l = parts(0)
      val r = parts(1)
      Jobs(parseRange(l), parseRange(r))

    }

    private def parseRange(input: String): Range = {
      val parts = input.trim().split('-').take(2)
      val l = parts(0)
      val r = parts(1)
      l.toInt to r.toInt
    }
  }
  def parse(input : String) : List[Jobs] = {
    input.linesIterator.map(Jobs.parse).toList
  }
  def part1(input: List[Jobs]): Int = {
    input.count(_.useless)
  }
  def part2(input: List[Jobs]): Int = {
    input.count(_.anyMatch)
  }

  def symdiff[A](l: Set[A], r: Set[A]) = {
    (l ++ r) diff (l intersect r)
  }
  
  lazy val input = FileIO.getInput(2022, 4)

}

