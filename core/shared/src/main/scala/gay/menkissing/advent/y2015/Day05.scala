package gay.menkissing.advent
package y2015

import cats.*
import cats.implicits.*

object Day05 extends Problem[String, Int]:
  def parse(str: String): String = str

  def part1(input: String): Int =
    val vowels = "aeiou"
    val invalidParts = List("ab", "cd", "pq", "xy")
    input.linesIterator.count: l =>
      l.count(vowels.contains) >= 3 && l.toList.sliding2.exists((a, b) => a == b) && invalidParts.forall(it => !l.contains(it))

  def part2(input: String): Int =
    input.linesIterator.count: l =>
      val ls = l.toList.zipWithIndex.sliding2.map:
        case ((a, i), (b, j)) => (a.toString + b.toString, List(i, j))
      ls.exists((a, i) => ls.exists((b, j) => a == b && i.intersect(j).isEmpty)) && l.toList.sliding3.exists((a, _, b) => a == b)

  lazy val input: String = FileIO.getInput(2015, 5)
