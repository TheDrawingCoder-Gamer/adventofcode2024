package gay.menkissing.advent
package y2025

import cats.collections.{Diet, Range}
import gay.menkissing.common.*
import algebras.given

object Day05 extends Problem:
  type Input = (ranges: Diet[Long], ingredients: List[Long])
  type Output = Long

  def input: String = FileIO.getInput(2025, 5)

  def parse(str: String): Input =
    val Array(ranges, ings) = str.split("\n\n").runtimeChecked
    val diet =
      ranges.linesIterator.map:
        case s"$s-$e" => Range(s.toLong, e.toLong)
      .foldLeft(Diet.empty[Long])((acc, range) => acc.addRange(range))
    (
      ranges = diet,
      ingredients = ings.linesIterator.map(_.toLong).toList
    )

  def part1(input: Input): Long = input.ingredients.count(input.ranges.contains)

  def part2(input: Input): Long =
    input.ranges.foldLeftRange(0L): (acc, range) =>
      acc + range.size
