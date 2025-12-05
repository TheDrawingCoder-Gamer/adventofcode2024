package gay.menkissing.advent
package y2025

object Day05Writeup extends Problem:
  type Input = (List[LRange], List[Long])
  type Output = Long

  final case class LRange(start: Long, end: Long):
    def size: Long = end - start + 1
    def contains(n: Long): Boolean = n >= start && n <= end
    infix def intersect
      (
        t: LRange
      ): Option[LRange] =
      Option.when(end >= t.start && start <= t.end):
        LRange(start max t.start, end min t.end)
    infix def -
      (
        t: LRange
      ): Set[LRange] =
      this intersect t match
        case Some(h) =>
          var daSet = Set.empty[LRange]
          if start != h.start then daSet += LRange(start, h.start - 1)
          if end != h.end then daSet += LRange(h.end + 1, end)
          daSet
        case _ => Set(this)

  def input: String = FileIO.getInput(2025, 5)

  def parse(str: String): Input =
    val Array(ranges, ings) = str.split("\n\n").runtimeChecked
    (
      ranges =
        ranges.linesIterator.map:
          case s"$s-$e" => LRange(s.toLong, e.toLong)
        .toList,
      ingredients = ings.linesIterator.map(_.toLong).toList
    )

  def part1(input: Input): Long =
    val (ranges, ingredients) = input
    ingredients.count(ing => ranges.exists(_.contains(ing)))

  def part2(input: Input): Long =
    val (ranges, _) = input
    val head :: tail = ranges.runtimeChecked
    val combinedRanges =
      ranges.foldLeft(Set.empty[LRange]): (acc, range) =>
        val removed = acc.flatMap(_ - range)
        removed + range
    // toIterator because Set is not a functor and hates you
    combinedRanges.toIterator.map(_.size).sum
