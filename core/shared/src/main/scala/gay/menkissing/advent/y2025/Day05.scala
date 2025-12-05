package gay.menkissing.advent
package y2025

object Day05 extends Problem:
  type Input = (ranges: List[LRange], ingredients: List[Long])
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
    def combine(t: LRange): Set[LRange] =
      this intersect t match
        case Some(v) => (t - v) ++ (this - v) + t
        case _       => Set(this, t)

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
    input.ingredients.count(ing => input.ranges.exists(_.contains(ing)))

  def part2(input: Input): Long =
    val head :: tail = input.ranges.runtimeChecked
    val ranges =
      input.ranges.foldLeft(Set.empty[LRange]): (acc, range) =>
        val removed = acc.flatMap(_ - range)
        removed + range
    // toIterator because Set is not a functor and hates you
    ranges.toIterator.map(_.size).sum
