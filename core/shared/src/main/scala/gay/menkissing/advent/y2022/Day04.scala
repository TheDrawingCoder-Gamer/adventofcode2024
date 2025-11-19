package gay.menkissing.advent
package y2022

object Day04 extends Problem:
  type Input = List[Jobs]
  type Output = Int

  // j*bs...
  final case class Jobs(left: Range, right: Range):
    def useless: Boolean =
      val size = symdiff(left.toSet, right.toSet).size
      val expected = (left.size max right.size) - (left.size min right.size)
      size == expected

    def anyMatch: Boolean =
      val size = symdiff(left.toSet, right.toSet).size
      val expected = left.size + right.size
      // if any match between expected will be too high
      size != expected

  object Jobs:
    def parse(input: String): Jobs =
      val parts = input.split(',')
      val l = parts(0)
      val r = parts(1)
      Jobs(parseRange(l), parseRange(r))

    private def parseRange(input: String): Range =
      val parts = input.trim().split('-').take(2)
      val l = parts(0)
      val r = parts(1)
      l.toInt to r.toInt

  def parse(input: String): List[Jobs] =
    input.linesIterator.map(Jobs.parse).toList

  def part1(input: List[Jobs]): Int = input.count(_.useless)

  def part2(input: List[Jobs]): Int = input.count(_.anyMatch)

  def symdiff[A](l: Set[A], r: Set[A]) = (l ++ r) diff (l intersect r)

  def input = FileIO.getInput(2022, 4)
