package gay.menkissing.advent
package y2021

object Day01 extends Problem[List[Int], Int]:
  override def parse(str: String): List[Int] =
    str.linesIterator.map(_.toInt).toList

  def measureScans(scans: List[Int]): Int =
    scans.sliding(2).count: ls =>
      ls.head < ls.tail.head
  override def part1(input: List[Int]): Int =
    measureScans(input)

  def slidingSums(scans: List[Int]): List[Int] =
    scans.sliding(3).map(_.sum).toList

  override def part2(input: List[Int]): Int =
    measureScans(slidingSums(input))

  override lazy val input: String = FileIO.getInput(2021, 1)

