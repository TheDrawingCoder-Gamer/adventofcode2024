package gay.menkissing.advent
package y2022

object Day01 extends Problem[List[List[Int]], Int]:
  def parse(str: String): List[List[Int]] =
    str.split("\n\n").map: block =>
      block.linesIterator.map(_.toInt).toList
    .toList

  def part1(elfs: List[List[Int]]): Int =
    val summedElfs = elfs.map(it => it.sum)

    summedElfs.max

  def part2(elfs: List[List[Int]]): Int =
    val summedElfs: List[Int] = elfs.map(it => it.sum).sorted.reverse
    summedElfs.take(3).sum

  lazy val input = FileIO.getInput(2022, 1)
