package gay.menkissing.advent
package y2015

object Day17 extends Problem[List[Int], Int]:
  lazy val input = FileIO.getInput(2015, 17)

  // yes WE are giving our classes identity
  class Box[A](val value: A)

  def parse(str: String): List[Int] = str.linesIterator.map(_.toInt).toList

  val inputSize = 150
  def part1(input: List[Int]): Int =
    input.map(it => Box(it)).toSet.subsets()
      .count(_.toList.map(_.value).sum == inputSize)

  def part2(input: List[Int]): Int =
    val values =
      input.map(it => Box(it)).toSet.subsets()
        .filter(_.toList.map(_.value).sum == inputSize).toList
    val size = values.minBy(_.size).size
    values.count(_.size == size)
