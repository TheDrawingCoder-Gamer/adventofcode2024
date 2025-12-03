package gay.menkissing.advent
package y2025

import cats.collections.Range

object Day02 extends Problem:
  type Input = List[Range[Long]]
  type Output = Long

  def input = FileIO.getInput(2025, 2)

  def parse(str: String): List[Range[Long]] =
    str.trim.split(",").map:
      case s"$l-$r" => Range(l.toLong, r.toLong)
    .toList

  def invalidP1(n: Long): Boolean =
    val str = n.toString
    val len = str.length
    (len & 1) == 0 && str.take(len >> 1) == str.drop(len >> 1)

  def invalidP2(n: Long): Boolean =
    val str = n.toString
    val len = str.length
    (1 to len >> 1).filter(len % _ == 0).exists: size =>
      val rep = len / size
      val bit = str.take(size)
      (bit * rep) == str

  def part1(input: List[Range[Long]]): Long =
    input.map:
      _.toIterator.filter(invalidP1).sum
    .sum

  def part2(input: List[Range[Long]]): Long =
    input.map:
      _.toIterator.filter(invalidP2).sum
    .sum
