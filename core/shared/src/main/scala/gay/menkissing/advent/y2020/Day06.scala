package gay.menkissing.advent
package y2020

import cats.Show

object Day06 extends Problem:
  type Input = List[List[Set[Char]]]
  type Output = Int
  def showOutput: Show[Int] = summon

  override def parse(str: String): List[List[Set[Char]]] =
    str.split("\n\n").map: block =>
      block.linesIterator.map(_.toSet).toList
    .toList

  override def part1(input: List[List[Set[Char]]]): Int =
    input.map: block =>
      block.reduce(_ | _).size
    .sum

  override def part2(input: List[List[Set[Char]]]): Int =
    input.map: block =>
      block.reduce(_ & _).size
    .sum

  override lazy val input: String = FileIO.getInput(2020, 6)
