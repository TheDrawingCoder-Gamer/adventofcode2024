package gay.menkissing.advent
package y2025

import scala.annotation.tailrec

object Day03 extends Problem:
  type Input = List[List[Int]]
  type Output = Long

  def input: String = FileIO.getInput(2025, 3, test = true)

  def parse(str: String): Input =
    str.linesIterator.map:
      _.map(_.asDigit).toList
    .toList

  def maxBank(bank: List[Int]): Int =
    val (maxFirst, maxFirstIdx) = bank.dropRight(1).zipWithIndex.maxBy(_._1)
    val maxLast = bank.drop(maxFirstIdx + 1).max
    maxFirst * 10 + maxLast

  // basically just a generalization of the above code, with the rigid `2` substituted with an input
  // We reserve the last N - 1 batteries as fallbacks in case the maximum isn't in the first section
  // And then find the index of the maximum value in that section
  // To find the next bit, we exclude any numbers before and including the one we selected,
  // Then recurse
  // (This isn't represented above as for 2 batteries our second step will be our end step, and the index isn't needed after that point)
  def maxBankP2(bank: List[Int], batteries: Int): Long =
    @tailrec
    def go(bank: List[Int], batteries: Int, acc: Long): Long =
      if batteries == 0 then acc
      else
        val (bit, index) =
          bank.dropRight(batteries - 1).zipWithIndex.maxBy(_._1)
        go(bank.drop(index + 1), batteries - 1, acc * 10 + bit)
    go(bank, batteries, 0L)

  def part1(input: List[List[Int]]): Long = input.map(maxBank).sum

  def part2(input: List[List[Int]]): Long = input.map(maxBankP2(_, 12)).sum
