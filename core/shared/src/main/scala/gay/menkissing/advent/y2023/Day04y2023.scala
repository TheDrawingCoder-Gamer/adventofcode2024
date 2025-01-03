package gay.menkissing.advent
package y2023

import scala.collection.mutable as mut

object Day04y2023 extends Problem[List[Day04y2023.Scratchcard], Int]:
  lazy val input = FileIO.getInput(2023, 4)

  case class Scratchcard(winningNumbers: Set[Int], numbers: List[Int]):
    def countWinners: Int =
      numbers.count(winningNumbers)
    def score: Int =
      countWinners match
        case 0 => 0
        case n => math.pow(2, n - 1).toInt

  def parse(input: String): List[Scratchcard] =
    input.linesIterator.map:
      case s"Card $_: $winners | $nums" =>
        Scratchcard(winners.split(raw"\s+").flatMap(_.toIntOption).toSet, nums.split(raw"\s+").flatMap(_.toIntOption).toList)
    .toList

  def part1(input: List[Scratchcard]): Int =
    input.map(_.score).sum

  def part2(input: List[Scratchcard]): Int =
    val size = input.length
    val map = (0 until size).map((_, 1)).to(mut.HashMap)

    input.zipWithIndex.foldLeft(0):
      case (acc, (card, idx)) =>
        val nCards = map(idx)
        val nWins = card.countWinners
        (idx + 1 to idx + nWins).foreach: i =>
          map(i) = map(i) + nCards
        acc + nCards



