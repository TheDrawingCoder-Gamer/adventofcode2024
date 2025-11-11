package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import cats.syntax.all.*

object Day04 extends Problem:
  type Input = (List[Int], List[BingoCard])
  type Output = Int

  type BingoCard = Grid[Int]
  type RealBingoCard = Grid[(Int, Boolean)]

  lazy val input = FileIO.getInput(2021, 4)

  def parse(str: String): (List[Int], List[BingoCard]) =
    val blocks = str.split("\n\n")
    val callouts = blocks.head.trim.split(',').map(_.toInt)
    val cards =
      blocks.tail.map: block =>
        Grid:
          block.linesIterator.map: it =>
            it.trim.split(raw"\s+").map(_.toInt)

    (callouts.toList, cards.toList)

  def makeCard(base: BingoCard): RealBingoCard = base.map(it => (it, false))

  extension (card: RealBingoCard)
    def acceptCallout(callout: Int): RealBingoCard =
      card.map((n, x) => (n, (n == callout) || x))

    def score(lastCall: Int): Int =
      card.flatten.filterNot(_._2).map(_._1).sum * lastCall

    def won: Boolean =
      card.rows.exists(_.forall(_._2)) || card.columns.exists(_.forall(_._2))

  def part1(input: (List[Int], List[BingoCard])): Int =
    val (callouts, cards) = input
    val realCards = cards.map(makeCard)

    callouts.foldLeft((Option.empty[Int], realCards)):
      case ((r @ Some(_), c), _) => (r, c)
      case ((_, cards), callout) =>
        val newCards = cards.map(_.acceptCallout(callout))
        (newCards.find(_.won).map(_.score(callout)), newCards)
    ._1.get

  def part2(input: (List[Int], List[BingoCard])): Int =
    val (callouts, cards) = input
    val realCards = cards.map(makeCard)

    callouts.foldLeft((Option.empty[Int], realCards.toSet)):
      case ((f, cards), callout) =>
        val newCards = cards.map(_.acceptCallout(callout))
        val winningCards = newCards.filter(_.won)
        (
          newCards.find(_.won).map(_.score(callout)).orElse(f),
          newCards -- winningCards
        )
    ._1.get
