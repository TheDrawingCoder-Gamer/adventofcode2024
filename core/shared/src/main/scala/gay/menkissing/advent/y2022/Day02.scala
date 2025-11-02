package gay.menkissing.advent
package y2022

import cats.Show

object Day02 extends Problem:
  type Input = List[RawThrow]
  type Output = Int

  enum RPSResult:
    case Loss, Draw, Win

    def score: Int =
      this match
        case Loss => 0
        case Draw => 3
        case Win  => 6

  enum RPS:
    case Rock, Paper, Scissors

    def beatsOther(that: RPS): Boolean =
      this match
        case Rock     => that == Scissors
        case Paper    => that == Rock
        case Scissors => that == Paper

    def beats: RPS =
      this match
        case Rock     => Scissors
        case Paper    => Rock
        case Scissors => Paper

    def beatenBy: RPS =
      this match
        case Rock     => Paper
        case Paper    => Scissors
        case Scissors => Rock

    def result(that: RPS): RPSResult =
      if this.beatsOther(that) then RPSResult.Win
      else if that.beatsOther(this) then RPSResult.Loss
      else RPSResult.Draw

    def score: Int = ordinal + 1

  case class RawThrow(opponent: RPS, player: Int):
    def toStrategy: Strategy = Strategy(opponent, RPSResult.fromOrdinal(player))
    def toThrow: Throw = Throw(opponent, RPS.fromOrdinal(player))

  case class Throw(opponent: RPS, player: RPS):
    def score: Int =
      // real
      val shapeScore = player.score
      val result = player.result(opponent).score
      result + shapeScore

  case class Strategy(opponent: RPS, player: RPSResult):
    def toThrow: Throw =
      val newPlayer =
        player match
          case RPSResult.Win  => opponent.beatenBy
          case RPSResult.Draw => opponent
          case RPSResult.Loss => opponent.beats

      Throw(opponent, newPlayer)

  def parse(input: String): List[RawThrow] =
    input.linesIterator.map: line =>
      val parts = line.split(' ').take(2)
      // 65 == A
      // 88 = X
      val opponent = RPS.fromOrdinal(parts(0).charAt(0) - 'A')
      val player = (parts(1).charAt(0) - 'X').toInt
      RawThrow(opponent, player)
    .toList

  def part1(input: List[RawThrow]): Int = input.map(_.toThrow.score).sum

  def part2(input: List[RawThrow]): Int =
    input.map(_.toStrategy.toThrow.score).sum

  lazy val input = FileIO.getInput(2022, 2)
