package gay.menkissing.advent
package y2023

import cats.implicits.*
import cats.syntax.functorFilter.*
import cats.*

object Day02 extends Problem[List[Day02.Game], Int]:
  case class Pull(r: Int, g: Int, b: Int):
    def +(that: Pull) =
      zipWith(_ + _)(that)

    infix def max(that: Pull) =
      zipWith(math.max)(that)

    def zipWith(i: (Int, Int) => Int)(that: Pull) =
      Pull(i(this.r, that.r), i(this.g, that.g), i(this.b, that.b))

  object Pull:
    def fromString(it: String): Pull =
      var r = 0
      var g = 0
      var b = 0
      it.split(',').foreach: item =>
        item.trim match
          case s"$n red" =>
            r = n.toInt
          case s"$n green" =>
            g = n.toInt
          case s"$n blue" =>
            b = n.toInt
          // shouldn't happen ; )
          case _ => assert(false)
      Pull(r, g, b)

  case class Game(id: Int, pulls: List[Pull])

  object Game:
    def fromString(it: String): Game =
      it match
        case s"Game $n: $rest" =>
          Game(n.toInt, rest.split(";").map(Pull.fromString).toList)
        case _ => assert(false)


  lazy val input = FileIO.getInput(2023, 2)

  def parse(input: String): List[Game] = input.linesIterator.map(Game.fromString).toList



  def part1(input: List[Game]): Int =
    input.mapFilter:
      case Game(id, pulls) =>
        val res = pulls.reduce(_ max _)
        Option.when(res.r <= 12 && res.g <= 13 && res.b <= 14)(id)
    .sum

  def part2(input: List[Game]): Int =
    input.map:
      case Game(_, pulls) =>
        val res = pulls.reduce(_ max _)
        res.r * res.g * res.b
    .sum

