package gay.menkissing.advent
package y2021

import cats.syntax.all.*

import scala.annotation.tailrec
import cats.Show

object Day02 extends Problem:
  type Input = List[(Dir, Int)]
  type Output = Int
  def showOutput: Show[Int] = summon

  lazy val input = FileIO.getInput(2021, 2)

  enum Dir:
    case Forward
    case Down
    case Up

  object Dir:
    def parse(str: String): Dir =
      str match
        case "forward" => Dir.Forward
        case "down"    => Dir.Down
        case "up"      => Dir.Up

  def parse(input: String): List[(Dir, Int)] =
    input.linesIterator.map:
      case s"$dir $n" =>
        val d = Dir.parse(dir)
        (d, n.toInt)
    .toList

  def part1(input: List[(Dir, Int)]): Int =
    val (horz, depth) =
      input.foldLeft((0, 0)):
        case ((horz, depth), (dir, n)) =>
          dir match
            case Dir.Forward => (horz + n, depth)
            case Dir.Down    => (horz, depth + n)
            case Dir.Up      => (horz, depth - n)

    depth * horz

  def part2(input: List[(Dir, Int)]): Int =
    @tailrec
    def go(rest: List[(Dir, Int)], aim: Int, depth: Int, x: Int): (Int, Int) =
      rest match
        case Nil          => (x, depth)
        case head :: next =>
          head match
            case (Dir.Forward, i) => go(next, aim, depth + aim * i, x + i)
            case (Dir.Down, i)    => go(next, aim + i, depth, x)
            case (Dir.Up, i)      => go(next, aim - i, depth, x)
    val (x, depth) = go(input, 0, 0, 0)
    x * depth
