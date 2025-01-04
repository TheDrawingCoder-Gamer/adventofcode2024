package gay.menkissing.advent
package y2021

import cats.syntax.all.*

import scala.annotation.tailrec

object Day02y2021 extends Problem[List[(Day02y2021.Dir, Int)], Int]:
  lazy val input = FileIO.getInput(2021, 2)

  enum Dir:
    case Forward
    case Down
    case Up

  object Dir:
    def parse(str: String): Dir =
      str match
        case "forward" => Dir.Forward
        case "down" => Dir.Down
        case "up" => Dir.Up

  def parse(input: String): List[(Dir, Int)] =
    input.linesIterator.map:
      case s"$dir $n" =>
        val d = Dir.parse(dir)
        (d, n.toInt)
    .toList


  def part1(input: List[(Dir, Int)]): Int =
    val (horz, down, up) = input.foldLeft((0, 0, 0)):
      case ((horz, down, up), (dir, n)) =>
        dir match
          case Dir.Forward => (horz + n, down, up)
          case Dir.Down => (horz, down + n, up)
          case Dir.Up => (horz, down, up + n)

    val depth = down - up

    depth * horz

  def part2(input: List[(Dir, Int)]): Int =
    @tailrec
    def go(rest: List[(Dir, Int)], aim: Int, depth: Int, x: Int): (Int, Int) =
      rest match
        case Nil => (x, depth)
        case head :: next =>
          head match
            case (Dir.Forward, i) => go(next, aim, depth + aim * i, x + i)
            case (Dir.Down,    i) => go(next, aim + i, depth, x)
            case (Dir.Up,      i) => go(next, aim - i, depth, x)
    val (x, depth) = go(input, 0, 0, 0)
    x * depth