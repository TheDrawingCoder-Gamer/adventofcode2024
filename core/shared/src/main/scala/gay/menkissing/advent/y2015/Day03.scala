package gay.menkissing.advent
package y2015

import gay.menkissing.common.Direction2D

import scala.collection.mutable
import gay.menkissing.common.Vec2i

object Day03 extends Problem[String, Int]:
  def parse(str: String): String = str

  def part1(input: String): Int =
    val set = mutable.Set[Vec2i](Vec2i(0, 0))

    input.foldLeft(Vec2i(0, 0)): (acc, r) =>
      val dir =
        r match
          case '^' => Direction2D.Up
          case '>' => Direction2D.Right
          case '<' => Direction2D.Left
          case 'v' => Direction2D.Down

      val n = acc.offset(dir)
      set += n
      n

    set.size

  def dirOf(c: Char): Direction2D =
    c match
      case '^' => Direction2D.Up
      case '<' => Direction2D.Left
      case '>' => Direction2D.Right
      case 'v' => Direction2D.Down

  def calc(set: mutable.Set[Vec2i], str: Seq[Char]): Unit =
    str.foldLeft(Vec2i(0, 0)): (acc, r) =>
      val dir = dirOf(r)

      val n = acc.offset(dir)
      set += n
      n

  def part2(input: String): Int =
    val set = mutable.Set[Vec2i](Vec2i(0, 0))
    val santa = input.zipWithIndex.filter(_._2 % 2 == 0).map(_._1)
    val roboSanta = input.zipWithIndex.filter(_._2 % 2 == 1).map(_._1)

    calc(set, santa)
    calc(set, roboSanta)

    set.size



  lazy val input: String = FileIO.getInput(2015, 3)
