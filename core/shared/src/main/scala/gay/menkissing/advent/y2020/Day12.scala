package gay.menkissing.advent
package y2020

import cats.*
import cats.data.State
import cats.implicits.*
import gay.menkissing.common.{*, given}
import spire.implicits.IntAlgebra

import scala.collection.mutable

object Day12 extends Problem[List[(Char, Int)], Int]:
  case class DaState(x: Int = 0, y: Int = 0, facing: Direction2D = Direction2D.Right):
    def advance(action: Char, value: Int): DaState =
      action match
        case 'N' => copy(y = y - value)
        case 'S' => copy(y = y + value)
        case 'E' => copy(x = x + value)
        case 'W' => copy(x = x - value)
        case 'L' =>
          val by = value / 90
          copy(facing = facing.rotate(-by))
        case 'R' =>
          val by = value / 90
          copy(facing = facing.rotate(by))
        case 'F' =>

          val next = Vec2(x, y) + (facing.digitalDir * value)
          copy(x = next.x, y = next.y)
        case _ => this
  case class DaStateP2(ship: Vec2[Int] = Vec2(0, 0), waypointRelative: Vec2[Int] = Vec2(10, -1)):
    def advance(action: Char, value: Int): DaStateP2 =
      action match
        case 'N' => copy(waypointRelative = waypointRelative + Vec2(0, -value))
        case 'S' => copy(waypointRelative = waypointRelative + Vec2(0, value))
        case 'E' => copy(waypointRelative = waypointRelative + Vec2(value, 0))
        case 'W' => copy(waypointRelative = waypointRelative + Vec2(-value, 0))
        case 'L' => copy(waypointRelative = waypointRelative.rotate(-(value / 90)))
        case 'R' => copy(waypointRelative = waypointRelative.rotate(value / 90))
        case 'F' => copy(ship = ship + waypointRelative * value)

  extension(self: Vec2[Int])
    // specific to our coord system
    def rotate(by: Int): Vec2[Int] =
      by rem 4 match
        case 0 => self
        case 1 => Vec2(-self.y, self.x)
        case 2 => Vec2(-self.x, -self.y)
        case 3 => Vec2(self.y, -self.x)


  override def parse(str: String): List[(Char, Int)] =
    str.linesIterator.map: line =>
      (line.head, line.tail.toInt)
    .toList




  override def part1(input: List[(Char, Int)]): Int =
    val res = input.foldLeft(DaState()):
      case (s, (a, v)) =>
        s.advance(a,v)

    math.abs(res.x) + math.abs(res.y)


  def part2(input: List[(Char, Int)]): Int =
    val res = input.foldLeft(DaStateP2()):
      case (s, (a,v)) =>
        s.advance(a,v)

    res.ship `taxiDistance` Vec2(0,0)



  override lazy val input: String = FileIO.getInput(2020, 12)

