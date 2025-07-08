package gay.menkissing.advent
package y2020

import cats.*
import cats.data.State
import cats.implicits.*
import gay.menkissing.common.{*, given}

import scala.collection.mutable

object Day12y2020 extends Problem[List[(Day12y2020.Action, Int)], Int]:
  case class DaState(x: Int = 0, y: Int = 0, facing: Direction2D = Direction2D.Right)
  case class DaStateP2(ship: Vec2i = Vec2i(0, 0), waypointRelative: Vec2i = Vec2i(10, -1))

  extension(self: Vec2i)
    // specific to our coord system
    def rotate(by: Int): Vec2i =
      by rem 4 match {
        case 0 => self
        case 1 => Vec2i(-self.y, self.x)
        case 2 => Vec2i(-self.x, -self.y)
        case 3 => Vec2i(self.y, -self.x)
      }
  enum Action:
    case N,S,E,W,L,R,F

    def advanceP2(state: DaStateP2, value: Int): DaStateP2 = {
      this match {
        case Action.N => state.copy(waypointRelative = state.waypointRelative + Vec2i(0, -value))
        case Action.S => state.copy(waypointRelative = state.waypointRelative + Vec2i(0, value))
        case Action.E => state.copy(waypointRelative = state.waypointRelative + Vec2i(value, 0))
        case Action.W => state.copy(waypointRelative = state.waypointRelative + Vec2i(-value, 0))
        case Action.L => state.copy(waypointRelative = state.waypointRelative.rotate(-(value / 90)))
        case Action.R => state.copy(waypointRelative = state.waypointRelative.rotate(value / 90))
        case Action.F => state.copy(ship = state.ship + state.waypointRelative * value)
      }
    }

    def advance(state: DaState, value: Int): DaState =
      this match
        case N => state.copy(y = state.y - value)
        case S => state.copy(y = state.y + value)
        case E => state.copy(x = state.x + value)
        case W => state.copy(x = state.x - value)
        case L =>
          val by = value / 90
          state.copy(facing = state.facing.rotate(-by))
        case R =>
          val by = value / 90
          state.copy(facing = state.facing.rotate(by))
        case F =>

          val next = Vec2i(state.x, state.y) + (state.facing.digitalDir * value)
          state.copy(x = next.x, y = next.y)


  override def parse(str: String): List[(Action, Int)] =
    str.linesIterator.map: line =>
      val action = line.head match
        case 'N' => Action.N
        case 'S' => Action.S
        case 'E' => Action.E
        case 'W' => Action.W
        case 'L' => Action.L
        case 'R' => Action.R
        case 'F' => Action.F

      (action, line.tail.toInt)
    .toList




  override def part1(input: List[(Action, Int)]): Int = {
    val res = input.foldLeft(DaState()):
      case (s, (a, v)) =>
        a.advance(s, v)

    math.abs(res.x) + math.abs(res.y)
  }


  def part2(input: List[(Action, Int)]): Int =
    val res = input.foldLeft(DaStateP2()):
      case (s, (a,v)) =>
        a.advanceP2(s, v)

    res.ship `taxiDistance` Vec2i(0,0)



  override lazy val input: String = FileIO.getInput(2020, 12)

