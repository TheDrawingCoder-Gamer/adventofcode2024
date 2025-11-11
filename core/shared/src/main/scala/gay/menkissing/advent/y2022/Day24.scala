package gay.menkissing.advent
package y2022

import gay.menkissing.common.*, algebras.given

import scala.collection.mutable
import cats.*
import cats.derived.*
import cats.syntax.all.*

/**
 * This class is NOT thread safe!
 */
object Day24 extends Problem:
  type Input = BlizzardMap
  type Output = Int

  lazy val input = FileIO.getInput(2022, 24)

  sealed trait BlizzardMapPoint
  final case class BlizzardSpot(blizzards: List[Direction2D])
      extends BlizzardMapPoint
  object BlizzardSpot:
    def one(dir: Direction2D): BlizzardSpot = BlizzardSpot(List(dir))

  case object Wall extends BlizzardMapPoint
  case object Empty extends BlizzardMapPoint

  final case class TimeLocation(time: Int, loc: Vec2[Int]) derives Eq

  type BlizzardMap = Grid[BlizzardMapPoint]

  final class State:
    val memo = mutable.HashMap[Int, BlizzardMap]()

    def freakstar(start: TimeLocation, goal: Vec2[Int]): Option[TimeLocation] =
      astarByReturning(
        start,
        _.loc == goal,
        _.loc.taxiDistance(goal),
        (_, _) => 1d,
        it => neighbors(memo(it.time), it),
        (_, cur, _) => cur
      )

    def daAStar(start: Vec2[Int], goal: Vec2[Int]) =
      freakstar(TimeLocation(0, start), goal)
    def nextMap(blizzards: BlizzardMap, time: Int): BlizzardMap =
      if memo.contains(time) then return memo(time)
      val daThingie =
        blizzards.zipWithIndices.map: (spot, v) =>
          spot match
            case BlizzardSpot(blizzards) => Some(blizzards.map(it => (v, it)))
            case _                       => None
        .flatten.flatten
      val newPoses =
        daThingie.map: (pos, dir) =>
          val newPos = pos.offset(dir)
          val goodPos =
            if blizzards.getOrElse(newPos, Wall) == Wall then
              dir match
                case Direction2D.Up =>
                  newPos.copy(y =
                    blizzards.extractColumn(pos.x).lastIndexWhere(_ != Wall)
                  )
                case Direction2D.Down =>
                  newPos.copy(y =
                    blizzards.extractColumn(pos.x).indexWhere(_ != Wall)
                  )
                case Direction2D.Left =>
                  newPos.copy(x =
                    blizzards.extractRow(pos.y).lastIndexWhere(_ != Wall)
                  )
                case Direction2D.Right =>
                  newPos
                    .copy(x = blizzards.extractRow(pos.y).indexWhere(_ != Wall))
            else newPos
          (goodPos, dir)
      val daMap = newPoses.groupMap(_._1)(_._2).mapValues(_.toList)
      var newGrid =
        blizzards.map:
          case BlizzardSpot(_) => Empty
          case self            => self
      daMap.foreach: (k, v) =>
        val cur = newGrid(k)
        val newSpot =
          cur match
            case BlizzardSpot(blizzies) => BlizzardSpot(blizzies ++ v)
            case Wall                   => ???
            case Empty                  => BlizzardSpot(v)

        newGrid = newGrid.updated(k)(newSpot)

      memo.put(time, newGrid)
      newGrid

    def neighbors(blizzards: BlizzardMap, pos: TimeLocation) =
      val newMap = nextMap(blizzards, pos.time + 1)
      val daDirs =
        pos.loc.cardinalNeighbors.prepended(pos.loc).flatMap: p =>
          newMap.get(p).flatMap:
            case Empty => Some(TimeLocation(time = pos.time + 1, p))
            case _     => None
      daDirs

  def parse(input: String): BlizzardMap =
    Grid.fromString(input):
      case '.' => Empty
      case '#' => Wall
      case '^' => BlizzardSpot.one(Direction2D.Up)
      case 'v' => BlizzardSpot.one(Direction2D.Down)
      case '<' => BlizzardSpot.one(Direction2D.Left)
      case '>' => BlizzardSpot.one(Direction2D.Right)

  def part1(data: BlizzardMap): Int =
    val state = new State
    state.memo.put(0, data)
    val start = Vec2(data.rows.head.indexOf(Empty), 0)
    val end = Vec2(data.rows.last.indexOf(Empty), data.height - 1)
    state.daAStar(start, end).get.time

  def part2(data: BlizzardMap): Int =
    val state = new State
    state.memo.put(0, data)
    val start = Vec2(data.rows.head.indexOf(Empty), 0)
    val end = Vec2(data.rows.last.indexOf(Empty), data.height - 1)
    val firstEnd = state.daAStar(start, end).get
    val secondEnd = state.freakstar(firstEnd, start).get
    val thirdEnd = state.freakstar(secondEnd, end).get
    thirdEnd.time
