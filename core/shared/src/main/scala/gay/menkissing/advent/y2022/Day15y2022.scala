package gay.menkissing.advent
package y2022

import gay.menkissing.common.*

import scala.collection.mutable as mut
import cats.syntax.all.*
import cats.data.Chain

// TODO: Unacceptably slow
object Day15y2022 extends Problem[Set[Day15y2022.SensorRanged], Long]:
  case class SensorRanged(pos: Vec2i, beacon: Vec2i) {
    val range = pos `taxiDistance` beacon
    lazy val leftBound = pos.x - range
    lazy val rightBound = pos.x + range
    lazy val bottomBound = pos.y + range
    lazy val topBound = pos.y - range
    def rangeForRow(row: Int): Option[Range] = {
      val diff = math.abs(pos.y - row)
      val newRange = range - diff
      if (newRange <= 0)
        None
      else
        Some(Range(pos.x - newRange, pos.x + newRange))
    }

  }
  def emptySpots(target: Range, cover: List[Range]): Set[Int] =
    cover.foldLeft(List(target)):
      case (acc, range) =>
        acc.flatMap(r => diffRange(r, range))
    .flatten.toSet

  class RangeGrid(values: Set[SensorRanged]) {
     def getRow(row: Int): List[Range] = {
       values.foldLeft(Chain.empty[Range]):
         case (acc, ls) =>
           acc ++ Chain.fromOption(ls.rangeForRow(row))
       .toList
     }
     def beaconsOnLine(row: Int): Set[Int] =
       values.withFilter(_.beacon.y == row).map(_.beacon.x)

     def findValidPos(max: Int): Seq[Vec2i] = {
       (0 to max).flatMap: y =>
         val r = emptySpots(0 to max, getRow(y))
         if r.nonEmpty then
           val b = r.diff(beaconsOnLine(y))
           Option.when(b.nonEmpty):
             // This may be incorrect because my solution has 2 available slots
             //println(b)
             //assert(b.size == 1)
             Vec2i(b.toList.head, y)
         else
           None
     }
  }

  lazy val input = FileIO.getInput(2022, 15)

  def diffRange(r1: Range, r2: Range): List[Range] =
    val init = r1.start to math.min(r2.start - 1, r1.last)
    val tail = math.max(r1.start, r2.last + 1) to r1.last
    val res = if init == tail then
      List(init)
    else
      List(init, tail)
    res.filter(_.nonEmpty)


  def parse(input: String): Set[SensorRanged] =
    input.linesIterator.map {
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" => SensorRanged(Vec2i(sx.toInt, sy
        .toInt), Vec2i(bx.toInt, by.toInt))
      case _ => assert(false)
    }.toSet

  val max = 4000000

  def part1(input: Set[SensorRanged]): Long =
    input.foldLeft(0L):
      case (acc, item) =>
        item.rangeForRow(2000000) match
          case Some(range) =>  acc + (range.end - range.start).toLong
          case _           =>  acc

  def part2(input: Set[SensorRanged]): Long =
    val grid = RangeGrid(input)
    val spots = grid.findValidPos(max)
    assert(spots.length == 1)
    val Vec2i(x, y) = spots.head
    (x.toLong * 4000000) + y.toLong


