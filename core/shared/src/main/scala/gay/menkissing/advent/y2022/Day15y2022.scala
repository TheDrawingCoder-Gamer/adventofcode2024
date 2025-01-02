package gay.menkissing.advent
package y2022

import gay.menkissing.common.Vec2i

import scala.collection.mutable as mut
import cats.collections.*
import cats.syntax.all.*

// TODO: Unacceptably slow
object Day15y2022 extends Problem[Day15y2022.RangeGrid, Long]:
  case class SensorRanged(pos: Vec2i, beacon: Vec2i) {
    val range = pos `taxiDistance` beacon
    lazy val leftBound = pos.x - range
    lazy val rightBound = pos.x + range
    lazy val bottomBound = pos.y + range
    lazy val topBound = pos.y - range
    def ranges: Map[Int, Diet[Int]] =
      (for {
        row <- topBound to bottomBound
      } yield rangeForRow(row).map(it => (row, it)))
        .flatten
        .map((k, v) => (k, Diet.fromRange(v)))
        .toMap
    def rangeForRow(row: Int): Option[Range[Int]] = {
      val diff = Math.abs(pos.y - row)
      val newRange = range - diff
      if (newRange <= 0)
        None
      else
        Some(Range(pos.x - newRange, pos.x + newRange))
    }
    def affectsRow(row: Int): Boolean =
      rangeForRow(row).isDefined
    def withinRange(that: Vec2i): Boolean =
      (pos `taxiDistance` that) <= range
    def positionsInRow(row: Int): Set[Vec2i] =
      (for {
        xx <- rangeForRow(row).map(_.toList)
      } yield xx.map(it => Vec2i(it, row))).getOrElse(List()).toSet.filter(it => this.withinRange(it) && it != beacon)
  }
  class RangeGrid(values: Set[SensorRanged]) {
     val rows = values.view.map(_.ranges).fold(Map())(_ |+| _).filter((k, v) => k >= 0)
     def getRow(row: Int): Diet[Int] = {
       rows.getOrElse(row, Diet.empty)
     }
     def findValidPos(max: Int): Option[Vec2i] = {
       val (row, daRow) = rows.find((k, v) => k >= 0 && k <= max && !v.containsRange(Range(0, max))).get
       (0 to max).find(it => !daRow.contains(it)).map(it => Vec2i(it, row))
     }
  }

  lazy val input = FileIO.getInput(2022, 15)

  def parse(input: String): RangeGrid =
    val data  =
      input.linesIterator.map {
        case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" => SensorRanged(Vec2i(sx.toInt, sy
          .toInt), Vec2i(bx.toInt, by.toInt))
        case _ => assert(false)
      }.toSet
    RangeGrid(data)

  val max = 4000000

  def part1(input: RangeGrid): Long =
    input.getRow(2000000).foldLeftRange(0L):
      case (acc, range) => acc + (range.end - range.start).toLong

  def part2(input: RangeGrid): Long =
    val Vec2i(x, y) = input.findValidPos(max).get
    (x.toLong * 4000000) + y.toLong


