package gay.menkissing.advent
package y2022

import gay.menkissing.common.*

import scala.collection.mutable as mut
import cats.collections.{Diet, Range}
import cats.collections.syntax.range.*
import cats.syntax.all.*
import cats.data.Chain
import spire.implicits.IntAlgebra

// TODO: Unacceptably slow on JS
object Day15 extends Problem[Set[Day15.SensorRanged], Long]:
  case class SensorRanged(pos: Vec2[Int], beacon: Vec2[Int]):
    val range = pos `taxiDistance` beacon

    def rangeForRow(row: Int): Option[Range[Int]] =
      val diff = math.abs(pos.y - row)
      val newRange = range - diff
      Option.unless(newRange <= 0)(Range(pos.x - newRange, pos.x + newRange))

  class RangeGrid(values: Set[SensorRanged]):
    def getRow(row: Int): Diet[Int] =
      values.foldLeft(Diet.empty[Int]):
        case (acc, ls) =>
          ls.rangeForRow(row) match
            case Some(v) => acc + v
            case _ => acc

    private def trivialEmptySpotsDiet(row: Int): Diet[Int] =
      var diet = Diet.fromRange(Range(0, max))
      values.foreach: sensor =>
        sensor.rangeForRow(row).foreach: rng =>
          diet -= rng
      diet


    def beaconsOnLine(row: Int): Set[Int] =
      values.withFilter(_.beacon.y == row).map(_.beacon.x)

    def findValidPos(max: Int): Option[Vec2[Int]] =
      (0 to max).findMap: y =>
        // by never calling `getRow`, i save like 16s
        // because that is VERY expensive
        var diet = trivialEmptySpotsDiet(y)
        if !diet.isEmpty then
          beaconsOnLine(y).foreach: beacon =>
            diet -= beacon
          diet.toList.headOption.map: x =>
            Vec2(x, y)

        else
          None


  lazy val input = FileIO.getInput(2022, 15)


  def parse(input: String): Set[SensorRanged] =
    input.linesIterator.map:
      case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" => SensorRanged(Vec2(sx.toInt, sy
        .toInt), Vec2(bx.toInt, by.toInt))
      case _ => whatTheScallop.!
    .toSet

  val max = 4000000

  def dietSize(diet: Diet[Int]): Int =
    diet.toIterator.map(it => it.end - it.start).sum
  def part1(input: Set[SensorRanged]): Long =
    val grid = RangeGrid(input)
    dietSize(grid.getRow(2_000_000)).toLong

  def part2(input: Set[SensorRanged]): Long =
    val grid = RangeGrid(input)
    val spots = grid.findValidPos(max)
    val Vec2(x, y) = spots.get
    (x.toLong * 4000000) + y.toLong


