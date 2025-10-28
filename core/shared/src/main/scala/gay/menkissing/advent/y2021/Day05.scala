package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import cats.syntax.all.*

import scala.collection.mutable as mut

object Day05 extends Problem[List[Day05.Line], Int]:
  lazy val input = FileIO.getInput(2021, 5)


  case class Line(start: Vec2i, end: Vec2i):
    def orientation: LineOrientation =
      if start == end then
        LineOrientation.Point
      else if start.x == end.x then
        LineOrientation.Vertical
      else if start.y == end.y then
        LineOrientation.Horizontal
      else
        LineOrientation.Diagonal

    def covers: Set[Vec2i] =
      orientation match
        case LineOrientation.Point => Set(this.start)
        case LineOrientation.Horizontal => (math.min(start.x, end.x) to math.max(start.x, end.x)).map(x => Vec2i(x, start.y)).toSet
        case LineOrientation.Vertical => (math.min(start.y, end.y) to math.max(start.y, end.y))
          .map(y => Vec2i(start.x, y)).toSet
        case LineOrientation.Diagonal =>
          ((start.x to end.x by math.signum(end.x - start.x)).toList, 
            (start.y to end.y by math.signum(end.y - start.y)).toList).parMapN(Vec2i.apply)
          .toSet
  enum LineOrientation:
    case Horizontal, Vertical, Diagonal, Point

  def parse(input: String): List[Line] =
    input.linesIterator.map:
      case s"$x1,$y1 -> $x2,$y2" => Line(Vec2i(x1.toInt, y1.toInt), Vec2i(x2.toInt, y2.toInt))
    .toList

  extension[A, B] (map: Map[A, B])
    def unionWith(that: Map[A, B])(f: (B, B) => B): Map[A, B] =
      map.alignMergeWith(that)(f)

  extension (map: mut.HashMap[Vec2i, Int])
    def updateInPlaceField(line: Line): Unit =
      val daSet = line.covers
      daSet.foreach: p =>
        map.updateWith(p):
          case Some(v) => Some(v + 1)
          case None => Some(1)

    def countDanger: Int = map.count((_, i) => i >= 2)

  def part1(input: List[Line]): Int =
    val daMap = mut.HashMap.empty[Vec2i, Int]
    input.filter(_.orientation != LineOrientation.Diagonal).foreach(daMap.updateInPlaceField)

    daMap.countDanger

  def part2(input: List[Line]): Int =
    val daMap = mut.HashMap.empty[Vec2i, Int]
    input.foreach(daMap.updateInPlaceField)

    daMap.countDanger

