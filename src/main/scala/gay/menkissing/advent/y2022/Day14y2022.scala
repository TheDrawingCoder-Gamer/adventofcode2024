package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import cats.*
import cats.implicits.*

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable as mut

object Day14y2022 extends Problem[Day14y2022.SparseCaveGrid, Int]:
  extension (a: Int) {
    infix def ascendsTo(b: Int): Range = {
      val s = a `min` b
      val e = b `max` a

      s to e
    }
  }
  case class Pathway(points: List[Vec2i]) {
    def bake: Set[Vec2i] =
      points.sliding(2).flatMap { case List(s, e) =>
        require(s.x == e.x || s.y == e.y)
        for {
          y <- s.y ascendsTo e.y
          x <- s.x ascendsTo e.x
        } yield Vec2i(x, y)
      }.toSet
  }

  val source = Vec2i(500, 0)


  enum CavePoint {
    case Rock, Sand, Air
  }

  case class SparseCaveGrid(points: Map[Vec2i, CavePoint], source: Vec2i) {
    val lowestPoint: Vec2i = points.maxBy( (p, c) => if (c == CavePoint.Rock) p.y else 0)._1

    val floor = lowestPoint.y + 2
    def apply(p: Vec2i) =
      points.getOrElse(p, if (p.y >= floor) CavePoint.Rock else CavePoint.Air)

    def getP1(p: Vec2i) =
      points.getOrElse(p, CavePoint.Air)

    
    @tailrec
    final def genericMove(p: Vec2i, get: Vec2i => CavePoint, rejectWhen: Vec2i => Boolean): Option[Vec2i] =
      if p.y > floor || rejectWhen(p) then
        None
      else
        val v = get(p)
        if v != CavePoint.Air then
          None
        else
          // y is flipped
          val belowPos = p.copy(y = p.y + 1)
          val berightPos = belowPos.copy(x = p.x + 1)
          val beleftPos = belowPos.copy(x = p.x - 1)

          val below = apply(belowPos)
          lazy val belowRight = apply(berightPos)
          lazy val belowLeft = apply(beleftPos)
          if (below == CavePoint.Air)
            genericMove(belowPos, get, rejectWhen)
          else if (belowLeft == CavePoint.Air)
            genericMove(beleftPos, get, rejectWhen)
          else if (belowRight == CavePoint.Air)
            genericMove(berightPos, get, rejectWhen)
          else
            Some(p)

    def moveSandP1(p: Vec2i) = genericMove(p, getP1, p => p.y > lowestPoint.y)
    def withSandP1: Option[SparseCaveGrid] =
      moveSandP1(source).map: it =>
        SparseCaveGrid(points.updated(it, CavePoint.Sand), source)


    def moveSand(p: Vec2i) = genericMove(p, apply, _ => false)
    def withSand: Option[SparseCaveGrid] =
      moveSand(source).map { it =>
        SparseCaveGrid(points.updated(it, CavePoint.Sand), source)
      }
    def show: String = {
      val rightBound = points.maxBy( (p, _) => p.x)._1.x
      val leftBound = points.minBy( (p, _) => p.x)._1.x
      val topBound = 0
      val bottomBound = floor
      (for {
        y <- topBound to bottomBound
      } yield {
        (for {
          x <- leftBound to rightBound
        } yield
          apply(Vec2i(x, y)) match
            case CavePoint.Rock => '#'
            case CavePoint.Sand => 'o'
            case CavePoint.Air => '.'
        ).mkString
      }).mkString("", "\n", "")
    }
  }
  object SparseCaveGrid {
    def apply(rock: Set[Vec2i], source: Vec2i): SparseCaveGrid = {
      SparseCaveGrid(Map.from(rock.zip(Vector.fill(rock.size)(CavePoint.Rock))), source)
    }
  }
  def parse(input: String): SparseCaveGrid = {
    val data = input.linesIterator.map { it =>
      Pathway(it.split("->").map(_.trim).map {
        case s"$l,$r" => Vec2i(l.toInt, r.toInt)
      }.toList)
    }.toVector
    val goodData = data.flatMap(_.bake).toSet
    SparseCaveGrid(goodData, source)
  }

  lazy val input = FileIO.getInput(2022, 14)



  def part1(input: SparseCaveGrid): Int =
    Iterator.iterate(Option(input))(_.flatMap(_.withSandP1)).takeWhile(_.isDefined).size - 1

  def part2(input: SparseCaveGrid): Int =
    Iterator.iterate(Option(input))(_.flatMap(_.withSand)).takeWhile(_.isDefined).size - 1


