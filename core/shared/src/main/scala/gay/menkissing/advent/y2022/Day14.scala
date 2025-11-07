package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import ArityN.*
import algebra.instances.all.*

import scala.annotation.tailrec
import scala.collection.mutable as mut
import scala.io.Source
import cats.Show

object Day14 extends Problem:
  type Input = SparseCaveGrid
  type Output = Int

  extension (a: Int)
    infix def ascendsTo(b: Int): Range =
      val s = a `min` b
      val e = b `max` a

      s to e
  case class Pathway(points: List[Vec2[Int]]):
    def bake: Set[Vec2[Int]] =
      points.slidingN[2].flatMap:
        case (s, e) =>
          require(s.x == e.x || s.y == e.y)
          for
            y <- s.y ascendsTo e.y
            x <- s.x ascendsTo e.x
          yield Vec2(x, y)
      .toSet

  val source = Vec2(500, 0)

  enum CavePoint:
    case Rock, Sand, Air

  final case class SparseCaveGrid
    (
      points: Map[Vec2[Int], CavePoint],
      source: Vec2[Int]
    ):
    val lowestPoint: Vec2[Int] =
      points.maxBy((p, c) => if c == CavePoint.Rock then p.y else 0)._1

    val floor = lowestPoint.y + 2
    def apply(p: Vec2[Int]) =
      points
        .getOrElse(p, if p.y >= floor then CavePoint.Rock else CavePoint.Air)

    def getP1(p: Vec2[Int]) = points.getOrElse(p, CavePoint.Air)

    @tailrec
    def genericMove
      (
        p: Vec2[Int],
        get: Vec2[Int] => CavePoint,
        rejectWhen: Vec2[Int] => Boolean
      ): Option[Vec2[Int]] =
      if p.y > floor || rejectWhen(p) then None
      else
        val v = get(p)
        if v != CavePoint.Air then None
        else
          // y is flipped
          val belowPos = p.copy(y = p.y + 1)
          val berightPos = belowPos.copy(x = p.x + 1)
          val beleftPos = belowPos.copy(x = p.x - 1)

          val below = apply(belowPos)
          lazy val belowRight = apply(berightPos)
          lazy val belowLeft = apply(beleftPos)
          if below == CavePoint.Air then genericMove(belowPos, get, rejectWhen)
          else if belowLeft == CavePoint.Air then
            genericMove(beleftPos, get, rejectWhen)
          else if belowRight == CavePoint.Air then
            genericMove(berightPos, get, rejectWhen)
          else Some(p)

    def moveSandP1(p: Vec2[Int]) =
      genericMove(p, getP1, p => p.y > lowestPoint.y)
    def withSandP1: Option[SparseCaveGrid] =
      moveSandP1(source).map: it =>
        SparseCaveGrid(points.updated(it, CavePoint.Sand), source)

    def moveSand(p: Vec2[Int]) = genericMove(p, apply, _ => false)
    def withSand: Option[SparseCaveGrid] =
      moveSand(source).map: it =>
        SparseCaveGrid(points.updated(it, CavePoint.Sand), source)
    def show: String =
      val rightBound = points.maxBy((p, _) => p.x)._1.x
      val leftBound = points.minBy((p, _) => p.x)._1.x
      val topBound = 0
      val bottomBound = floor
      (topBound to bottomBound).map: y =>
        (leftBound to rightBound).map: x =>
          apply(Vec2(x, y)) match
            case CavePoint.Rock => '#'
            case CavePoint.Sand => 'o'
            case CavePoint.Air  => '.'
        .mkString
      .mkString("\n")

  object SparseCaveGrid:
    def apply(rock: Set[Vec2[Int]], source: Vec2[Int]): SparseCaveGrid =
      SparseCaveGrid(
        Map.from(rock.zip(Vector.fill(rock.size)(CavePoint.Rock))),
        source
      )

  def parse(input: String): SparseCaveGrid =
    val data =
      input.linesIterator.map: it =>
        Pathway:
          it.split("->").map(_.trim).map:
            case s"$l,$r" => Vec2(l.toInt, r.toInt)
          .toList
      .toVector
    val goodData = data.flatMap(_.bake).toSet
    SparseCaveGrid(goodData, source)

  lazy val input = FileIO.getInput(2022, 14)

  def part1(input: SparseCaveGrid): Int =
    LazyList.iterate(Option(input))(_.flatMap(_.withSandP1))
      .countWhile(_.isDefined).toInt - 1

  def part2(input: SparseCaveGrid): Int =
    LazyList.iterate(Option(input))(_.flatMap(_.withSand))
      .countWhile(_.isDefined).toInt - 1
