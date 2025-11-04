package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import algebras.given
import cats.implicits.*

object Day13 extends ProblemAdv:
  type Input = (Set[Vec2[Int]], List[Fold])
  type OutputP1 = Int
  type OutputP2 = String

  lazy val input = FileIO.getInput(2021, 13)

  def parse(str: String): (Set[Vec2[Int]], List[Fold]) =
    val Array(dots, folds) = str.trim.split("\n\n").toArray: @unchecked
    val goodDots =
      dots.linesIterator.map:
        case s"$x,$y" => Vec2(x.toInt, y.toInt)
        case _        => whatTheScallop.!
      .toSet
    val goodFolds =
      folds.linesIterator.map:
        case s"fold along $axis=$n" =>
          val a =
            axis match
              case "x" => Axis2D.X
              case "y" => Axis2D.Y
          Fold(a, n.toInt)
      .toList
    (goodDots, goodFolds)

  case class Fold(axis: Axis2D, at: Int):
    def apply(pos: Vec2[Int]): Vec2[Int] =
      axis match
        case Axis2D.X => pos.copy(x = fold(at)(pos.x))
        case Axis2D.Y => pos.copy(y = fold(at)(pos.y))
    def fold(along: Int)(value: Int): Int =
      if value < along then value else along - (value - along)

  def foldThingie(dots: Set[Vec2[Int]], axis: Axis2D, at: Int): Set[Vec2[Int]] =
    val (side1, side2) = dots.partition(_.coord(axis.ordinal) < at)
    // it doesnt seem to matter which way we fold (left vs right, who gives a scallop)
    // so for convienience, pick the side that has less length
    val maxN = dots.maxBy(_.coord(axis.ordinal)).coord(axis.ordinal) - at
    if maxN < at then
      // fold side2 onto side1
      val foldedSide2 =
        axis match
          case Axis2D.X => side2.map(it => it.copy(x = at - (it.x - at)))
          case Axis2D.Y => side2.map(it => it.copy(y = at - (it.y - at)))
      side1 ++ foldedSide2
    else
      // fold side1 onto side2, and subtract at so that we stay in the same coord system
      val foldedSide1 =
        axis match
          case Axis2D.X => side1.map(it => it.copy(x = at - it.x))
          case Axis2D.Y => side1.map(it => it.copy(y = at - it.y))
      val rootN = VecN.axis[Vec2][Int](axis.ordinal) * at
      foldedSide1 ++ side2.map(_ - rootN)

  def part1(input: (Set[Vec2[Int]], List[Fold])): Int =
    val (dots, folds) = input
    dots.map(folds.head.apply).size

  def part2(input: (Set[Vec2[Int]], List[Fold])): String =
    val (dots, folds) = input
    val c =
      folds.foldLeft(dots): (acc, fold) =>
        acc.map(fold.apply)

    val minX = c.minBy(_.x).x
    val minY = c.minBy(_.y).y
    val min = Vec2(minX, minY)
    val cropped = c.map(_ - min).map(_ -> '#').toMap

    Grid.fromSparse(
      cropped.keySet.maxBy(_.x).x + 1,
      cropped.keySet.maxBy(_.y).y + 1,
      cropped
    )(
      '.'
    ).values.map(_.mkString("")).mkString("\n")
