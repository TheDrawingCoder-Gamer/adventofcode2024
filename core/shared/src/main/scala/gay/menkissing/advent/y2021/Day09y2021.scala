package gay.menkissing.advent
package y2021

import gay.menkissing.common.{Grid, Vec2i, given}
import cats.syntax.all.*

import scala.annotation.tailrec

object Day09y2021 extends Problem[Grid[Int], Int]:
  lazy val input = FileIO.getInput(2021, 9)
  def parse(input: String): Grid[Int] =
    Grid(input.linesIterator.map(_.map(_.asDigit)))

  def part1(input: Grid[Int]): Int =
    val lowPoints = input.zipWithIndices.filter((h, p) => p.cardinalNeighbors.forall(it => input.get(it).forall(_ > h)))
    lowPoints.foldLeft(0)((acc, r) => acc + r._1 + 1)

  @tailrec
  def followBasin(grid: Grid[Int], p: Vec2i, v: Int): (Int, Vec2i) =
    val filteredNs = p.cardinalNeighbors.flatMap(it => grid.get(it).map(x => (x, it)))
    val (_, lpos) = filteredNs.foldLeft((10, Vec2i(-1, -1))):
      case (tv@(v, _), ta@(a, _)) =>
        if v < a then tv else ta

    val myValue = grid(p)

    if v == 9 then
      (9, Vec2i(-1, -1))
    else if filteredNs.forall(_._1 > myValue) then
      (v, p)
    else
      followBasin(grid, lpos, v)

  def part2(input: Grid[Int]): Int =
    val basinPos = input.zipWithIndices.map((v, p) => followBasin(input, p, v)._2)

    val basinCounts = basinPos.filter(_ != Vec2i(-1, -1)).groupBy(identity).map(_._2.length).toSeq

    basinCounts.sorted(using Ordering[Int].reverse).take(3).product
