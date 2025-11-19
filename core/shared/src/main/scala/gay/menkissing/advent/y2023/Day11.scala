package gay.menkissing.advent
package y2023

import gay.menkissing.common.*, algebras.given
import cats.*
import cats.syntax.all.*

object Day11 extends Problem:
  type Input = Grid[Boolean]
  type Output = Long
  def input = FileIO.getInput(2023, 11)

  def parse(str: String): Grid[Boolean] =
    Grid.fromString(str):
      case '#' => true
      case _   => false

  extension (self: Grid[Boolean])
    def expandUniverse: Grid[Boolean] =
      val rows =
        self.rows
          .flatMap(seq => if seq.forall(!_) then Seq(seq, seq) else Seq(seq))
      val cols =
        rows.transpose
          .flatMap(seq => if seq.forall(!_) then Seq(seq, seq) else Seq(seq))
      Grid(cols.transpose)

  def part1(input: Grid[Boolean]): Long =
    input.expandUniverse.zipWithIndices.filter(_._1).map(_._2).toSet.subsets(2)
      .map: s =>
        val l = s.head
        val r = s.tail.head
        // why was it written like "find the shortest path (there are no obstacles)" ???
        l.taxiDistance(r)
      .sum.toLong

  def part2(input: Grid[Boolean]): Long =
    val numGrid = input.map(it => if it then None else Some(1L))
    val cols =
      numGrid.values.map(seq =>
        if seq.forall(_.isDefined) then seq.as(Some(1_000_000L)) else seq
      ).transpose
    val rows =
      numGrid.values.transpose.map(seq =>
        if seq.forall(_.isDefined) then seq.as(Some(1_000_000L)) else seq
      ).transpose
    val points = numGrid.zipWithIndices.filter((v, _) => v.isEmpty).map(_._2)
    def calculateNewPoint(p: Vec2[Int]): Vec2[Long] =
      val newX = rows(p.y).take(p.x).map(_.getOrElse(1L)).sum
      val newY = cols(p.x).take(p.y).map(_.getOrElse(1L)).sum
      Vec2(newX, newY)
    val newPoints = points.map(calculateNewPoint)
    newPoints.toSet.subsets(2).map: s =>
      val l = s.head
      val r = s.tail.head
      l.taxiDistance(r)
    .sum
