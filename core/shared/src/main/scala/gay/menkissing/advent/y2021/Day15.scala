package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import algebras.given
import cats.implicits.*

object Day15 extends Problem:
  type Input = Grid[Int]
  type Output = Int

  def input = FileIO.getInput(2021, 15)

  def parse(str: String): Input = Grid.fromString(str)(_.asDigit)

  def search(grid: Grid[Int]): Int =
    val goal = Vec2(grid.width - 1, grid.height - 1)
    astarScore(
      Vec2(0, 0),
      Vec2(grid.width - 1, grid.height - 1),
      _ taxiDistance goal,
      (_, r) => grid.get(r).map(_.toDouble).getOrElse(Double.PositiveInfinity),
      _.cardinalNeighbors
    ).get.toInt
  def part1(input: Grid[Int]): OutputP1 = search(input)

  def incrRisk(r: Int): Int =
    val x = (r + 1) % 9
    if x == 0 then 9 else x

  def part2(input: Grid[Int]): Int =
    val firstRow =
      List.iterate(input, 5)(_.map(incrRisk)).reduce(_.combineHorizontal(_))
    val newGrid =
      List.iterate(firstRow, 5)(_.map(incrRisk)).reduce(_.combineVertical(_))
    search(newGrid)
