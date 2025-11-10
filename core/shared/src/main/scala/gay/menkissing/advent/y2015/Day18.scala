package gay.menkissing.advent
package y2015

import gay.menkissing.common.*
import cats.syntax.all.*
import algebra.instances.all.*

object Day18 extends Problem:
  type Input = Grid[Boolean]
  type Output = Int
  lazy val input: String = FileIO.getInput(2015, 18)

  def parse(str: String): Grid[Boolean] =
    Grid.fromString(str):
      case '.' => false
      case '#' => true

  def setCorners(grid: Grid[Boolean]): Grid[Boolean] =
    grid.updated(0, 0)(true).updated(grid.width - 1, 0)(true)
      .updated(0, grid.height - 1)(true)
      .updated(grid.width - 1, grid.height - 1)(true)

  def step(grid: Grid[Boolean]): Grid[Boolean] =
    grid.mapWithIndex: (p, v) =>
      val c = p.allNeighbors.count(it => grid.getOrElse(it, false))
      if v then c == 2 || c == 3
      else c == 3

  def part1(input: Grid[Boolean]): Int =
    step.repeated(100)(input).count(identity).toInt

  def part2(input: Grid[Boolean]): Int =
    setCorners(setCorners.andThen(step).repeated(100)(input)).count(identity)
      .toInt
