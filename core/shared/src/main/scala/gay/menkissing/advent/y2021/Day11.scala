package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import cats.syntax.all.*
import spire.implicits.IntAlgebra

import scala.annotation.tailrec
import cats.Show

object Day11 extends Problem:
  type Input = Grid[Int]
  type Output = Long
  def showOutput: Show[Long] = summon

  lazy val input = FileIO.getInput(2021, 11)

  def parse(input: String): Grid[Int] = Grid.fromString(input)(_.asDigit)

  def flashElem(grid: Grid[Int], p: Vec2[Int], v: Int): Int =
    if v == -1 then -1
    else if v >= 10 then -1
    else
      val freakCount = p.allNeighbors.flatMap(grid.get).count(_ >= 10)
      v + freakCount

  def flashElems(grid: Grid[Int]): Grid[Int] =
    grid.mapWithIndex((l, r) => flashElem(grid, l, r))

  @tailrec
  def flash(grid: Grid[Int]): Grid[Int] =
    val flashed = flashElems(grid)
    if flashed.exists(_ >= 10) then flash(flashed)
    else flashed

  def stepSum(octopi: Grid[Int], nflash: Long): (Grid[Int], Long) =
    val octopi2 = octopi.map(_ + 1)
    val flashed = flash(octopi2)
    val nflash2 = flashed.count(_ == -1)
    val reset =
      flashed.map:
        case -1 => 0
        case v  => v
    (reset, nflash + nflash2)

  def simStream(octopi: Grid[Int]): Iterator[(Grid[Int], Long)] =
    Iterator.iterate((octopi, 0L))(stepSum)

  def part1(grid: Grid[Int]): Long = simStream(grid).drop(100).next()._2

  def part2(grid: Grid[Int]): Long =
    simStream(grid).indexWhere(_._1.forall(_ == 0))
