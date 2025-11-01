package gay.menkissing.advent
package y2024

import gay.menkissing.common.*
import cats.Show

object Day04 extends Problem:
  type Input = Grid[Char]
  type Output = Int
  def showOutput: Show[Int] = summon

  lazy val input = FileIO.getInput(2024, 4)

  override def parse(str: String): Grid[Char] =
    Grid[Char](str.linesIterator.map(_.toCharArray))

  def searchDir
    (
      grid: Grid[Char],
      x: Int,
      y: Int,
      dir: PrincibleWind2D
    ): Boolean =
    val digitalDir = dir.digitalDir
    val fullX = x + digitalDir.x * 3
    val fullY = y + digitalDir.y * 3
    if fullX < 0 || fullX >= grid.width || fullY < 0 || fullY >= grid.height
    then false
    else
      val c1 = grid(x, y)
      val c2 = grid(x + digitalDir.x, y + digitalDir.y)
      val c3 = grid(x + digitalDir.x * 2, y + digitalDir.y * 2)
      val c4 = grid(x + digitalDir.x * 3, y + digitalDir.y * 3)
      val str = String.valueOf(Array(c1, c2, c3, c4))
      // Don't test reverse to prevent double counting
      str == "XMAS"

  private def matchesMS(l: Char, r: Char): Boolean =
    l == 'M' && r == 'S' || l == 'S' && r == 'M'

  def xmasSearch(grid: Grid[Char], x: Int, y: Int): Boolean =
    if x <= 0 || x >= grid.width - 1 || y <= 0 || y >= grid.height - 1 then
      // Don't test edges
      false
    else
      grid(x, y) == 'A' && matchesMS(grid(x - 1, y - 1), grid(x + 1, y + 1)) &&
      matchesMS(grid(x + 1, y - 1), grid(x - 1, y + 1))

  override def part1(grid: Grid[Char]): Int =
    grid.indices.map: (x, y) =>
      PrincibleWind2D.values.count(searchDir(grid, x, y, _))
    .sum

  override def part2(grid: Grid[Char]): Int =
    grid.indices.count: (x, y) =>
      xmasSearch(grid, x, y)
