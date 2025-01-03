package gay.menkissing.advent

import gay.menkissing.advent.Problem
import gay.menkissing.common.*

import scala.io.Source


object Day4 extends Problem[Grid[Char], Int]:
  lazy val input = FileIO.getInput(2024, 4)

  override def parse(str: String): Grid[Char] =
    Grid[Char](str.linesIterator.map(_.toCharArray))
    
  

  def searchDir(grid: Grid[Char], x: Int, y: Int, dir: PrincibleWind2D): Boolean = {
    val digitalDir = dir.digitalDir
    val fullX = x + digitalDir.x * 3
    val fullY = y + digitalDir.y * 3
    if (fullX < 0 || fullX >= grid.width || fullY < 0 || fullY >= grid.height) {
      false
    } else {
      val c1 = grid(x, y)
      val c2 = grid(x + digitalDir.x, y + digitalDir.y)
      val c3 = grid(x + digitalDir.x * 2, y + digitalDir.y * 2)
      val c4 = grid(x + digitalDir.x * 3, y + digitalDir.y * 3)
      val str = String.valueOf(Array(c1, c2, c3, c4))
      // Don't test reverse to prevent double counting
      str == "XMAS"
    }
  }

  def xmasSearch(grid: Grid[Char], x: Int, y: Int): Boolean = {
    if (x <= 0 || x >= grid.width - 1 || y <= 0 || y >= grid.height - 1) {
      // Don't test edges
      false
    } else {
      (for {
        _ <- Option.when(grid(x, y) == 'A')(())
        c1 = grid(x - 1, y - 1)
        c2 = grid(x + 1, y + 1)
        _ <- Option.when((c1 == 'M' && c2 == 'S') || (c2 == 'M' && c1 == 'S'))(())
        c3 = grid(x + 1, y - 1)
        c4 = grid(x - 1, y + 1)
        _ <- Option.when((c3 == 'M' && c4 == 'S') || (c4 == 'M' && c3 == 'S'))(())
      } yield ()).isDefined
    }
  
  }

  override def part1(grid: Grid[Char]): Int =
    grid.indices.map { case (x, y) =>
      PrincibleWind2D.values.count(searchDir(grid, x, y, _))
    }.sum

  override def part2(grid: Grid[Char]): Int =
    grid.indices.count { case (x, y) =>
      xmasSearch(grid, x, y)
    }
