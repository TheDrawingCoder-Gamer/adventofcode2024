package gay.menkissing.advent.y2024

import gay.menkissing.advent.{FileIO, Problem}
import gay.menkissing.common.*

import scala.io.Source


object Day10 extends Problem[Grid[Int], Int]:
  lazy val input = FileIO.getInput(2024, 10)

  override def parse(str: String): Grid[Int] =
    Grid[Int](str.linesIterator.map(_.map(_.asDigit)))

  def neighbors(grid: Grid[Int], pos: Vec2i): Seq[Vec2i] = {
    Direction2D.values.flatMap { dir =>
      val daPos = pos.offset(dir)
      Option.when(grid.isDefinedAt(daPos.x, daPos.y))(daPos)
    }.filter(it => grid(it) - grid(pos) == 1)
  }
  
  def scoreTrail(grid: Grid[Int], start: Vec2i, endNodes: Seq[Vec2i]): Int = {
    endNodes.collect(Function.unlift { it =>
      astar[Vec2i](start, it, c => c.taxiDistance(it), (l, r) => if (grid(r) - grid(l) == 1) 1.0 else Double.PositiveInfinity, i => neighbors(grid, i))
    }).size
  }
  
  def rateTrailhead(grid: Grid[Int], start: Vec2i, endNodes: Seq[Vec2i]): Int = {
    def rateSingleTrail(e: Vec2i): Int = {
      findAllPaths[Vec2i](start, e, it => neighbors(grid, it)).size
    }
    endNodes.map(rateSingleTrail).sum
  }
  
  def parseZeroNines(data: Grid[Int]): (Seq[Vec2i], Seq[Vec2i]) =
    (data.zipWithIndices.iterator.withFilter(_._1 == 0).map(_._2).toSeq, data.zipWithIndices.withFilter(_._1 == 9).map(_._2).toSeq)
    
  

  def part1(grid: Grid[Int]): Int = {
    val (zeros, nines) = parseZeroNines(grid)
  
    zeros.map(scoreTrail(grid, _, nines)).sum
  }
  
  

  def part2(grid: Grid[Int]): Int = {
    val (zeros, nines) = parseZeroNines(grid)
  
    zeros.map(rateTrailhead(grid, _, nines)).sum
  }
