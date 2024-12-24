package gay.menkissing.advent

import gay.menkissing.advent.Problem
import gay.menkissing.common.*
import gay.menkissing.common.GridAxisSystem.*

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day8 extends Problem[Grid[Char], Int]:
  val input = Source.fromResource("day8.txt").mkString

  override def parse(str: String): Grid[Char] =
    Grid[Char](str.linesIterator.map(_.iterator))


  def calculateAntiNodesFor(grid: Grid[Char], c: Char, calcForPoints: (Grid[Char], Vec2i, Vec2i) => List[Vec2i]): Set[Vec2i] = {
    val antennaPoints = grid.indices.iterator.filter { case (x, y) =>
      grid(x, y) == c
    }.iterator.toSeq
  
    antennaPoints.combinations(2).flatMap { case Seq((x1, y1), (x2, y2)) =>
      calcForPoints(grid, Vec2i(x1, y1), Vec2i(x2, y2))
    }.toSet
  }
  
  def calculateAntiNodeForPoints(grid: Grid[Char], l: Vec2i, r: Vec2i): List[Vec2i] = {
    val run = (r.x - l.x)
    val rise = (r.y - l.y)
    val node1 = Vec2i(r.x + run, r.y + rise)
    val node2 = Vec2i(l.x - run, l.y - rise)
    List(node1, node2).filter(it => grid.isDefinedAt(it.x, it.y))
  }
  
  def rawCalculateP2AntiNodes(grid: Grid[Char], l: Vec2i, r: Vec2i): List[Vec2i] = {
    val run = (r.x - l.x)
    val rise = (r.y - l.y)
    List.unfold[Vec2i, Vec2i](Vec2i(r.x, r.y)) { vec =>
      val newP = Vec2i(vec.x + run, vec.y + rise)
      Option.when(grid.isDefinedAt(newP.x, newP.y))((newP, newP))
    }
  }
  
  def calculateAntiNodesForPointsP2(grid: Grid[Char], l: Vec2i, r: Vec2i): List[Vec2i] = {
    val lowerNodes = rawCalculateP2AntiNodes(grid, r, l)
    val higherNodes = rawCalculateP2AntiNodes(grid, l, r)
    lowerNodes ++ higherNodes ++ List(l, r)
  }
  
  def uniqueChars(grid: Grid[Char]): Set[Char] =
    grid.flatten.toSet - '.'

  override def part1(input: Grid[Char]): Int =
    uniqueChars(input).map(it => calculateAntiNodesFor(input, it, calculateAntiNodeForPoints)).reduce(_ ++ _).size

  override def part2(input: Grid[Char]): Int =
    uniqueChars(input).map(it => calculateAntiNodesFor(input, it, calculateAntiNodesForPointsP2)).reduce(_ ++ _).size

