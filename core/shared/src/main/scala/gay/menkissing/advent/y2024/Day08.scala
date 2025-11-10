package gay.menkissing.advent
package y2024

import gay.menkissing.common.*, ArityN.*

import scala.collection.mutable.ListBuffer
import algebra.instances.all.*

object Day08 extends Problem:
  type Input = Grid[Char]
  type Output = Int

  lazy val input = FileIO.getInput(2024, 8)

  override def parse(str: String): Grid[Char] =
    Grid[Char](str.linesIterator.map(_.iterator))

  def calculateAntiNodesFor
    (
      grid: Grid[Char],
      c: Char,
      calcForPoints: (Grid[Char], Vec2[Int], Vec2[Int]) => List[Vec2[Int]]
    ): Set[Vec2[Int]] =
    val antennaPoints =
      grid.indices.iterator.filter:
        case (x, y) => grid(x, y) == c
      .iterator.toSeq

    antennaPoints.combinationsN[2].flatMap:
      case ((x1, y1), (x2, y2)) =>
        calcForPoints(grid, Vec2(x1, y1), Vec2(x2, y2))
    .toSet

  def calculateAntiNodeForPoints
    (
      grid: Grid[Char],
      l: Vec2[Int],
      r: Vec2[Int]
    ): List[Vec2[Int]] =
    val run = (r.x - l.x)
    val rise = (r.y - l.y)
    val node1 = Vec2(r.x + run, r.y + rise)
    val node2 = Vec2(l.x - run, l.y - rise)
    List(node1, node2).filter(it => grid.isDefinedAt(it.x, it.y))

  def rawCalculateP2AntiNodes
    (
      grid: Grid[Char],
      l: Vec2[Int],
      r: Vec2[Int]
    ): List[Vec2[Int]] =
    val run = (r.x - l.x)
    val rise = (r.y - l.y)
    List.unfold[Vec2[Int], Vec2[Int]](Vec2(r.x, r.y)): vec =>
      val newP = Vec2(vec.x + run, vec.y + rise)
      Option.when(grid.isDefinedAt(newP.x, newP.y))((newP, newP))

  def calculateAntiNodesForPointsP2
    (
      grid: Grid[Char],
      l: Vec2[Int],
      r: Vec2[Int]
    ): List[Vec2[Int]] =
    val lowerNodes = rawCalculateP2AntiNodes(grid, r, l)
    val higherNodes = rawCalculateP2AntiNodes(grid, l, r)
    lowerNodes ++ higherNodes ++ List(l, r)

  def uniqueChars(grid: Grid[Char]): Set[Char] = grid.flatten.toSet - '.'

  override def part1(input: Grid[Char]): Int =
    uniqueChars(input)
      .map(it => calculateAntiNodesFor(input, it, calculateAntiNodeForPoints))
      .reduce(_ ++ _).size

  override def part2(input: Grid[Char]): Int =
    uniqueChars(input).map(it =>
      calculateAntiNodesFor(input, it, calculateAntiNodesForPointsP2)
    ).reduce(_ ++ _).size
