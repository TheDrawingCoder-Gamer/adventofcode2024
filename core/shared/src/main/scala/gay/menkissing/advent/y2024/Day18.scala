package gay.menkissing.advent
package y2024

import gay.menkissing.common.*

import scala.annotation.tailrec
import spire.implicits.IntAlgebra

object Day18 extends ProblemAdv:
  type Input = List[Vec2[Int]]
  type OutputP1 = Int
  type OutputP2 = Vec2[Int]

  def showOutputP1 = summon
  def showOutputP2 = summon

  case class GridSize(x: Int, y: Int)

  override def parse(str: String): List[Vec2[Int]] =
    str.linesIterator.map:
      case s"$x,$y" => Vec2(x.toInt, y.toInt)
    .toList

  override def part1(input: List[Vec2[Int]]): Int =
    val bytes = input.take(bytesFallen)
    val daGrid =
      bytes.foldLeft(Grid.fill(gridSize.x, gridSize.y)(false))((g, p) =>
        g.updated(p)(true)
      )
    val daEnd = Vec2(gridSize.x - 1, gridSize.y - 1)
    val path =
      astarByReturning(
        Vec2(0, 0),
        _ == daEnd,
        _.taxiDistance(daEnd),
        (_, _) => 1d,
        _.cardinalNeighbors.filter(p => daGrid.get(p).contains(false)),
        SearchReturns.lengthFromPath
      ).get

    path - 1

  extension [A](ls: List[A])
    @tailrec
    def foldLeftCollect[Z, R]
      (
        start: Z
      )
      (func: (Z, A) => Either[R, Z]): Option[R] =
      ls match
        case head :: rest =>
          func(start, head) match
            case Left(result) => Some(result)
            case Right(acc)   => rest.foldLeftCollect(acc)(func)
        case Nil => None

  override def part2(input: List[Vec2[Int]]): Vec2[Int] =
    val daGrid = Grid.fill(gridSize.x, gridSize.y)(false)
    val daEnd = Vec2(gridSize.x - 1, gridSize.y - 1)

    def testPath(g: Grid[Boolean]): Boolean =
      astar[Vec2[Int]](
        Vec2(0, 0),
        daEnd,
        _.taxiDistance(daEnd),
        (_, _) => 1d,
        _.cardinalNeighbors.filter(p => g.get(p).contains(false))
      ).isDefined

    val res =
      input.foldLeftCollect[Grid[Boolean], Vec2[Int]](daGrid): (grid, p) =>
        val newGrid = grid.updated(p)(true)
        val openNeighbors =
          p.cardinalNeighbors.map(newGrid.get).count(_.contains(false))
        if openNeighbors == 2 && !testPath(newGrid) then Left(p)
        else Right(newGrid)

    res.get

  val test = false

  val gridSize: GridSize = if test then GridSize(7, 7) else GridSize(71, 71)
  val bytesFallen: Int = if test then 12 else 1024
  override lazy val input: String = FileIO.getInput(2024, 18, test)
