package gay.menkissing.advent.y2024

import gay.menkissing.advent.{FileIO, Problem}
import gay.menkissing.common.*
import ArityN.*
import spire.implicits.IntAlgebra

import scala.io.Source

object Day16 extends Problem[Day16.ProblemState, Int]:
  type Maze = Grid[Boolean]

  extension (maze: Maze)
    def neighbors(reindeer: Reindeer): List[Reindeer] =
      reindeer.neighbors.filter(it => maze.get(it.pos).contains(false))

  extension (solution: List[Reindeer])
    def solved: Int =
      solution.slidingN[2].foldLeft(0.0) {
        case (acc, (l, r)) =>
          acc + l.edgeScore(r)
      }.toInt
  case class Reindeer(pos: Vec2[Int], dir: Direction2D):
    def neighbors: List[Reindeer] =
      Direction2D.values.flatMap { d =>
        if (d == dir.clockwise.clockwise)
          None else Some(Reindeer(pos.offset(d), d))
      }.toList
    def edgeScore(that: Reindeer): Double = {
      if (math.abs(this.pos.x - that.pos.x) != 1 && math.abs(this.pos.y - that.pos.y) != 1)
        Double.PositiveInfinity
      else if (this.dir == that.dir)
        1.0
      else if (this.dir.clockwise == that.dir || this.dir.counterClockwise == that.dir)
        1001.0
      else
        // hopefully this is never attempted
        // 2001.0
        // Prevent turning aorund
        Double.PositiveInfinity
    }

  case class ProblemState(reindeer: Reindeer, maze: Maze, end: Vec2[Int]):
    def getSolution: List[Reindeer] =
      astarGeneric[Reindeer](reindeer, _.pos == end, _.pos.taxiDistance(end), _.edgeScore(_), maze.neighbors).get

    def getGoodSeats: Set[Vec2[Int]] =
      val paths = dijstraAll[Reindeer](reindeer, _.pos == end, _.edgeScore(_), maze.neighbors, (l, r) => l.pos == r.pos)
      paths.map(_.map(_.pos).toSet).reduce(_ ++ _)

    def solve: Int =
      getSolution.solved

    def prettySolution(solution: List[Reindeer]): String =
      val solMap = solution.groupBy(_.pos)
      maze.values.zipWithIndex.map { (line, y) =>
        line.zipWithIndex.map { (wall, x) =>
          val p = Vec2(x, y)
          if (wall)
            '#'
          else if (reindeer.pos == p) {
            'S'
          } else if (end == p)
            'E'
          else
            solMap.get(Vec2(x, y)) match
              case None => '.'
              case Some(v) =>
                v.head.dir match
                  case Direction2D.Up => "^"
                  case Direction2D.Right => ">"
                  case Direction2D.Down => "v"
                  case Direction2D.Left => "<"
        }.mkString("", "", "")
      }.mkString("", "\n", "")


  override def parse(str: String): ProblemState =
    var reindeer = Reindeer(Vec2(0, 0), Direction2D.Right)
    var end = Vec2(0, 0)

    val maze = Grid(str.linesIterator.zipWithIndex.map: (line, y) =>
      line.zipWithIndex.map: (c, x) =>
        c match {
          case 'E' => end = Vec2(x, y)
          case 'S' => reindeer = Reindeer(Vec2(x, y), Direction2D.Right)
          case _ =>
        }
        c match {
          case '#' => true
          case _ => false
        })

    ProblemState(reindeer, maze, end)

  override def part1(input: ProblemState): Int =
    val soln = input.getSolution
    //println(soln)
    //println(input.prettySolution(soln))
    soln.solved

  override def part2(input: ProblemState): Int =
    input.getGoodSeats.size

  override lazy val input: String = FileIO.getInput(2024, 16)



