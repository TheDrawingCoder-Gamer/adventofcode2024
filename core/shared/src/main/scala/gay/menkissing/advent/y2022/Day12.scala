package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import spire.implicits.IntAlgebra

// import cats.collections.*
// import cats.collections.syntax.all.*
object Day12 extends Problem[(Vec2[Int], Vec2[Int], Day12.MountainMap), Int] {
  case class MountainMap(grid: Grid[Byte]) extends AnyVal {
    def verticies: Vector[Vec2[Int]] = 
      (for {
        xx <- 0 until grid.width
        yy <- 0 until grid.height 
      } yield Vec2(xx, yy)).toVector
    def neighbors(x: Int, y: Int): Iterable[Byte] = 
      neighbors(Vec2(x, y))
    def neighborVerts(p: Vec2[Int]): Iterable[Vec2[Int]] = 
      Direction2D.values.map(it => p.offset(it)).filter(it => hasEdge(p, it))
    def neighbors(p: Vec2[Int]): Iterable[Byte] = neighborVerts(p).map[Byte](p => grid.get(p).getOrElse(28)).toSeq 
    // travelling from u to v
    def hasEdge(u: Vec2[Int], v: Vec2[Int]): Boolean = {
      if (!grid.isDefinedAt(u.x, u.y) || !grid.isDefinedAt(v.x, v.y)) 
        false
      else {
        val uv = grid(u)
        val vv = grid(v)
        vv - uv <= 1
      }
    }
    def size = grid.width * grid.height
    def filterIndex(f: (Vec2[Int], Byte) => Boolean): Vector[(Vec2[Int], Byte)] = {
      (for {
        vert <- verticies 
      } yield (vert, grid(vert))).filter(f.tupled)
    }

    def zipWithIndex: Seq[(Byte, Vec2[Int])] = grid.zipWithIndices
  }

  def manhattanDistance(v1: Vec2[Int], v2: Vec2[Int]) = Math.abs(v1.x - v2.x) + Math.abs(v1.y - v2.y)


  def neighbors(p: Vec2[Int]): List[Vec2[Int]] = 
    Direction2D.values.map(it => p.offset(it)).toList


  def parse(str: String): (Vec2[Int], Vec2[Int], MountainMap) = {
    var start = Vec2(0,0)
    var end = Vec2(0, 0)
    var y = 0
    val values = str.linesIterator.map { it =>
      var x = 0
      val res = it.map {
        case 'S' => {
          start = Vec2(x, y)

          x += 1
          0
        }
        case 'E' => {
          end = Vec2(x, y)
          x += 1
          25
        } 
        case c => 
          x += 1 
          c - 'a'
      }
      y += 1
      res.map(_.toByte)
    }.toVector.map(_.toVector)
    val grid = Grid(values)
    (start, end, MountainMap(grid))
  }

  lazy val input = FileIO.getInput(2022, 12)
  def part1(input: (Vec2[Int], Vec2[Int], MountainMap)): Int =
    val (start, end, graph) = input
    astar(start, end, it => manhattanDistance(end, it), (_, _) => 1.0, graph.neighborVerts).get.length - 1

  def part2(input: (Vec2[Int], Vec2[Int], MountainMap)): Int = {
    val (_, end, graph) = input
    val starts = graph.zipWithIndex.flatMap((i, p) => Option.when(i == 0)(p))
    starts.flatMap(start => astar[Vec2[Int]](start, end, it => manhattanDistance(end, it), (_, _) => 1.0, graph.neighborVerts).map(_.length)).min - 1

  }
}
