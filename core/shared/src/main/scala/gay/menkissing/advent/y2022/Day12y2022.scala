package gay.menkissing.advent
package y2022

import gay.menkissing.common.*

// import cats.collections.*
// import cats.collections.syntax.all.*
object Day12y2022 extends Problem[(Vec2i, Vec2i, Day12y2022.MountainMap), Int] {
  case class MountainMap(grid: Grid[Byte]) {
    def verticies: Vector[Vec2i] = 
      (for {
        xx <- 0 until grid.width
        yy <- 0 until grid.height 
      } yield Vec2i(xx, yy)).toVector
    def neighbors(x: Int, y: Int): Iterable[Byte] = 
      neighbors(Vec2i(x, y))
    def neighborVerts(p: Vec2i): Iterable[Vec2i] = 
      Direction2D.values.map(it => p.offset(it)).filter(it => hasEdge(p, it))
    def neighbors(p: Vec2i): Iterable[Byte] = neighborVerts(p).map[Byte](p => grid.get(p).getOrElse(28)).toSeq 
    // travelling from u to v
    def hasEdge(u: Vec2i, v: Vec2i): Boolean = {
      if (!grid.isDefinedAt(u.x, u.y) || !grid.isDefinedAt(v.x, v.y)) 
        false
      else {
        val uv = grid(u)
        val vv = grid(v)
        vv - uv <= 1
      }
    }
    def size = grid.width * grid.height
    def filterIndex(f: (Vec2i, Byte) => Boolean): Vector[(Vec2i, Byte)] = {
      (for {
        vert <- verticies 
      } yield (vert, grid(vert))).filter(f.tupled)
    }
  }

  def manhattanDistance(v1: Vec2i, v2: Vec2i) = Math.abs(v1.x - v2.x) + Math.abs(v1.y - v2.y)


  def neighbors(p: Vec2i): List[Vec2i] = 
    Direction2D.values.map(it => p.offset(it)).toList


  def parse(str: String): (Vec2i, Vec2i, MountainMap) = {
    var start = Vec2i(0,0)
    var end = Vec2i(0, 0)
    var y = 0
    val values = str.linesIterator.map { it =>
      var x = 0
      val res = it.map {
        case 'S' => {
          start = Vec2i(x, y)

          x += 1
          0
        }
        case 'E' => {
          end = Vec2i(x, y)
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
  def part1(input: (Vec2i, Vec2i, MountainMap)): Int =
    val (start, end, graph) = input
    astar(start, end, it => manhattanDistance(end, it), (_, _) => 1.0, graph.neighborVerts).get.length - 1

  def part2(input: (Vec2i, Vec2i, MountainMap)): Int = {
    val (start, end, graph) = input
    val starts = graph.filterIndex((i, b) => b == 0).map(_._1)
    val res = starts.map(start => astar[Vec2i](start, end, it => manhattanDistance(end, it), (_, _) => 1.0, graph.neighborVerts))

    res.map(_.map(_.length).getOrElse(Int.MaxValue)).min - 1
  }
}
