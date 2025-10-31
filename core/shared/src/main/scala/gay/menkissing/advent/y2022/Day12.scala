package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import spire.implicits.IntAlgebra

// import cats.collections.*
// import cats.collections.syntax.all.*
object Day12 extends Problem[(Vec2[Int], Vec2[Int], Day12.MountainMap), Int]:
  case class MountainMap(grid: Grid[Byte]) extends AnyVal:
    def verticies: Vector[Vec2[Int]] = 
      (0 until grid.width).flatMap(x => 0.until(grid.height).map(y => Vec2(x, y))).toVector
    def neighbors(x: Int, y: Int): Iterable[Byte] = 
      neighbors(Vec2(x, y))
    def neighborVerts(p: Vec2[Int]): Iterable[Vec2[Int]] = 
      Direction2D.values.map(it => p.offset(it)).filter(it => hasEdge(p, it))
    def neighbors(p: Vec2[Int]): Iterable[Byte] = neighborVerts(p).map[Byte](p => grid.get(p).getOrElse(28)).toSeq 
    // travelling from u to v
    def hasEdge(u: Vec2[Int], v: Vec2[Int]): Boolean =
      if !grid.isDefinedAt(u.x, u.y) || !grid.isDefinedAt(v.x, v.y) then
        false
      else
        val uv = grid(u)
        val vv = grid(v)
        vv - uv <= 1
    def size = grid.width * grid.height
    def filterIndex(f: (Vec2[Int], Byte) => Boolean): Vector[(Vec2[Int], Byte)] =
      verticies.map(vert => (vert, grid(vert))).filter(f.tupled)

    def zipWithIndex: Seq[(Byte, Vec2[Int])] = grid.zipWithIndices


  def neighbors(p: Vec2[Int]): List[Vec2[Int]] = 
    Direction2D.values.map(it => p.offset(it)).toList


  def parse(str: String): (Vec2[Int], Vec2[Int], MountainMap) =
    var start = Vec2(0,0)
    var end = Vec2(0, 0)
    var y = 0
    val grid = 
      Grid.fromStringWithIndex(str):
        case (v, 'S') => 
          start = v
          0.toByte
        case (v, 'E') =>
          end = v
          25.toByte
        case (_, c) =>
          (c - 'a').toByte
    (start, end, MountainMap(grid))

  lazy val input = FileIO.getInput(2022, 12)
  def part1(input: (Vec2[Int], Vec2[Int], MountainMap)): Int =
    val (start, end, graph) = input
    astar(start, end, _.taxiDistance(end), (_, _) => 1.0, graph.neighborVerts).get.length - 1

  def part2(input: (Vec2[Int], Vec2[Int], MountainMap)): Int =
    val (_, end, graph) = input
    val starts = graph.zipWithIndex.flatMap((i, p) => Option.when(i == 0)(p))
    starts.flatMap(start => astar[Vec2[Int]](start, end, _.taxiDistance(end), (_, _) => 1.0, graph.neighborVerts).map(_.length)).min - 1

