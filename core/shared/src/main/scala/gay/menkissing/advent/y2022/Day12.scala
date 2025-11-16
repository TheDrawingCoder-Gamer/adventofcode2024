package gay.menkissing.advent
package y2022

import gay.menkissing.common.*, algebras.given

// import cats.collections.*
// import cats.collections.syntax.all.*
object Day12 extends Problem:
  type Input = (Vec2[Int], Vec2[Int], MountainMap)
  type Output = Int

  type MountainMap = MountainMap.Type
  object MountainMap extends Newtype[Grid[Byte]]:
    extension (self: Type)
      def hasEdge(u: Vec2[Int], v: Vec2[Int]): Boolean =
        if !self.value.isDefinedAt(u) || !self.value.isDefinedAt(v) then false
        else
          val uv = self.value(u)
          val vv = self.value(v)
          vv - uv <= 1
      def neighborVerts(p: Vec2[Int]): Iterable[Vec2[Int]] =
        p.cardinalNeighbors.filter(it => hasEdge(p, it))
      inline def zipWithIndex: Seq[(Byte, Vec2[Int])] =
        self.value.zipWithIndices

  def parse(str: String): (Vec2[Int], Vec2[Int], MountainMap) =
    var start = Vec2(0, 0)
    var end = Vec2(0, 0)

    val grid =
      Grid.fromStringWithIndex(str):
        case (v, 'S') =>
          start = v
          0.toByte
        case (v, 'E') =>
          end = v
          25.toByte
        case (_, c) => (c - 'a').toByte
    (start, end, MountainMap(grid))

  lazy val input = FileIO.getInput(2022, 12)
  def part1(input: (Vec2[Int], Vec2[Int], MountainMap)): Int =
    val (start, end, graph) = input
    astarScore(
      start,
      end,
      _.taxiDistance(end),
      (_, _) => 1.0,
      graph.neighborVerts
    ).get.toInt

  def part2(input: (Vec2[Int], Vec2[Int], MountainMap)): Int =
    val (_, end, graph) = input
    val starts = graph.zipWithIndex.flatMap((i, p) => Option.when(i == 0)(p))
    starts.flatMap(start =>
      astarScore[Vec2[Int]](
        start,
        end,
        _.taxiDistance(end),
        (_, _) => 1.0,
        graph.neighborVerts
      ).map(_.toInt)
    ).min
