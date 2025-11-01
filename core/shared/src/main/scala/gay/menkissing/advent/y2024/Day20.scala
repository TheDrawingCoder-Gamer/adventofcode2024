package gay.menkissing.advent
package y2024

import gay.menkissing.common.*

import scala.collection.mutable as mut
import spire.implicits.IntAlgebra
import cats.syntax.all.*

object Day20 extends Problem[Day20.RaceTrack, Int]:
  extension (grid: Grid[Boolean])
    def pathfind(start: Vec2[Int], goal: Vec2[Int]): Option[List[Vec2[Int]]] =
      def reconstructPath
        (
          cameFrom: Map[Vec2[Int], Vec2[Int]],
          p: Vec2[Int]
        ): List[Vec2[Int]] =
        val totalPath = mut.ListBuffer[Vec2[Int]](p)
        var current = p
        while cameFrom.contains(current) do
          current = cameFrom(current)
          totalPath.prepend(current)
        totalPath.toList
      val cameFrom = mut.HashMap[Vec2[Int], Vec2[Int]]()
      val dist = mut.HashMap[Vec2[Int], Int](start -> 0)

      val q = MinBinaryHeap[Vec2[Int], Int]()
      q.insert(start, 0)

      while q.nonEmpty && q.head =!= goal do
        val (current, score) = q.extractWithPriority()

        current.cardinalNeighbors.filter(grid.get(_).contains(false)).foreach:
          neighbor =>
            val alt = score + 1
            if dist.get(neighbor).forall(alt < _) then
              cameFrom(neighbor) = current
              dist(neighbor) = alt
              q.updatePriority(neighbor, alt)

      q.headOption.map: p =>
        reconstructPath(cameFrom.toMap, p)

  case class RaceTrack(start: Vec2[Int], end: Vec2[Int], grid: Grid[Boolean]):
    lazy val basePath: List[Vec2[Int]] = grid.pathfind(start, end).get

    def findCheats(limit: Int): List[Cheat] =
      basePath.zipWithIndex.flatMap: (lp, li) =>
        basePath.zipWithIndex.drop(li).flatMap: (rp, ri) =>
          val dist = lp.taxiDistance(rp)
          Option.when(dist <= limit && (dist < ri - li))(
            Cheat(lp, rp, (ri - li) - dist)
          )

  case class Cheat(start: Vec2[Int], end: Vec2[Int], saved: Int)

  override def parse(str: String): RaceTrack =
    val goodGrid =
      mut.ArrayBuffer
        .fill(str.linesIterator.length, str.linesIterator.next().length)(false)
    var start = Vec2(0, 0)
    var end = Vec2(0, 0)
    str.linesIterator.zipWithIndex.foreach: (line, y) =>
      line.zipWithIndex.foreach: (char, x) =>
        char match
          case 'S' => start = Vec2(x, y)
          case 'E' => end = Vec2(x, y)
          case _   => ()
        char match
          case '#' => goodGrid(y)(x) = true
          case _   => ()
    RaceTrack(start, end, Grid(goodGrid))

  override def part1(input: RaceTrack): Int =
    val cheats = input.findCheats(2)

    cheats.count(_.saved >= 100)

  override def part2(input: RaceTrack): Int =
    val cheats = input.findCheats(20)

    cheats.count(_.saved >= 100)

  override lazy val input: String = FileIO.getInput(2024, 20)
