package gay.menkissing.advent.y2024

import gay.menkissing.advent.{FileIO, Problem}
import gay.menkissing.common.*

import scala.collection.mutable as mut
import scala.io.Source

object Day20 extends Problem[Day20.RaceTrack, Int]:
  extension (grid: Grid[Boolean])
    def pathfind(start: Vec2i, goal: Vec2i): Option[List[Vec2i]] =
      def reconstructPath(cameFrom: Map[Vec2i, Vec2i], p: Vec2i): List[Vec2i] = {
        val totalPath = mut.ListBuffer[Vec2i](p)
        var current = p
        while (cameFrom.contains(current)) {
          current = cameFrom(current)
          totalPath.prepend(current)
        }
        totalPath.toList
      }
      val cameFrom = mut.HashMap[Vec2i, Vec2i]()
      val dist = mut.HashMap[Vec2i, Double](start -> 0d)

      val q = mut.PriorityQueue(start -> 0d)(using Ordering.by[(Vec2i, Double), Double](_._2).reverse)

      while q.nonEmpty && q.head._1 != goal do
        val (current, score) = q.dequeue()

        current.cardinalNeighbors.filter(grid.get(_).contains(false)).foreach: neighbor =>
          val alt = score + 1d
          if alt < dist.getOrElse(neighbor, Double.PositiveInfinity) then
            cameFrom(neighbor) = current
            dist(neighbor) = alt
            q.addOne(neighbor -> alt)

      q.headOption.map: (p, _) =>
        reconstructPath(cameFrom.toMap, p)

  case class RaceTrack(start: Vec2i, end: Vec2i, grid: Grid[Boolean]):
    lazy val basePath: List[Vec2i] = grid.pathfind(start, end).get

    def findCheats(limit: Int): List[Cheat] =
      basePath.zipWithIndex.flatMap: (lp, li) =>
        basePath.zipWithIndex.drop(li).flatMap: (rp, ri) =>
          val dist = lp.taxiDistance(rp)
          Option.when(dist <= limit && (dist < ri - li))(Cheat(lp, rp, (ri - li) - dist))


  case class Cheat(start: Vec2i, end: Vec2i, saved: Int)
  


  override def parse(str: String): RaceTrack =
    val goodGrid = mut.ArrayBuffer.fill(str.linesIterator.length, str.linesIterator.next().length)(false)
    var start = Vec2i(0, 0)
    var end = Vec2i(0, 0)
    str.linesIterator.zipWithIndex.foreach: (line, y) =>
      line.zipWithIndex.foreach: (char, x) =>
        char match
          case 'S' => start = Vec2i(x, y)
          case 'E' => end = Vec2i(x, y)
          case _ => ()
        char match
          case '#' => goodGrid(y)(x) = true
          case _ => ()
    RaceTrack(start, end, Grid(goodGrid))

  override def part1(input: RaceTrack): Int =
    val cheats = input.findCheats(2)

    cheats.count(_.saved >= 100)

  override def part2(input: RaceTrack): Int =
    val cheats = input.findCheats(20)

    cheats.count(_.saved >= 100)

  override lazy val input: String = FileIO.getInput(2024, 20)
