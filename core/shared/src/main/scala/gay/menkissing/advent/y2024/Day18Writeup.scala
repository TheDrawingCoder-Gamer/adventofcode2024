package gay.menkissing.advent
package y2024

import scala.annotation.tailrec
import scala.collection.mutable as mut
import cats.derived.*
import cats.*

object Day18Writeup extends ProblemAdv:
  type Input = (Int, Int, List[Vec2i])
  type OutputP1 = Int
  type OutputP2 = Vec2i

  override given showOutputP2: Show[Vec2i] = Show.derived

  case class Vec2i(x: Int, y: Int):
    def cardinalNeighbors: List[Vec2i] =
      List(Vec2i(x - 1, y), Vec2i(x + 1, y), Vec2i(x, y - 1), Vec2i(x, y + 1))

    def isContainedIn(w: Int, h: Int): Boolean =
      x >= 0 && x < w && y >= 0 && y < h

  extension (walls: Set[Vec2i])
    def search(gridSize: Int): Option[List[Vec2i]] =
      def reconstructPath(cameFrom: Map[Vec2i, Vec2i], p: Vec2i): List[Vec2i] =
        val totalPath = mut.ListBuffer[Vec2i](p)
        var current = p
        while cameFrom.contains(current) do
          current = cameFrom(current)
          totalPath.prepend(current)
        totalPath.toList

      val start = Vec2i(0, 0)
      val goal = Vec2i(gridSize - 1, gridSize - 1)
      val cameFrom = mut.HashMap[Vec2i, Vec2i]()

      val dist = mut.HashMap(start -> 0d)

      val q =
        mut.PriorityQueue(start -> 0d)(using
          Ordering.by[(Vec2i, Double), Double](_._2).reverse
        )

      while q.nonEmpty && q.head._1 != goal do
        val (current, score) = q.dequeue()

        current.cardinalNeighbors.filter(it =>
          it.isContainedIn(gridSize, gridSize) && !walls.contains(it)
        ).foreach: neighbor =>
          val alt = score + 1d
          if alt < dist.getOrElse(neighbor, Double.PositiveInfinity) then
            cameFrom(neighbor) = current
            dist(neighbor) = alt
            q.addOne(neighbor -> alt)

      q.headOption.map(it => reconstructPath(cameFrom.toMap, it._1))

  override def parse(str: String): (Int, Int, List[Vec2i]) =
    val ps =
      str.linesIterator.map:
        case s"$x,$y" => Vec2i(x.toInt, y.toInt)
      .toList
    (
      if ps.length == 25 then 7 else 71,
      if ps.length == 25 then 12 else 1024,
      ps
    )

  override def part1(input: (Int, Int, List[Vec2i])): Int =
    val (size, take, walls) = input
    walls.take(take).toSet.search(size).get.size - 1

  override def part2(input: (Int, Int, List[Vec2i])): Vec2i =
    val (size, take, walls) = input
    // binary search
    Iterator.iterate(take -> walls.size): (i0, i1) =>
      if walls.take((i0 + i1) / 2).toSet.search(size).isEmpty then
        i0 -> (i0 + i1) / 2
      else (i0 + i1) / 2 + 1 -> i1
    .flatMap: (i0, i1) =>
      Option.when(i0 == i1)(walls(i0 - 1))
    .next()

  val test = false

  override lazy val input: String = FileIO.getInput(2024, 18, test)
