import gay.menkissing.common.*
import gay.menkissing.advent.Problem

import scala.io.Source
import scala.collection.mutable as mut
import scala.collection.parallel.CollectionConverters.*

object Day20 extends Problem[Day20.RaceTrack, Int]:
  extension (grid: Grid[Boolean])
    def validCheat(cheat: Cheat): Boolean = grid.timePathfind(cheat.start, cheat.end, cheat.length.toDouble).isEmpty
    def timePathfind(start: Vec2i, goal: Vec2i, limit: Double = Double.PositiveInfinity): Option[Int] =
      val cameFrom = mut.HashMap[Vec2i, Vec2i]()
      val dist = mut.HashMap[Vec2i, Double](start -> 0d)

      val q = mut.PriorityQueue(start -> 0d)(using Ordering.by[(Vec2i, Double), Double](_._2).reverse)

      while q.nonEmpty do
        val (current, score) = q.dequeue()

        if score > limit then
          return None
        if current == goal then
          return Some(score.toInt)

        current.cardinalNeighbors.filter(grid.get(_).contains(false)).foreach: neighbor =>
          val alt = score + 1d
          if alt < dist.getOrElse(neighbor, Double.PositiveInfinity) then
            cameFrom(neighbor) = current
            dist(neighbor) = alt
            q.addOne(neighbor -> alt)

      None

  case class RaceTrack(start: Vec2i, end: Vec2i, grid: Grid[Boolean]):
    lazy val baseScore: Int =
      grid.timePathfind(start, end).get


    private val startCache = mut.HashMap[Vec2i, Int]()
    private val endCache = mut.HashMap[Vec2i, Int]()

    def cheatScore(cheat: Cheat): Int =
      val startScore = startCache.getOrElseUpdate(cheat.start, grid.timePathfind(start, cheat.start).get)
      val endScore = endCache.getOrElseUpdate(cheat.end, grid.timePathfind(cheat.end, end).get)
      startScore + cheat.length + endScore

  case class Cheat(start: Vec2i, end: Vec2i):
    val length: Int = start.taxiDistance(end)
  


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

  def findCheats(grid: Grid[Boolean], limit: Int = 2): List[Cheat] =
    val emptyTiles = grid.zipWithIndices.filter(!_._1)

    emptyTiles.flatMap: (_, l) =>
      emptyTiles.flatMap: (_, r) =>
        if l == r then
          None
        else if l.taxiDistance(r) > limit then
          None
        else
          val c = Cheat(l, r)
          Option.when(grid.validCheat(c))(c)
    .toList

  override def part1(input: RaceTrack): Int =
    val baseScore = input.baseScore
    val cheats = findCheats(input.grid)
    val timesSaved = cheats.par.map(baseScore - input.cheatScore(_))
    // println(timesSaved.groupBy(identity).map((l, r) => (l, r.length)))
    timesSaved.count(_ >= 100)

  override def part2(input: RaceTrack): Int =
    val baseScore = input.baseScore
    val cheats = findCheats(input.grid, 20)
    val timesSaved = cheats.par.map(baseScore - input.cheatScore(_))

    timesSaved.count(_ >= 100)

  override lazy val input: String = Source.fromResource("day20.txt").mkString

@main def main(): Unit =
  Day20.debugAndTimeP1()
  Day20.debugAndTimeP2()