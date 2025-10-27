package gay.menkissing.advent
package y2023

import gay.menkissing.common.{*, given}
import scala.collection.mutable
import cats.*
import cats.implicits.*
import cats.data.*

object Day10 extends Problem[(Grid[Option[Day10.Pipe]], Vec2i), Int]:
  lazy val input: String = FileIO.getInput(2023, 10)


  enum FloodFillTile:
    case Impassible
    case Filled
    case Empty

  case class Pipe(conn1: Direction2D, conn2: Direction2D):
    
    def exit(entrance: Direction2D): Direction2D =
      if conn1 == entrance then conn2 else conn1
    def contains(dir: Direction2D): Boolean = 
      dir == conn1 || dir == conn2

  object Pipe:
    def apply(conn1: Direction2D, conn2: Direction2D): Pipe =
      val min = if conn1.ordinal < conn2.ordinal then conn1 else conn2
      val max = if conn1.ordinal < conn2.ordinal then conn2 else conn1
      new Pipe(min, max)

    given Show[Pipe] = pipe =>
      (pipe.conn1, pipe.conn2) match
        case (Direction2D.Up, Direction2D.Down) => "|"
        case (Direction2D.Left, Direction2D.Right) => "-"
        case (Direction2D.Up, Direction2D.Right) => "L"
        case (Direction2D.Up, Direction2D.Left) => "J"
        case (Direction2D.Down, Direction2D.Left) => "7"
        case (Direction2D.Down, Direction2D.Right) => "F"
        case _ => whatTheScallop.!
  def parse(str: String): (Grid[Option[Day10.Pipe]], Vec2i) =
    val grid = 
      Grid.fromString(str):
        case '.' => None
        case '|' => Some(Pipe(Direction2D.Up, Direction2D.Down))
        case '-' => Some(Pipe(Direction2D.Left, Direction2D.Right))
        case 'L' => Some(Pipe(Direction2D.Up, Direction2D.Right))
        case 'J' => Some(Pipe(Direction2D.Up, Direction2D.Left))
        case '7' => Some(Pipe(Direction2D.Down, Direction2D.Left))
        case 'F' => Some(Pipe(Direction2D.Down, Direction2D.Right))
        // HACK : ( this makes me sad
        case 'S' => null
        case _ => whatTheScallop.!
    (grid, grid.indexWhere(_ == null).getOrElse(whatTheScallop.!))

  // LOOP TUAH THAT THANG!!!!!!
  def searchLoop(grid: Grid[Option[Pipe]], start: Vec2i): Map[Vec2i, Int] =
    val visited = mutable.Map[Vec2i, Int]()
    val dirs = grid(start).getOrElse(whatTheScallop.!)
    def advance(cur: Vec2i, enteredFrom: Direction2D, i: Int): Option[(Vec2i, Direction2D, Int)] =
      visited.updateWith(cur):
        case Some(v) => Some(math.min(v, i))
        case None => Some(i)
      val dir = grid(cur).getOrElse(whatTheScallop.!).exit(enteredFrom)
      val newCur = cur.offset(dir)
      Option.when(newCur != start && visited(cur) == i)((newCur, dir.reverse, i + 1))
    // ???
    val _ = Monad[Option].iterateForeverM((start, dirs.conn1, 0))(advance.tupled)
    val _ = Monad[Option].iterateForeverM((start, dirs.conn2, 0))(advance.tupled)
    visited.toMap

  def getStartGrid(grid: Grid[Option[Pipe]], start: Vec2i): Grid[Option[Pipe]] =
    val dirs = 
      Direction2D.values.toList
        .map: it => 
          (it, start.offset(it), grid.getOrElse(start.offset(it), None))
        .filter((dir, _, xs) => xs.exists(_.contains(dir.reverse)))
    assert(dirs.size == 2)
    grid.updated(start)(Some(Pipe(dirs(0)._1, dirs(1)._1)))

  def part1(input: (Grid[Option[Pipe]], Vec2i)): Int =
    val (grid, start) = input
    val goodGrid = getStartGrid(grid, start)

    searchLoop(goodGrid, start).maxBy(_._2)._2

  extension (self: Grid[Boolean])
    def floodFill(x: Int, y: Int): Set[Vec2i] = {
      val q = mutable.Queue[Vec2i]()
      val start = self(x, y)
      val res = mutable.Set[Vec2i]()
      q.addOne(Vec2i(x, y))
      while (q.nonEmpty) {
        val n = q.removeHead()
        if (self.get(n).contains(start) && !res.contains(n)) {
          res += n
          q.addAll(n.cardinalNeighbors)
        }
      }
      res.toSet
    }

  def part2(input: (Grid[Option[Pipe]], Vec2i)): Int =
    val (grid, start) = input
    val goodGrid = getStartGrid(grid, start)
    val map = searchLoop(goodGrid, start).as(true)
    val loopGrid = goodGrid.mapWithIndex((k, v) => if map.contains(k) then v else None).expand(None)(1)
    val newGrid = 
      loopGrid.scaleWith:
        case None => Grid.fill(3, 3)(false)
        case Some(v) => 
          val isUp = v.contains(Direction2D.Up)
          val isLeft = v.contains(Direction2D.Left)
          val isDown = v.contains(Direction2D.Down)
          val isRight = v.contains(Direction2D.Right)
          Grid:
            Vector(
              Vector(false, isUp, false),
              Vector(isLeft, true, isRight),
              Vector(false, isDown, false)
            )
    println(newGrid.width)
    println(newGrid.height)
    // println(newGrid.map(c => if c then 'X' else '.').show)
    val filledBigGrid = Grid.fromSparse(newGrid.width, newGrid.height, newGrid.floodFill(0, 0).map(i => (i, true)).toMap)(false)
    // println(filledBigGrid.map(if _ then 'X' else '.').show)
    val filledGrid = 
      filledBigGrid
        .scaleDownWith(3, 3): c =>
          c.flatten.forall(identity)
    assert(filledGrid.width == loopGrid.width)
    // println(loopGrid.map(_.map(_.show).getOrElse(".")).show)
    // println(filledGrid.map(c => if c then 'X' else '.').show)
    val junkTiles = loopGrid.zipWithIndices.filter(_._1.isEmpty).map(_._2).toSet
    (junkTiles -- filledGrid.zipWithIndices.filter(_._1).map(_._2).toSet).size