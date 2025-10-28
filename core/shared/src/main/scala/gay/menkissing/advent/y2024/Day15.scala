package gay.menkissing.advent.y2024

import cats.*
import cats.implicits.*
import spire.implicits.IntAlgebra
import gay.menkissing.advent.{FileIO, Problem}
import gay.menkissing.common.*

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable as mut
import scala.io.Source


object Day15 extends Problem[Day15.ProblemState, Long] {
  enum GridItem:
    case Empty, Wall, Box

  enum GridItemP2:
    case Empty, Wall, BoxL, BoxR

  extension (grid: Grid[GridItem]) {
    @targetName("updatedMoveP1")
    def updatedMove(start: Vec2i, dir: Direction2D): Option[Grid[GridItem]] = {
      @tailrec
      def go(curGrid: Grid[GridItem], start: Vec2i): Option[Grid[GridItem]] =
        val newPos = start.offset(dir)
        val oldItem = curGrid(start)
        curGrid(newPos) match {
          case GridItem.Wall => None
          case GridItem.Empty => Some(curGrid.updated(newPos)(oldItem))
          case GridItem.Box => go(curGrid, newPos)
        }

      // prevent weird
      go(grid.updated(start)(GridItem.Empty), start).map: it =>
        val newPos = start.offset(dir)
        if it.isDefinedAt(newPos.x, newPos.y) then
          it.updated(newPos)(GridItem.Empty)
        else it
    }
    def pretty: String =
      grid.values.map: it =>
        it.map:
          case GridItem.Empty => '.'
          case GridItem.Wall => '#'
          case GridItem.Box => 'O'
        .mkString("", "", "")
      .mkString("", "\n", "")
  }

  extension (grid: Grid[GridItemP2])
    @targetName("updatedMoveP2")
    def updatedMove(istart: Vec2i, dir: Direction2D): Option[Grid[GridItemP2]] =
      // ???
      def go(start: Vec2i, movedItems: List[(Vec2i, GridItemP2)]): Eval[Option[List[(Vec2i, GridItemP2)]]] =
        val newPos = start.offset(dir)
        grid(newPos) match
          case GridItemP2.Wall => Eval.always(None)
          case GridItemP2.Empty => Eval.now(Some(movedItems))
          case GridItemP2.BoxL =>
            val rPos = newPos.offset(Direction2D.Right)
            assert(grid(rPos) == GridItemP2.BoxR)
            val newItems = movedItems.prepended(newPos, GridItemP2.BoxL)
            dir match
              case Direction2D.Left | Direction2D.Right =>
                go(newPos, newItems)
              case Direction2D.Up | Direction2D.Down =>
                go(newPos, newItems).flatMap:
                  case Some(it) => go(rPos, it.prepended(rPos, GridItemP2.BoxR))
                  case None => Eval.now(None)
          case GridItemP2.BoxR =>
            val lPos = newPos.offset(Direction2D.Left)
            assert(grid(lPos) == GridItemP2.BoxL)
            val newItems = movedItems.prepended(newPos, GridItemP2.BoxR)
            dir match
              case Direction2D.Right | Direction2D.Left =>
                go(newPos, newItems)
              case Direction2D.Up | Direction2D.Down =>
                go(newPos, newItems).flatMap:
                  case Some(it) => go(lPos, it.prepended(lPos, GridItemP2.BoxL))
                  case None => Eval.now(None)


      go(istart, List()).value.map:
        val alreadyInspected = mut.Set[Vec2i]()
        _.foldLeft(grid.updated(istart)(GridItemP2.Empty)):
          case (g, (p, item)) =>
            if (!alreadyInspected.contains(p))
              alreadyInspected.add(p)
              g.updated(p)(GridItemP2.Empty).updated(p.offset(dir))(item)
            else g

  case class ProblemState(grid: Grid[GridItem], robot: Vec2i, remainingMoves: List[Direction2D]):
    def step: Option[ProblemState] =
      remainingMoves match
        case head :: rest =>
          grid.updatedMove(robot, head) match
            case Some(newGrid) => Some(ProblemState(newGrid, robot.offset(head), rest))
            case _ => Some(ProblemState(grid, robot, rest))
        case _ => None

    def gpsCalc: Long =
      grid.zipWithIndices.filter(_._1 == GridItem.Box).map((_, p) => p.y.toLong * 100L + p.x.toLong).sum

    def doubled: ProblemStateP2 =
      val newGrid = Grid(grid.values.map(_.flatMap:
        case GridItem.Box => List(GridItemP2.BoxL, GridItemP2.BoxR)
        case GridItem.Wall => List(GridItemP2.Wall, GridItemP2.Wall)
        case GridItem.Empty => List(GridItemP2.Empty, GridItemP2.Empty)
      ))
      ProblemStateP2(newGrid, robot.copy(x = robot.x * 2), remainingMoves)
    def pretty: String =
      (grid.values.zipWithIndex.map: (it, y) =>
        (it.zipWithIndex.map: (m, x) =>
          m match
            case _ if Vec2i(x, y) == robot => '@'
            case GridItem.Empty => '.'
            case GridItem.Wall => '#'
            case GridItem.Box => 'O').mkString("", "", "")).mkString("", "\n", "")

  case class ProblemStateP2(grid: Grid[GridItemP2], robot: Vec2i, remainingMoves: List[Direction2D]):
    def step: Option[ProblemStateP2] =
      remainingMoves match
        case head :: rest =>
          grid.updatedMove(robot, head) match
            case Some(newGrid) => Some(ProblemStateP2(newGrid, robot.offset(head), rest))
            case _ => Some(ProblemStateP2(grid, robot, rest))
        case _ => None

    def gpsCalc: Long =
      grid.zipWithIndices.filter(_._1 == GridItemP2.BoxL).map((_, p) => p.y.toLong * 100L + p.x.toLong).sum

    def pretty: String =
      (grid.values.zipWithIndex.map: (it, y) =>
        (it.zipWithIndex.map: (m, x) =>
          m match
            case _ if Vec2i(x, y) == robot => '@'
            case GridItemP2.Empty => '.'
            case GridItemP2.Wall => '#'
            case GridItemP2.BoxL => '['
            case GridItemP2.BoxR => ']').mkString("", "", "")).mkString("", "\n", "")
  override def parse(str: String): ProblemState =
    val sides = str.split("\n\n")
    var robotPos = Vec2i(-1, -1)
    val grid = Grid(sides(0).linesIterator.zipWithIndex.map: (str, y) =>
      str.toVector.zipWithIndex.map: (it, x) =>
        it match
          case '#' => GridItem.Wall
          case 'O' => GridItem.Box
          case '.' => GridItem.Empty
          case '@' =>
            assert(robotPos == Vec2i(-1, -1))
            robotPos = Vec2i(x, y)
            GridItem.Empty
          case _ => ???
      )
    val dirs = sides(1).linesIterator.flatten.map:
      case '<' => Direction2D.Left
      case '^' => Direction2D.Up
      case '>' => Direction2D.Right
      case 'v' => Direction2D.Down
      case _ => ???

    ProblemState(grid, robotPos, dirs.toList)

  override def part1(input: ProblemState): Long =
    @tailrec
    def go(i: ProblemState): ProblemState =
      if i.remainingMoves.nonEmpty then
        go(i.step.get)
      else i

    //println(input.pretty)
    val res = go(input)
    //println(res.grid.pretty)
    res.gpsCalc

  override def part2(input: ProblemState): Long =
    @tailrec def go(i: ProblemStateP2): ProblemStateP2 =
      if i.remainingMoves.nonEmpty then
        val r = i.step.get
        //println(i.remainingMoves.head)
        //println(r.pretty)
        go(r)
      else i

    // println(input.doubled.step.get.step.get.pretty)
    val res = go(input.doubled)
    res.gpsCalc

  override lazy val input: String = FileIO.getInput(2024, 15)
}
