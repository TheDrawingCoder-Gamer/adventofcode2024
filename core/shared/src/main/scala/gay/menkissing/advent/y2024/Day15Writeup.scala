package gay.menkissing.advent
package y2024

import cats.*

import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

object Day15Writeup extends Writeup[Day15Writeup.ProblemState, Long]:
  enum Direction:
    case Up, Down, Left, Right

  final case class Vec2i(x: Int, y: Int):
    def offset(dir: Direction): Vec2i =
      dir match
        case Direction.Up    => Vec2i(x, y - 1)
        case Direction.Down  => Vec2i(x, y + 1)
        case Direction.Left  => Vec2i(x - 1, y)
        case Direction.Right => Vec2i(x + 1, y)

    def isContainedIn(w: Int, h: Int): Boolean =
      x >= 0 && x < w && y >= 0 && y < h

  final case class Grid[A](values: Vector[Vector[A]]):
    val height: Int = values.size
    val width: Int = values.head.size

    def apply(x: Int, y: Int): A = values(y)(x)
    def apply(p: Vec2i): A = apply(p.x, p.y)

    def isDefinedAt(p: Vec2i): Boolean = p.isContainedIn(width, height)
    def isDefinedAt(x: Int, y: Int): Boolean = isDefinedAt(Vec2i(x, y))

    def updated(x: Int, y: Int)(value: A): Grid[A] =
      Grid(values.updated(y, values(y).updated(x, value)))
    def updated(p: Vec2i)(value: A): Grid[A] = updated(p.x, p.y)(value)

    def zipWithIndices: Seq[(A, Vec2i)] =
      for
        y <- 0 until height
        x <- 0 until width
      yield (this(x, y), Vec2i(x, y))
  object Grid:
    def apply[A](values: IterableOnce[IterableOnce[A]]): Grid[A] =
      Grid(values.iterator.map(_.iterator.toVector).iterator.toVector)
  enum GridItem:
    case Empty, Wall, Box

  enum GridItemP2:
    case Empty, Wall, BoxL, BoxR

  extension (grid: Grid[GridItem])
    @targetName("updatedMoveP1")
    def updatedMove(start: Vec2i, dir: Direction): Option[Grid[GridItem]] =
      @tailrec
      def go(curGrid: Grid[GridItem], start: Vec2i): Option[Grid[GridItem]] =
        val newPos = start.offset(dir)
        val oldItem = curGrid(start)
        curGrid(newPos) match
          case GridItem.Wall  => None
          case GridItem.Empty => Some(curGrid.updated(newPos)(oldItem))
          case GridItem.Box   => go(curGrid, newPos)

      // prevent weird
      go(grid, start).map: it =>
        val newPos = start.offset(dir)
        if it.isDefinedAt(newPos) then it.updated(newPos)(GridItem.Empty)
        else it

  extension (grid: Grid[GridItemP2])
    @targetName("updatedMoveP2")
    def updatedMove(istart: Vec2i, dir: Direction): Option[Grid[GridItemP2]] =
      // ???
      def go
        (
          start: Vec2i,
          movedItems: List[(Vec2i, GridItemP2)]
        ): Eval[Option[List[(Vec2i, GridItemP2)]]] =
        val newPos = start.offset(dir)
        grid(newPos) match
          case GridItemP2.Wall  => Eval.always(None)
          case GridItemP2.Empty => Eval.now(Some(movedItems))
          case GridItemP2.BoxL  =>
            val rPos = newPos.offset(Direction.Right)
            assert(grid(rPos) == GridItemP2.BoxR)
            val newItems = movedItems.prepended(newPos, GridItemP2.BoxL)
            dir match
              case Direction.Left | Direction.Right => go(newPos, newItems)
              case Direction.Up | Direction.Down    =>
                go(newPos, newItems).flatMap:
                  case Some(it) => go(rPos, it.prepended(rPos, GridItemP2.BoxR))
                  case None     => Eval.now(None)
          case GridItemP2.BoxR =>
            val lPos = newPos.offset(Direction.Left)
            assert(grid(lPos) == GridItemP2.BoxL)
            val newItems = movedItems.prepended(newPos, GridItemP2.BoxR)
            dir match
              case Direction.Right | Direction.Left => go(newPos, newItems)
              case Direction.Up | Direction.Down    =>
                go(newPos, newItems).flatMap:
                  case Some(it) => go(lPos, it.prepended(lPos, GridItemP2.BoxL))
                  case None     => Eval.now(None)

      go(istart, List()).value.map:
        val alreadyInspected = mutable.Set[Vec2i]()
        _.foldLeft(grid.updated(istart)(GridItemP2.Empty)):
          case (g, (p, item)) =>
            if !alreadyInspected.contains(p) then
              alreadyInspected.add(p)
              g.updated(p)(GridItemP2.Empty).updated(p.offset(dir))(item)
            else g

  final case class ProblemState
    (
      grid: Grid[GridItem],
      robot: Vec2i,
      remainingMoves: List[Direction]
    ):
    def step: Option[ProblemState] =
      remainingMoves match
        case head :: rest =>
          grid.updatedMove(robot, head) match
            case Some(newGrid) =>
              Some(ProblemState(newGrid, robot.offset(head), rest))
            case _ => Some(ProblemState(grid, robot, rest))
        case _ => None

    def gpsCalc: Long =
      grid.zipWithIndices.filter(_._1 == GridItem.Box)
        .map((_, p) => p.y.toLong * 100L + p.x.toLong).sum

    def doubled: ProblemStateP2 =
      val newGrid =
        Grid(grid.values.map(_.flatMap:
          case GridItem.Box   => List(GridItemP2.BoxL, GridItemP2.BoxR)
          case GridItem.Wall  => List(GridItemP2.Wall, GridItemP2.Wall)
          case GridItem.Empty => List(GridItemP2.Empty, GridItemP2.Empty)))
      ProblemStateP2(newGrid, robot.copy(x = robot.x * 2), remainingMoves)

  final case class ProblemStateP2
    (
      grid: Grid[GridItemP2],
      robot: Vec2i,
      remainingMoves: List[Direction]
    ):
    def step: Option[ProblemStateP2] =
      remainingMoves match
        case head :: rest =>
          grid.updatedMove(robot, head) match
            case Some(newGrid) =>
              Some(ProblemStateP2(newGrid, robot.offset(head), rest))
            case _ => Some(ProblemStateP2(grid, robot, rest))
        case _ => None

    def gpsCalc: Long =
      grid.zipWithIndices.filter(_._1 == GridItemP2.BoxL)
        .map((_, p) => p.y.toLong * 100L + p.x.toLong).sum

    def pretty: String =
      (grid.values.zipWithIndex.map: (it, y) =>
        (it.zipWithIndex.map: (m, x) =>
          m match
            case _ if Vec2i(x, y) == robot => '@'
            case GridItemP2.Empty          => '.'
            case GridItemP2.Wall           => '#'
            case GridItemP2.BoxL           => '['
            case GridItemP2.BoxR           => ']'
        ).mkString("", "", ""))
        .mkString("", "\n", "")
  override def parse(str: String): ProblemState =
    val Array(gridStr, moveStr) = str.split("\n\n")
    var robotPos = Vec2i(-1, -1)
    val grid =
      Grid(gridStr.linesIterator.zipWithIndex.map: (str, y) =>
        str.toVector.zipWithIndex.map: (it, x) =>
          it match
            case '#' => GridItem.Wall
            case 'O' => GridItem.Box
            case '.' => GridItem.Empty
            case '@' =>
              assert(robotPos == Vec2i(-1, -1))
              robotPos = Vec2i(x, y)
              GridItem.Empty
            case _ => ???)
    val dirs =
      moveStr.linesIterator.flatten.map:
        case '<' => Direction.Left
        case '^' => Direction.Up
        case '>' => Direction.Right
        case 'v' => Direction.Down
        case _   => ???

    ProblemState(grid, robotPos, dirs.toList)

  override def part1(str: String): Long =
    val input = parse(str)
    Iterator.iterate(input): i =>
      i.step.getOrElse(i)
    .find: it =>
      it.remainingMoves.isEmpty
    .get.gpsCalc

  override def part2(str: String): Long =
    val input = parse(str)
    Iterator.iterate(input.doubled): i =>
      i.step.getOrElse(i)
    .find(_.remainingMoves.isEmpty).get.gpsCalc

  override lazy val input: String = FileIO.getInput(2024, 15)
