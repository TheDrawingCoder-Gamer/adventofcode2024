package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import algebra.instances.all.*

object Day23 extends Problem:
  type Input = ElfGrid
  type Output = Int

  type ElfGrid = Grid[Boolean]

  final case class Elf(current: Vec2[Int], proposal: Option[Vec2[Int]])

  val order: Vector[Direction2D] =
    Vector(
      Direction2D.Up,
      Direction2D.Down,
      Direction2D.Left,
      Direction2D.Right
    )

  def propose
    (
      grid: ElfGrid,
      moveOffset: Int,
      pos: Vec2[Int]
    ): (Vec2[Int], Vec2[Int]) =
    if !grid(pos) then return pos -> pos
    if !pos.allNeighbors.exists(p => grid.getOrElse(p, false)) then
      return pos -> pos

    val noe = grid.getOrElse(Vec2(pos.x + 1, pos.y - 1), false)
    val n = grid.getOrElse(pos.copy(y = pos.y - 1), false)
    val nw = grid.getOrElse(Vec2(pos.x - 1, pos.y - 1), false)
    val sw = grid.getOrElse(Vec2(pos.x - 1, pos.y + 1), false)
    val se = grid.getOrElse(Vec2(pos.x + 1, pos.y + 1), false)
    val s = grid.getOrElse(pos.copy(y = pos.y + 1), false)
    val e = grid.getOrElse(pos.copy(x = pos.x + 1), false)
    val w = grid.getOrElse(pos.copy(x = pos.x - 1), false)
    val northValid = !noe && !n && !nw
    val southValid = !se && !s && !sw
    val eastValid = !se && !e && !noe
    val westValid = !sw && !w && !nw
    (0 until 4).map(order.access(moveOffset)).find: it =>
      it match
        case Direction2D.Up    => northValid
        case Direction2D.Down  => southValid
        case Direction2D.Left  => westValid
        case Direction2D.Right => eastValid
    .map(i => pos -> pos.offset(i)).getOrElse(pos -> pos)
  def round(grid: ElfGrid, moveOffset: Int): (ElfGrid, Boolean) =
    val elfs =
      for
        y <- 0 until grid.height
        x <- 0 until grid.width if grid(x, y)
      yield propose(grid, moveOffset, Vec2(x, y))

    val daProposals = elfs.filter(_ != _).groupBy(_._2)
    val goodPositions =
      daProposals.filter(_._2.lengthCompare(1) == 0).map((_, y) => y.head)
    if goodPositions.isEmpty then (grid, true)
    else
      var daGrid = grid.expand(false)(1)
      goodPositions.foreach:
        case (from, to) =>
          daGrid =
            daGrid.updated(from + Vec2(1, 1))(false)
              .updated(to + Vec2(1, 1))(true)

      (daGrid, false)
  def parse(input: String): ElfGrid = Grid.fromString(input)(_ == '#')
  extension (elfGrid: ElfGrid)
    def sliced: Grid[Boolean] =
      val rows = elfGrid.rows
      val cols = elfGrid.columns
      val topBound = rows.indexWhere(_.exists(identity))
      val botBound = rows.lastIndexWhere(_.exists(identity))
      val rightBound = cols.lastIndexWhere(_.exists(identity))
      val leftBound = cols.indexWhere(_.exists(identity))

      // println(prettyShowBoolGrid(resGrid))
      elfGrid.slice(Vec2(leftBound, topBound), Vec2(rightBound, botBound))

  def part1(grid: ElfGrid): Int =
    // println(prettyShowBoolGrid(grid))

    val (resGrid, _) = (0 until 10).foldLeft((grid, 0)):
      case ((grid, offset), _) =>
        val (newGrid, _) = round(grid, offset)
        // println(prettyShowBoolGrid(newGrid))
        (newGrid, (offset + 1) % 4)

    resGrid.sliced.flatten.count(!_)

  extension (order: Vector[Direction2D])
    def access(offset: Int)(idx: Int): Direction2D = order((idx + offset) % 4)

  // todo: takes 25s???
  def part2(grid: ElfGrid): Int =
    Iterator.iterate((grid, 0, false)): (grid, offset, _) =>
      val (newGrid, didntMove) = round(grid, offset)
      (newGrid, (offset + 1) % 4, didntMove)
    .indexWhere(_._3)

  def prettyShowBoolGrid(grid: Grid[Boolean]): String =
    grid.rows.map: it =>
      it.map(if _ then '#' else '.').mkString
    .foldLeft("")(_ + "\n" + _)

  def input: String = FileIO.getInput(2022, 23)
