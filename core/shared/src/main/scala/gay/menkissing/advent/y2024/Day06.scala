package gay.menkissing.advent
package y2024

import gay.menkissing.common.*

import scala.collection.mutable
import algebra.instances.all.*

object Day06 extends Problem:
  type Input = (Grid[Boolean], Int)
  type Output = Int

  lazy val input = FileIO.getInput(2024, 6)

  override def parse(str: String): (Grid[Boolean], Int) =
    (
      Grid[Boolean](str.linesIterator.map(_.map(_ == '#'))),
      str.linesIterator.flatten.indexWhere(_ == '^')
    )

  def getStarts(grid: Grid[Boolean], guardIdx: Int): (Vec2[Int], Direction2D) =
    (Vec2(guardIdx % grid.width, guardIdx / grid.width), Direction2D.Up)

  override def part1(input: (Grid[Boolean], Int)): Int =
    val (grid, guardIdx) = input

    var guardPos = Vec2(guardIdx % grid.width, guardIdx / grid.width)

    var guardDirection = Direction2D.Up

    val daPoints = mutable.Set(guardPos)

    while grid.isDefinedAt(guardPos.x, guardPos.y) do
      val nextPos = guardPos.offset(guardDirection)
      grid.get(nextPos) match
        case Some(it) =>
          if it then guardDirection = guardDirection.clockwise
          else
            guardPos = nextPos
            daPoints.add(guardPos)
        case None => guardPos = nextPos

    daPoints.size

  override def part2(input: (Grid[Boolean], Int)): Int =
    val (grid, guardIdx) = input

    def testLoop(grid2: Grid[Boolean]): Boolean =
      var (guardPos, guardDirection) = getStarts(grid, guardIdx)

      val freakyPoints =
        mutable.Set[(Vec2[Int], Direction2D)]((guardPos, guardDirection))

      while grid2.isDefinedAt(guardPos.x, guardPos.y) do
        val nextPos = guardPos.offset(guardDirection)
        if grid2.get(nextPos).exists(identity) then
          guardDirection = guardDirection.clockwise
        else guardPos = nextPos

        if freakyPoints.contains((guardPos, guardDirection)) then return true
        freakyPoints.add((guardPos, guardDirection))

      false

    var guardPos = Vec2(guardIdx % grid.width, guardIdx / grid.width)

    var guardDirection = Direction2D.Up

    val daPoints = mutable.Set(guardPos)

    // missing start point
    val goodPoints: mutable.Set[(Vec2[Int], Direction2D)] = mutable.Set()

    while grid.isDefinedAt(guardPos.x, guardPos.y) do
      val nextPos = guardPos.offset(guardDirection)
      grid.get(nextPos) match
        case Some(it) =>
          if it then guardDirection = guardDirection.clockwise
          else
            guardPos = nextPos
            daPoints.add(guardPos)
            goodPoints.add((guardPos, guardDirection))
        case None => guardPos = nextPos

    val h =
      daPoints.flatMap: it =>
        if grid.isDefinedAt(it.x, it.y) then
          Option.when(testLoop(grid.updated(it)(true)))(it)
        else None

    h.toSet.size
