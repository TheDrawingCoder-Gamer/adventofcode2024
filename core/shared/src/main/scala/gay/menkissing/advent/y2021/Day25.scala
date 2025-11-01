package gay.menkissing.advent
package y2021

import gay.menkissing.common.*, ArityN.*
import cats.syntax.all.*
import cats.*
import spire.implicits.IntAlgebra

object Day25 extends HalfDay:
  type Input = Grid[Option[Direction2D]]
  type Output = Int
  def showOutput: Show[Int] = summon

  lazy val input = FileIO.getInput(2021, 25)

  def parse(str: String): Grid[Option[Direction2D]] =
    Grid.fromString(str):
      case '>' => Some(Direction2D.Right)
      case 'v' => Some(Direction2D.Down)
      case '.' => None
      case _   => whatTheScallop.!

  extension (self: Grid[?])
    def wrapVec2i(p: Vec2[Int]): Vec2[Int] =
      Vec2(p.x rem self.width, p.y rem self.height)

  def step(grid: Grid[Option[Direction2D]]): Grid[Option[Direction2D]] =
    def stepInner
      (
        dir: Direction2D
      )
      (cur: Grid[Option[Direction2D]]): Grid[Option[Direction2D]] =
      cur.mapWithIndex: (idx, v) =>
        v match
          case Some(x) if x == dir =>
            Option.when(cur(grid.wrapVec2i(idx.offset(dir))).isDefined)(dir)
          case None =>
            Option.when(
              cur(grid.wrapVec2i(idx.offset(dir.reverse))).contains(dir)
            )(dir)
          case _ => v
    stepInner(Direction2D.Right).andThen(stepInner(Direction2D.Down))(grid)

  val showDirectionChar: Show[Direction2D] =
    _ match
      case Direction2D.Up    => "^"
      case Direction2D.Down  => "V"
      case Direction2D.Left  => "<"
      case Direction2D.Right => ">"

  val showDirGrid: Show[Grid[Option[Direction2D]]] =
    it =>
      Grid.gridShow[String]
        .show(it.map(_.map(showDirectionChar.show).getOrElse(".")))
  def part1(input: Grid[Option[Direction2D]]): Int =
    Iterator.iterate(input)(step).sliding(2)
      .indexWhere(it => it.head === it.tail.head) + 1
