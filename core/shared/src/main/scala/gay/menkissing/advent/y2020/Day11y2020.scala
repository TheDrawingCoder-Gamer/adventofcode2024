package gay.menkissing.advent
package y2020

import gay.menkissing.common.{Grid, Vec2i, PrincibleWind2D, given}
import cats.*
import cats.implicits.*

import scala.collection.mutable

object Day11y2020 extends Problem[Grid[Day11y2020.Seat], Int]:
  enum Seat:
    case Floor, Occupied, Empty

    def isSeat: Boolean = this != Floor
  given Show[Seat] = {
    case Seat.Floor => "."
    case Seat.Occupied => "#"
    case Seat.Empty => "L"
  }

  override def parse(str: String): Grid[Seat] =
    Grid(str.linesIterator.map(_.map:
      case '.' => Seat.Floor
      case 'L' => Seat.Empty
      case '#' => Seat.Occupied
    ))





  extension (grid: Grid[Seat])
    def lineIsOccupied(start: Vec2i)(dir: PrincibleWind2D): Boolean =
      var s = start + dir.digitalDir
      while grid.isDefinedAt(s.x, s.y) do
        val r = grid(s.x, s.y)
        if r.isSeat then
          return r == Seat.Occupied
        s += dir.digitalDir
      false
    def next(): Option[Grid[Seat]] = {
      var updated = false
      val res = grid.mapWithIndex: (index, x) =>
        val nOccupied = grid.valuesAround(Seat.Empty)(index.x, index.y).updated(1,1)(Seat.Floor).foldLeft(0)((a, s) => a + (if s == Seat.Occupied then 1 else 0))
        x match
          case Seat.Empty if nOccupied == 0 =>
            updated = true
            Seat.Occupied
          case Seat.Occupied if nOccupied >= 4 =>
            updated = true
            Seat.Empty
          case y => y
      Option.when(updated)(res)
    }
    def nextP2(): Option[Grid[Seat]] = {
      var updated = false
      val res = grid.mapWithIndex: (index, x) =>
        val nOccupied = PrincibleWind2D.values.count(grid.lineIsOccupied(index))
        x match
          case Seat.Empty if nOccupied == 0 =>
            updated = true
            Seat.Occupied
          case Seat.Occupied if nOccupied >= 5 =>
            updated = true
            Seat.Empty
          case y => y
      Option.when(updated)(res)
    }



  override def part1(input: Grid[Seat]): Int = {
    Iterator.unfold(input): x =>
      x.next().map(y => (y,y))
    .toList.last.flatten.map:
      case Seat.Occupied => 1
      case _ => 0
    .sum
  }


  def part2(input: Grid[Seat]): Int =
    Iterator.unfold(input): x =>
      x.nextP2().map(y => (y,y))
    .toList.last.flatten.map:
      case Seat.Occupied => 1
      case _ => 0
    .sum



  override lazy val input: String = FileIO.getInput(2020, 11)

