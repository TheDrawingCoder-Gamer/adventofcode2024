package gay.menkissing.advent
package y2023

import gay.menkissing.common.*
import cats.syntax.functorFilter.*

object Day03y2023 extends Problem[Grid[Char], Int]:

  lazy val input = FileIO.getInput(2023, 3)

  def parse(input: String): Grid[Char] =
    Grid(input.linesIterator.map(_.iterator))


  case class Number(n: Int, size: Int, point: Vec2i)
  extension (grid: Grid[Char])
    def gears: Seq[Vec2i] =
      grid.indices.mapFilter: (x, y) =>
        Option.when(grid(Vec2i(x, y)) == '*')(Vec2i(x, y))

    def symbols: Seq[(Char, Vec2i)] =
      grid.zipWithIndices.filter((c, _) => !c.isDigit && c != '.')

    def numberAt(pos: Vec2i): Option[Number] =
      def go(p: Vec2i, goingLeft: Boolean): Option[Number] =
        grid.get(p) match
          case Some(v) if v.isDigit =>
            val r = v - '0'
            Some:
              if goingLeft then
                go(p.copy(x = p.x - 1), goingLeft) match
                  case None => Number(r, 1, p)
                  case Some(Number(r2, s2, p2)) => Number(r2 * 10 + r, s2 + 1, p2)
              else
                go(p.copy(x = p.x + 1), goingLeft) match
                  case None => Number(r, 1, p)
                  case Some(Number(r2, s2, p2)) => Number(r * math.pow(10, s2).toInt + r2, s2 + 1, p)
          case _ => None
      val left = go(pos.copy(x = pos.x - 1), true)
      val right = go(pos, false)
      right.map { r =>
        left match
          case None => r
          case Some(Number(lv, ls, lp)) => Number(lv * math.pow(10, r.size).toInt + r.n, ls + r.size, lp)
      }

  def part1(input: Grid[Char]): Int =
    input.symbols.flatMap: (_, p) =>
      p.allNeighbors.flatMap(input.numberAt)
    .distinctBy(_.point).foldLeft(0):
      case (acc, Number(n, _, _)) =>
        acc + n


  def part2(input: Grid[Char]): Int =
    input.gears.flatMap: p =>
      p.allNeighbors.flatMap(input.numberAt).distinct.toList match
        case List(x, y) => Some(x.n * y.n)
        case _ => None
    .sum