package gay.menkissing.advent
package y2023

import gay.menkissing.common.*
import cats.syntax.functorFilter.*
import algebra.instances.all.*

object Day03 extends Problem:
  type Input = Grid[Char]
  type Output = Int

  lazy val input = FileIO.getInput(2023, 3)

  def parse(input: String): Grid[Char] =
    Grid(input.linesIterator.map(_.iterator))

  final case class Number(n: Int, size: Int, point: Vec2[Int])
  extension (grid: Grid[Char])
    def gears: Seq[Vec2[Int]] =
      grid.indices.mapFilter: (x, y) =>
        Option.when(grid(Vec2(x, y)) == '*')(Vec2(x, y))

    def symbols: Seq[(Char, Vec2[Int])] =
      grid.zipWithIndices.filter((c, _) => !c.isDigit && c != '.')

    def numberAt(pos: Vec2[Int]): Option[Number] =
      def go(p: Vec2[Int], goingLeft: Boolean): Option[Number] =
        grid.get(p) match
          case Some(v) if v.isDigit =>
            val r = v - '0'
            Some:
              if goingLeft then
                go(p.copy(x = p.x - 1), goingLeft) match
                  case None                     => Number(r, 1, p)
                  case Some(Number(r2, s2, p2)) =>
                    Number(r2 * 10 + r, s2 + 1, p2)
              else
                go(p.copy(x = p.x + 1), goingLeft) match
                  case None                     => Number(r, 1, p)
                  case Some(Number(r2, s2, p2)) =>
                    Number(r * math.pow(10, s2).toInt + r2, s2 + 1, p)
          case _ => None
      val left = go(pos.copy(x = pos.x - 1), true)
      val right = go(pos, false)
      left match
        case None                     => right
        case Some(Number(lv, ls, lp)) =>
          right.map: r =>
            Number(lv * math.pow(10, r.size).toInt + r.n, ls + r.size, lp)

  def part1(input: Grid[Char]): Int =
    input.symbols.flatMap: (_, p) =>
      p.allNeighbors.flatMap(input.numberAt)
    .distinctBy(_.point).foldLeft(0):
      case (acc, Number(n, _, _)) => acc + n

  def part2(input: Grid[Char]): Int =
    input.gears.flatMap: p =>
      p.allNeighbors.flatMap(input.numberAt).distinct match
        case List(x, y) => Some(x.n * y.n)
        case _          => None
    .sum
