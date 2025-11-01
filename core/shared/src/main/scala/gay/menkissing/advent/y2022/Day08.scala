package gay.menkissing.advent
package y2022

import gay.menkissing.common.*

import scala.io.Source
import scala.math.Ordering

object Day08 extends Problem[Grid[Int], Int]:
  lazy val input = FileIO.getInput(2022, 8)

  override def parse(str: String): Grid[Int] =
    Grid:
      str.linesIterator.map:
        _.map(_.toInt - '0')

  def isVisible(grid: Grid[Int])(x: Int, y: Int): Boolean =
    val v = grid(x, y)
    val isEdge =
      (x == 0 || x == grid.width - 1) || (y == 0 || y == grid.height - 1)
    lazy val right = grid.rows(y).drop(x + 1).forall(_ < v)
    lazy val left = grid.rows(y).take(x).forall(_ < v)
    lazy val col = grid.columns(x)
    lazy val up = col.take(y).forall(_ < v)
    lazy val down = col.drop(y + 1).forall(_ < v)
    // short circut safety
    isEdge || right || left || up || down
  def zeroAsOne(i: Int): Int =
    i match
      case 0 => 1
      case _ => i
  def views(grid: Grid[Int])(x: Int, y: Int): Int =
    val v = grid(x, y)
    val ledge = x == 0
    val redge = x == grid.width - 1
    val uedge = y == 0
    val dedge = y == grid.height - 1
    val isEdge = ledge || redge || uedge || dedge
    if isEdge then 0
    else
      def edgeView(part: Seq[Int]): Int =
        val r = part.countWhile(_ < v)
        if part.lengthIs == r then r
        else r + 1
      val row = grid.rows(y)
      val right = edgeView(row.drop(x + 1))
      val left = edgeView(row.take(x).reverse)
      val col = grid.columns(x)
      val up = edgeView(col.take(y).reverse)
      val down = edgeView(col.drop(y + 1))
      right * left * up * down

  override def part1(grid: Grid[Int]): Int =
    grid.indices.count:
      case (x, y) => isVisible(grid)(x, y)

  override def part2(grid: Grid[Int]): Int =
    grid.indices.map:
      case (x, y) => views(grid)(x, y)
    .max
