package gay.menkissing.advent
package y2024

import gay.menkissing.common.*, algebras.given

object Day10 extends Problem:
  type Input = Grid[Int]
  type Output = Int

  lazy val input = FileIO.getInput(2024, 10)

  override def parse(str: String): Grid[Int] =
    Grid[Int](str.linesIterator.map(_.map(_.asDigit)))

  def neighbors(grid: Grid[Int], pos: Vec2[Int]): Seq[Vec2[Int]] =
    Direction2D.values.flatMap: dir =>
      val daPos = pos.offset(dir)
      Option.when(grid.isDefinedAt(daPos.x, daPos.y))(daPos)
    .filter(it => grid(it) - grid(pos) == 1)

  def scoreTrail
    (
      grid: Grid[Int],
      start: Vec2[Int],
      endNodes: Seq[Vec2[Int]]
    ): Int =
    endNodes.flatMap: it =>
      astarByReturning[Vec2[Int], Unit](
        start,
        _ == it,
        _.taxiDistance(it),
        (l, r) =>
          if grid(r) - grid(l) == 1 then 1.0 else Double.PositiveInfinity,
        i => neighbors(grid, i),
        (_, _, _) => ()
      )
    .size

  def rateTrailhead
    (
      grid: Grid[Int],
      start: Vec2[Int],
      endNodes: Seq[Vec2[Int]]
    ): Int =
    def rateSingleTrail(e: Vec2[Int]): Int =
      findAllPaths[Vec2[Int]](start, e, it => neighbors(grid, it)).size

    endNodes.map(rateSingleTrail).sum

  def parseZeroNines(data: Grid[Int]): (Seq[Vec2[Int]], Seq[Vec2[Int]]) =
    (
      data.zipWithIndices.iterator.withFilter(_._1 == 0).map(_._2).toSeq,
      data.zipWithIndices.withFilter(_._1 == 9).map(_._2).toSeq
    )

  def part1(grid: Grid[Int]): Int =
    val (zeros, nines) = parseZeroNines(grid)

    zeros.map(scoreTrail(grid, _, nines)).sum

  def part2(grid: Grid[Int]): Int =
    val (zeros, nines) = parseZeroNines(grid)

    zeros.map(rateTrailhead(grid, _, nines)).sum
