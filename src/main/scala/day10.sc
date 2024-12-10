import scala.io.Source
import gay.menkissing.common.*
import scala.collection.parallel.CollectionConverters.*


val data = Grid[Int](Source.fromResource("day10.txt").getLines.map(_.map(_.asDigit)))

def neighbors(grid: Grid[Int], pos: Vec2i): Seq[Vec2i] = {
  Direction2D.values.flatMap { dir =>
    val daPos = pos.offset(dir)
    Option.when(grid.isDefinedAt(daPos.x, daPos.y))(daPos)
  }.filter(it => grid(it) - grid(pos) == 1)
}

def scoreTrail(start: Vec2i, endNodes: Seq[Vec2i]): Int = {
  endNodes.par.collect(Function.unlift { it =>
    astar[Vec2i](start, it, c => c.taxiDistance(it), (l, r) => if (data(r) - data(l) == 1) 1.0 else Double.PositiveInfinity, i => neighbors(data, i))
  }).size
}

def rateTrailhead(start: Vec2i, endNodes: Seq[Vec2i]): Int = {
  def rateSingleTrail(e: Vec2i): Int = {
    findAllPaths[Vec2i](start, e, it => neighbors(data, it)).size
  }
  endNodes.par.map(rateSingleTrail).sum
}

def parseZeroNines: (Seq[Vec2i], Seq[Vec2i]) =
  (data.zipWithIndices.iterator.withFilter(_._1 == 0).map(_._2).toSeq, data.zipWithIndices.withFilter(_._1 == 9).map(_._2).toSeq)

def part1: Int = {
  val (zeros, nines) = parseZeroNines

  zeros.par.map(scoreTrail(_, nines)).sum
}

def part2: Int = {
  val (zeros, nines) = parseZeroNines

  zeros.par.map(rateTrailhead(_, nines)).sum
}

debugTiming {
  part1
}

debugTiming {
  part2
}