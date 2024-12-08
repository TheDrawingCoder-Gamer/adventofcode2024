import scala.io.Source
import gay.menkissing.common.*
import GridAxisSystem.*

import scala.collection.mutable.ListBuffer

val grid = Grid[Char](Source.fromResource("day8.txt").getLines.map(_.iterator))

def calculateAntiNodesFor(c: Char, calcForPoints: (Vec2i, Vec2i) => List[Vec2i]): Set[Vec2i] = {
  val antennaPoints = grid.indices.iterator.filter { case (x, y) =>
    grid(x, y) == c
  }.iterator.toSeq

  antennaPoints.combinations(2).flatMap { case Seq((x1, y1), (x2, y2)) =>
    calcForPoints(Vec2i(x1, y1), Vec2i(x2, y2))
  }.toSet
}

def calculateAntiNodeForPoints(l: Vec2i, r: Vec2i): List[Vec2i] = {
  val run = (r.x - l.x)
  val rise = (r.y - l.y)
  val node1 = Vec2i(r.x + run, r.y + rise)
  val node2 = Vec2i(l.x - run, l.y - rise)
  List(node1, node2).filter(it => grid.isDefinedAt(it.x, it.y))
}

def rawCalculateP2AntiNodes(l: Vec2i, r: Vec2i): List[Vec2i] = {
  val run = (r.x - l.x)
  val rise = (r.y - l.y)
  List.unfold[Vec2i, Vec2i](Vec2i(r.x, r.y)) { vec =>
    val newP = Vec2i(vec.x + run, vec.y + rise)
    Option.when(grid.isDefinedAt(newP.x, newP.y))((newP, newP))
  }
}

def calculateAntiNodesForPointsP2(l: Vec2i, r: Vec2i): List[Vec2i] = {
  val lowerNodes = rawCalculateP2AntiNodes(r, l)
  val higherNodes = rawCalculateP2AntiNodes(l, r)
  lowerNodes ++ higherNodes ++ List(l, r)
}

val uniqueChars = grid.flatten.toSet - '.'

uniqueChars.map(it => calculateAntiNodesFor(it, calculateAntiNodesForPointsP2)).reduce(_ ++ _).size
