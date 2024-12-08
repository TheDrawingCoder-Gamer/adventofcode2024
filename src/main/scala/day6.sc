import scala.io.Source
import gay.menkissing.common.*
import scala.collection.mutable as mut
import cats.effect.*
import cats.implicits.*
import cats.effect.unsafe.implicits.*

import GridAxisSystem.*

val data = Source.fromResource("day6.txt").getLines.toVector

val grid = Grid[Boolean](data.map(_.map(_ == '#')))

val guardIdx = data.flatten.indexWhere { it =>
  it == '^'
}



def getStarts: (Vec2i, Direction2D) = (Vec2i(guardIdx % grid.width, guardIdx / grid.width), Direction2D.Up)




def calculatePart1: Int = {
  var guardPos = Vec2i(guardIdx % grid.width, guardIdx / grid.width)

  var guardDirection = Direction2D.Up

  val daPoints = mut.Set(guardPos)

  while (grid.isDefinedAt(guardPos.x, guardPos.y)) {
    val nextPos = guardPos.genOffset(guardDirection)
    grid.get(nextPos) match {
      case Some(it) =>
        if (it) {
          guardDirection = guardDirection.clockwise
        } else {
          guardPos = nextPos
          daPoints.add(guardPos)
        }
      case None => guardPos = nextPos
    }
  }

  daPoints.size
}

def testLoop(grid2: Grid[Boolean]): Boolean = {
  var (guardPos, guardDirection) = getStarts

  val freakyPoints = mut.Set[(Vec2i, Direction2D)]((guardPos, guardDirection))

  while (grid2.isDefinedAt(guardPos.x, guardPos.y)) {
    val nextPos = guardPos.genOffset(guardDirection)
    if (grid2.get(nextPos).exists(identity)) {
      guardDirection = guardDirection.clockwise
    }  else {
      guardPos = nextPos
    }

    if (freakyPoints.contains((guardPos, guardDirection))) {
      return true
    }
    freakyPoints.add((guardPos, guardDirection))
  }

  false
}

def part2(): Int = {
  var guardPos = Vec2i(guardIdx % grid.width, guardIdx / grid.width)

  var guardDirection = Direction2D.Up

  val daPoints = mut.Set(guardPos)

  // missing start point
  val goodPoints: mut.Set[(Vec2i, Direction2D)] = mut.Set()

  while (grid.isDefinedAt(guardPos.x, guardPos.y)) {
    val nextPos = guardPos.genOffset(guardDirection)
    grid.get(nextPos) match {
      case Some(it) =>
        if (it) {
          guardDirection = guardDirection.clockwise
        } else {
          guardPos = nextPos
          daPoints.add(guardPos)
          goodPoints.add((guardPos, guardDirection))
        }
      case None => guardPos = nextPos
    }
  }



  val gyatPoints = mut.Set.empty[Vec2i]

  daPoints.foreach { it =>
    if (grid.isDefinedAt(it.x, it.y)) {
      Direction2D.values.foreach { dir =>
        val obstacle = it.genOffset(dir)
        if (grid.isDefinedAt(obstacle.x, obstacle.y)) {
          if (!gyatPoints.contains(obstacle) && testLoop(grid.updated(obstacle)(true))) {
            gyatPoints.add(obstacle)
          }
        }
      }
    }
  }
  gyatPoints.size


}

// calculatePart1
part2()
