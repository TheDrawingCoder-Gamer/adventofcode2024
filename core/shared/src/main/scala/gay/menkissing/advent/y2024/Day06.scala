package gay.menkissing.advent.y2024

import gay.menkissing.advent.{FileIO, Problem}
import gay.menkissing.common.*

import scala.collection.mutable as mut
import scala.io.Source
import spire.implicits.IntAlgebra

object Day06 extends Problem[(Grid[Boolean], Int), Int]:
  lazy val input = FileIO.getInput(2024, 6)

  override def parse(str: String): (Grid[Boolean], Int) =
    (Grid[Boolean](str.linesIterator.map(_.map(_ == '#'))), str.linesIterator.flatten.indexWhere(_ == '^'))

  def getStarts(grid: Grid[Boolean], guardIdx: Int): (Vec2[Int], Direction2D) = (Vec2(guardIdx % grid.width, guardIdx / grid.width), Direction2D.Up)


  override def part1(input: (Grid[Boolean], Int)): Int =
    val (grid, guardIdx) = input

    
    var guardPos = Vec2(guardIdx % grid.width, guardIdx / grid.width)
  
    var guardDirection = Direction2D.Up
  
    val daPoints = mut.Set(guardPos)
  
    while (grid.isDefinedAt(guardPos.x, guardPos.y)) {
      val nextPos = guardPos.offset(guardDirection)
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

  override def part2(input: (Grid[Boolean], Int)): Int =
    val (grid, guardIdx) = input

    def testLoop(grid2: Grid[Boolean]): Boolean = {
      var (guardPos, guardDirection) = getStarts(grid, guardIdx)
    
      val freakyPoints = mut.Set[(Vec2[Int], Direction2D)]((guardPos, guardDirection))
    
      while (grid2.isDefinedAt(guardPos.x, guardPos.y)) {
        val nextPos = guardPos.offset(guardDirection)
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
    
    var guardPos = Vec2(guardIdx % grid.width, guardIdx / grid.width)
  
    var guardDirection = Direction2D.Up
  
    val daPoints = mut.Set(guardPos)
  
    // missing start point
    val goodPoints: mut.Set[(Vec2[Int], Direction2D)] = mut.Set()
  
    while (grid.isDefinedAt(guardPos.x, guardPos.y)) {
      val nextPos = guardPos.offset(guardDirection)
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
  
  
  
    val h = daPoints.collect {
      Function.unlift[Vec2[Int], Vec2[Int]] { it =>
        if (grid.isDefinedAt(it.x, it.y)) {
          if (testLoop(grid.updated(it)(true))) {
            Some(it)
          } else None
        } else None
      }
    }
    h.toSet.size
    
    
    
