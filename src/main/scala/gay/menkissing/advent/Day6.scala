package gay.menkissing.advent

import gay.menkissing.advent.Problem
import gay.menkissing.common.*
import gay.menkissing.common.GridAxisSystem.*

import scala.collection.mutable as mut
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.ParSet
import scala.collection.parallel.mutable.ParSet as MParSet
import scala.io.Source

object Day6 extends Problem[(Grid[Boolean], Int), Int]:
  val input = Source.fromResource("day6.txt").mkString

  override def parse(str: String): (Grid[Boolean], Int) =
    (Grid[Boolean](str.linesIterator.map(_.map(_ == '#'))), str.linesIterator.flatten.indexWhere(_ == '^'))

  def getStarts(grid: Grid[Boolean], guardIdx: Int): (Vec2i, Direction2D) = (Vec2i(guardIdx % grid.width, guardIdx / grid.width), Direction2D.Up)


  override def part1(input: (Grid[Boolean], Int)): Int =
    val (grid, guardIdx) = input

    
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

  override def part2(input: (Grid[Boolean], Int)): Int =
    val (grid, guardIdx) = input

    def testLoop(grid2: Grid[Boolean]): Boolean = {
      var (guardPos, guardDirection) = getStarts(grid, guardIdx)
    
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
  
  
  
    val h = daPoints.collect {
      Function.unlift[Vec2i, Vec2i] { it =>
        if (grid.isDefinedAt(it.x, it.y)) {
          if (testLoop(grid.updated(it)(true))) {
            Some(it)
          } else None
        } else None
      }
    }
    h.toSet.size
    
    
    
