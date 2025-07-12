package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import gay.menkissing.common.Sys3D.*

import scala.annotation.tailrec
import scala.collection.mutable as mut
import scala.io.Source


// This code was mostly written in 2022, and ported to fit this new repo's model
object Day19 extends Problem[List[Set[Vec3i]], Int]{
  def findTransformIfIntersects(left : Set[Vec3i], right : Set[Vec3i]) : Option[Scanner] = {
    Direction3D.values.collectFirst(Function.unlift {(facing : Direction3D) =>
      Rotation.values.collectFirst (Function.unlift { (rot : Rotation) =>
        // get causes an exception so it's partial
        val rightReoriented = right.map(_.face(facing).orient(rot))
        left.collectFirst(Function.unlift { s1 =>
          rightReoriented.collectFirst(Function.unlift { s2 =>
            val difference = s1 - s2
            val moved = rightReoriented.map(_ + difference)
            if ((moved intersect left).size >= 12)
              Some(Scanner(difference, moved))
            else
              None
          })
        })
      })
    })
  }
  def parse(input: String): List[Set[Vec3i]] = {
    input.split("\n\n").map { scanner => 
      scanner.linesIterator
      .drop(1)
      .map(Vec3i.of)
       .toSet
    }.toList
  }
  def solve(data: List[Set[Vec3i]]): Solution = {
    val baseSector = mut.Set.from(data.head)
    val foundScanners = mut.Set(Vec3i(0,0,0))
    val unmappedSector = mut.ListBuffer().addAll(data.tail)
    while (unmappedSector.nonEmpty) {
      val thisSector = unmappedSector.head 
      unmappedSector.remove(0)
      findTransformIfIntersects(baseSector.toSet, thisSector) match {
        case None => unmappedSector.addOne(thisSector)
        case Some(value) => 
          baseSector ++= value.detections
          foundScanners += value.pos
      }
    }
    Solution(foundScanners.toSet, baseSector.toSet)
  }
  def part1(input: List[Set[Vec3i]]): Int = {
    val solved = solve(input)
    solved.beacons.size 
  }
  def part2(input: List[Set[Vec3i]]): Int = {
    val solved = solve(input)
    val beegList = solved.scanners.toList.combinations(2).map { case l :: (r :: _) => 
      l.manhattanDistance(r)
    }
    beegList.max
  }


  lazy val input = FileIO.getInput(2021, 19)
  case class Scanner(pos: Vec3i, detections: Set[Vec3i])

  case class Solution(scanners: Set[Vec3i], beacons: Set[Vec3i])
}


