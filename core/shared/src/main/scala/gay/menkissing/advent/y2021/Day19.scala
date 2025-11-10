package gay.menkissing.advent
package y2021

import gay.menkissing.common.*, ArityN.*, Sys3D.*, algebras.given

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

import cats.syntax.all.*
import alleycats.std.set.*

// This code was mostly written in 2022, and ported to fit this new repo's model
object Day19 extends Problem:
  type Input = List[Set[Vec3[Int]]]
  type Output = Int

  def findTransformIfIntersects
    (
      left: Set[Vec3[Int]],
      right: Set[Vec3[Int]]
    ): Option[Scanner] =
    Direction3D.values.collectFirstSome: facing =>
      Rotation.values.collectFirstSome: rot =>
        // get causes an exception so it's partial
        val rightReoriented = right.map(_.face(facing).orient(rot))
        left.collectFirstSome: s1 =>
          rightReoriented.collectFirstSome: s2 =>
            val difference = s1 - s2
            val moved = rightReoriented.map(_ + difference)
            Option.when((moved intersect left).size >= 12):
              Scanner(difference, moved)
  def parse(input: String): List[Set[Vec3[Int]]] =
    input.split("\n\n").map: scanner =>
      scanner.linesIterator.drop(1).map(Vec3i.of).toSet
    .toList
  def solve(data: List[Set[Vec3[Int]]]): Solution =
    val baseSector = mutable.Set.from(data.head)
    val foundScanners = mutable.Set(Vec3(0, 0, 0))
    val unmappedSector = mutable.ListBuffer.from(data.tail)
    while unmappedSector.nonEmpty do
      val thisSector = unmappedSector.head
      unmappedSector.remove(0)
      findTransformIfIntersects(baseSector.toSet, thisSector) match
        case None        => unmappedSector.addOne(thisSector)
        case Some(value) =>
          baseSector ++= value.detections
          foundScanners += value.pos
    Solution(foundScanners.toSet, baseSector.toSet)
  def part1(input: List[Set[Vec3[Int]]]): Int =
    val solved = solve(input)
    solved.beacons.size
  def part2(input: List[Set[Vec3[Int]]]): Int =
    val solved = solve(input)
    val beegList =
      solved.scanners.toList.combinationsN[2].map(_ `taxiDistance` _)
    beegList.max

  lazy val input = FileIO.getInput(2021, 19)
  case class Scanner(pos: Vec3[Int], detections: Set[Vec3[Int]])

  case class Solution(scanners: Set[Vec3[Int]], beacons: Set[Vec3[Int]])
