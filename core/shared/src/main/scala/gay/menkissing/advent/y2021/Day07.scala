package gay.menkissing.advent
package y2021

import gay.menkissing.common.IntSequences
import cats.Show

object Day07 extends Problem:
  type Input = List[Int]
  type Output = Int

  lazy val input = FileIO.getInput(2021, 7)

  def parse(input: String): List[Int] =
    input.trim.split(",").map(_.toInt).toList

  def median(input: List[Int]): Int =
    val len = input.length
    val l = len.toDouble / 2.0
    val ll = l.floor.toInt
    val lu = l.ceil.toInt
    val sorted = input.sorted
    if ll == lu then sorted(ll)
    else math.max(sorted(ll), sorted(lu))

  def average(ls: List[Int]): Double = ls.sum.toDouble / ls.length.toDouble

  def sumDistance(crabs: List[Int], pt: Int): Int =
    crabs.map(it => math.abs(it - pt)).sum

  def part1(input: List[Int]): Int = sumDistance(input, median(input))

  def sumtorial(p: Int): Int = IntSequences.triangleNumber(p).toInt

  def distanceP2(crabs: List[Int], pt: Int): Int =
    crabs.map(it => sumtorial(math.abs(it - pt))).sum

  def part2(input: List[Int]): Int =
    val pt = average(input)
    // We don't know what the best out of the two is so lets just test both
    distanceP2(input, pt.floor.toInt) min distanceP2(input, pt.ceil.toInt)
