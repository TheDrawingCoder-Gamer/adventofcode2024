package gay.menkissing.advent
package y2020

import cats.*
import cats.implicits.*
import gay.menkissing.common.{*, given}

import scala.collection.mutable

object Day15 extends Problem:
  type Input = List[Int]
  type Output = Int
  def showOutput: Show[Int] = summon

  override def parse(str: String): List[Int] =
    str.split(",").map(_.toInt).toList

  def calc(input: List[Int], n: Int): Int =
    // ; )
    val arr = Array.fill[Long](n)(0L)
    input.zipWithIndex.foreach: (x, i) =>
      arr(x) = i.toLong + 1L
    def setArr(num: Int, idx: Int): Unit =
      arr(num) = (arr(num) << 32) + idx.toLong + 1L
    var lastNum = input.last
    (input.length until n).foreach: idx =>
      val n = arr(lastNum)
      if n > Int.MaxValue.toLong then
        val x = (n & Int.MaxValue.toLong) - (n >> 32)
        lastNum = x.toInt
        setArr(x.toInt, idx)
      else
        lastNum = 0
        setArr(0, idx)
    lastNum

  override def part1(input: List[Int]): Int = calc(input, 2020)

  def part2(input: List[Int]): Int = calc(input, 30000000)

  override lazy val input: String = FileIO.getInput(2020, 15)
