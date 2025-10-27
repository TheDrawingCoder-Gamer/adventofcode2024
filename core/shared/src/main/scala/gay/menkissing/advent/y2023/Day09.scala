package gay.menkissing.advent
package y2023

import gay.menkissing.common.ArityN.*
import cats.*
import cats.syntax.all.*

object Day09 extends Problem[List[List[Int]], Int]:
  lazy val input = FileIO.getInput(2023, 9)

  def parse(str: String): List[List[Int]] =
    str.linesIterator.map(_.trim.split(raw"\s+").map(_.toInt).toList).toList


  def diffSeq(values: List[Int]): List[Int] =
    values.sliding2.map((l, r) => r - l)

  // doesn't include the all 0s one
  def freakySequence(values: List[Int]): LazyList[List[Int]] =
    LazyList.iterate(values)(diffSeq).takeWhile(_.exists(_ != 0))

  def calc(values: List[Int]): Int =
    // REVERSE TUAH, O(1) THAT THANG!!!
    val ls = freakySequence(values.reverse).map(_.head)
    ls.reverse.foldLeft(0): (n, x) =>
      x + n

  def calcLeft(values: List[Int]): Int =
    freakySequence(values).map(_.head)
      .reverse.foldLeft(0): (n, x) =>
        x - n

  def part1(input: List[List[Int]]): Int =
    val rs = input.map(calc)
    rs.sum

  // lmao, was part 2 supposed to be hard?
  def part2(input: List[List[Int]]): Int =
    input.map(calcLeft).sum