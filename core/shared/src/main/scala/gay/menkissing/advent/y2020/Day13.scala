package gay.menkissing.advent
package y2020

import cats.*
import cats.data.State
import cats.implicits.*
import gay.menkissing.common.{*, given}

import scala.collection.mutable

object Day13 extends Problem:
  type Input = (Int, List[Option[Int]])
  type Output = BigInt
  def showOutput: Show[BigInt] = summon

  override def parse(str: String): (Int, List[Option[Int]]) =
    str.linesIterator.toList match
      case List(s, xs) => (s.toInt, xs.split(",").map(_.toIntOption).toList)
      case _           => ???

  def leastMultipleGreaterThan(threshold: Int, multiplier: Int): Int =
    Iterator.iterate(multiplier)(_ + multiplier).find(threshold < _).get

  override def part1(input: (Int, List[Option[Int]])): BigInt =
    val (start, ids) = input
    val normalIds = ids.flatten
    val res =
      normalIds.zip(normalIds.map(it => leastMultipleGreaterThan(start, it)))
        .minBy(_._2)
    BigInt(res._1 * (res._2 - start))

  // woke comes to scala...
  // this actually stands for chinese remainder theroem. thanks past me for not
  // documenting that...
  def crt(ls: List[(Int, Int)]): BigInt =
    val (is, ms) = ls.unzip
    val m = ms.map(i => BigInt(i)).product
    val mn = ms.map(m / _)
    val yn =
      ms.zip(mn).map:
        case (x, m) => m.modInverse(BigInt(x))
    (is, mn, yn).parMapN((i, m, y) => m * i * y).sum.mod(m)

  def part2(input: (Int, List[Option[Int]])): BigInt =
    val (_, ids) = input
    val offsets =
      ids.zipWithIndex.collect:
        // index is how we ge
        case (Some(v), i) => ((v - i) rem v, v)

    crt(offsets)

  override lazy val input: String = FileIO.getInput(2020, 13)
