package gay.menkissing.advent
package y2015

import gay.menkissing.common.*
import cats.syntax.all.*
object Day24 extends Problem:
  type Input = List[Int]
  type Output = Long

  lazy val input = FileIO.getInput(2015, 24)

  def parse(str: String): Input = str.linesIterator.map(_.toInt).toList

  def findNumberWithSumOfPrimesEq(primes: List[Int], n: Int): Long =
    (1 until primes.length).findMap: combos =>
      primes.combinations(combos).findMap: x =>
        val z = x.map(_.toLong)
        Option.when(z.sum == n)(z.product)
    .get

  // Our input is prime numbers; so I assume the solution has something to do with
  // primes as well
  // OR, it could just be that with primes you can guarentee there will be only 1
  // solution and thats easier to manage
  def part1(input: List[Int]): OutputP1 =
    val sum = input.sum
    val third = sum / 3
    // ???
    findNumberWithSumOfPrimesEq(input, third)

  def part2(input: List[Int]): Long =
    val sum = input.sum
    val fourth = sum / 4

    findNumberWithSumOfPrimesEq(input, fourth)
