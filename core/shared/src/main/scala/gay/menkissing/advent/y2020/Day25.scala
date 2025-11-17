package gay.menkissing.advent
package y2020

import gay.menkissing.common.repeated
import cats.*

object Day25 extends HalfDay:
  type Input = (Long, Long)
  type Output = Long

  lazy val input = FileIO.getInput(2020, 25)

  def parse(str: String): Input =
    val Array(card, door) =
      str.linesIterator.map(_.toLong).toArray.runtimeChecked
    (card, door)

  def precalcMultInverses: Array[Int] =
    Array.tabulate(20201227): i =>
      if i == 0 then 0
      else (0 until 20201227).find(it => ((i * it) % 20201227) == 1).get

  def transform(subject: Long, loopSize: Int): Long =

    def transformStep(n: Long): Long = (n * subject) % 20201227L
    transformStep.repeated(loopSize)(1L)

  def part1(input: (Long, Long)): OutputP1 =
    val (card, door) = input
    // val cardLoopSize =
    //  Iterator.from(1).find(it => transform(7L, it) == card).get

    val inverseSeven = (0 until 20201227).find(it => ((7 * it) % 20201227) == 1)
      .get

    // our divisor is prime, meaning its always safe to "divide" in our modulo space
    // so.... we could reverse this in (mod 20201227) with n / (subject ^ loopSize)
    // or more simply just divide until we get the initial value
    val doorLoopSize =
      Monad[Id].tailRecM((door, 0)): (v, n) =>
        if v == 1 then Right(n)
        else
          val newV = (v * inverseSeven) % 20201227L
          Left((newV, n + 1))

    transform(card, doorLoopSize)
