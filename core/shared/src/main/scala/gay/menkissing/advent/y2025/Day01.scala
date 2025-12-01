package gay.menkissing.advent
package y2025

import gay.menkissing.common.rem
import scala.math.Integral.Implicits.infixIntegralOps

object Day01 extends Problem:
  type Input = List[(Boolean, Int)]
  type Output = Int

  lazy val input = FileIO.getInput(2025, 1)

  def parse(str: String): Input =
    str.linesIterator.map:
      case s"R$n" => (true, n.toInt)
      case s"L$n" => (false, n.toInt)
    .toList

  def part1(input: List[(Boolean, Int)]): OutputP1 =
    input.foldLeft((50, 0)):
      case ((cur, passZero), (right, by)) =>
        val newValue =
          if right then (cur + by) rem 100
          else (cur - by) rem 100

        (newValue, if newValue == 0 then passZero + 1 else passZero)
    ._2

  def part2(input: List[(Boolean, Int)]): OutputP2 =
    input.foldLeft((50, 0)):
      case ((cur, passZero), (right, by)) =>
        val (forcedWrap, byN) = by /% 100
        val byV = if right then byN else -byN
        val unwrapped = cur + byV
        val newValue = unwrapped rem 100
        val passV =
          forcedWrap + locally:
            if unwrapped >= 100 then 1
            else if cur != 0 && unwrapped <= 0 then 1
            else 0
        println(
          s"$cur (${if right then "R" else "L"}$by)-> $newValue $unwrapped $passV"
        )
        (newValue, passZero + passV)
    ._2
