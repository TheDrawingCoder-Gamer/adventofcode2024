package gay.menkissing.advent
package y2015

import cats.*
import cats.syntax.all.*

object Day11 extends Problem:
  type Input = String
  type Output = String

  def showOutput: Show[String] = summon

  def parse(str: String): String = str.trim

  def test(input: String): Boolean =
    val ls =
      input.toList.zipWithIndex.sliding2.flatMap:
        case ((a, i), (b, j)) =>
          Option.when(a == b)(a.toString + b.toString, List(i, j))
    input.forall("iol".contains) && ls.zip(ls).exists:
      case ((a, i), (b, j)) => a != b && i.intersect(j).isEmpty
    && input.toList.sliding3.exists: (a, b, c) =>
      c - b == 1 && b - a == 1
  def advance(input: String): String =
    val arr = input.getBytes("US-ASCII")
    var i = arr.length - 1
    while i >= 0 do
      arr(i) = (arr(i) + 1).toByte
      if arr(i) > 'z' then
        arr(i) = 'a'
        i -= 1
      else
        arr(i).toChar match
          case 'i' => arr(i) = 'j'
          case 'o' => arr(i) = 'p'
          case 'l' => arr(i) = 'm'
          case _   => ()
        i = -1

    new String(arr, "US-ASCII")

  def part1(input: String): String =
    Iterator.iterate(advance(input))(advance).find(test).get

  def part2(input: String): String =
    Iterator.iterate(advance(input))(advance).dropWhile(it => !test(it)).drop(1)
      .find(test).get

  lazy val input: String = FileIO.getInput(2015, 11)
