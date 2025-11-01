package gay.menkissing.advent
package y2015

import java.nio.{ByteBuffer, ByteOrder}
import scala.collection.mutable
import cats.Show

// this should not have been this hard???
object Day04 extends Problem:
  type Input = String
  type Output = Int
  def showOutput: Show[Int] = summon
  def parse(str: String): String = str.trim

  // NEVER roll your own crypto
  // after being done I looked up other solutions
  // and they all just used builtins
  object MD5:
    def calc(in: String, p2: Boolean): Boolean =
      val bites = gay.menkissing.hash.MD5.calc(in.getBytes("UTF-8"))
      if p2 then
        (0 until 3).forall: i =>
          bites(i) == 0
      else (0 until 2).forall(i => bites(i) == 0) && (bites(2) & 0xf0) == 0

  def part1(input: String): Int =
    Iterator.iterate(1)(_ + 1).find(it => MD5.calc(input + it.toString, false))
      .get
  def part2(input: String): Int =
    Iterator.iterate(1)(_ + 1).find(it => MD5.calc(input + it.toString, true))
      .get

  lazy val input: String = FileIO.getInput(2015, 4)
