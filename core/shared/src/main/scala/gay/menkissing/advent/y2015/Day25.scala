package gay.menkissing.advent
package y2015

import gay.menkissing.common.*

object Day25 extends HalfDay[Vec2[Int], Long]:
  lazy val input: String = FileIO.getInput(2015, 25)

  def parse(str: String): Vec2[Int] =
    str.trim match
      case s"To continue, please consult the code grid in the manual.  Enter the code at row $y, column $x." =>
        Vec2(x.toInt, y.toInt)
      case _ => whatTheScallop.!

  final def sumtorial(p: Int): Int = IntSequences.triangleNumber(p).toInt

  // A cursory search (that i didnt do before) for triangle numbers on the OEIS
  // linked to "Floyd's triangle"
  // which is exactly what we are working with here.
  // The "left edge" (the first column) has the lazy caterer's sequence
  // The "right edge" (the first row) has the triangle numbers (named sumtorial here)
  // if there was a way to translate X Y (in the puzzle space) to R C (row column in floyd's triangle),
  // I could trivially get the number.
  // Based off of a drawing, I think that R will = X - 1 + Y
  // I think that C will = X
  // Given R and C, the answer should be sumtorial(R - 1) + C
  // Sumtorial of R - 1 will give us the very last number of the previous row,
  // and then we add C to get our current number.

  def xyToN(v: Vec2[Int]): Int =
    val r = v.x - 1 + v.y
    val c = v.x
    sumtorial(r - 1) + c

  def advance(v: Long): Long = (v * 252533L) % 33554393L

  def part1(input: Vec2[Int]): Long =
    // translate our input into an index into an iterator
    val n = xyToN(input)
    advance.repeated(n - 1)(20151125L)
