package gay.menkissing.advent
package y2015

import gay.menkissing.common.*

object Day25 extends HalfDay[Vec2i, Long]: 
  lazy val input: String = FileIO.getInput(2015, 25)

  def parse(str: String): Vec2i =
    str.trim match
      case s"To continue, please consult the code grid in the manual.  Enter the code at row $y, column $x." => Vec2i(x.toInt, y.toInt)
      case _ => whatTheScallop.!
  
  final def sumtorial(p: Int): Int =
    if p < 0 then
      0
    else
      (1 to p).sum


  def xyToN(v: Vec2i): Int =
    val root = sumtorial(v.y - 1) + 1
    // if the end of the range is less then the start then it will be empty
    // and that works for me!
    // IT JUST WORKS :tm:
    // dont ask how i derived this (it was staring at the pattern)
    root + ((v.y + 1) to (v.y + v.x - 1)).sum

  def advance(v: Long): Long =
    (v * 252533L) % 33554393L

  def part1(input: Vec2i): Long =
    // translate our input into an index into an iterator
    val n = xyToN(input)
    advance.repeated(n - 1)(20151125L)

