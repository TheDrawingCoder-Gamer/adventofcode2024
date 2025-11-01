package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import cats.Show

object Day18 extends Problem:
  type Input = SnailfishNum
  type Output = Long
  def showOutput: Show[Long] = summon

  sealed trait SnailfishNum:
    def +(that: SnailfishNum): SnailfishNum = SnailfishGroup(this, that)
    def dfsMapFirst(f: SnailfishNum => Option[SnailfishNum]): SnailfishNum =
      this match
        case SnailfishLiteral(_)  => f(this).getOrElse(this)
        case SnailfishGroup(l, r) =>
          f(l) match
            case Some(v) => v
            case _       => f(r).getOrElse(this)

  case class SnailfishGroup(l: SnailfishNum, r: SnailfishNum)
      extends SnailfishNum
  object SnailfishNum:
    def parse(str: String): SnailfishNum =
      str match
        case s"[$l,$r]" => SnailfishGroup(parse(l), parse(r))
        case x          => SnailfishLiteral(x.toInt)

  case class SnailfishLiteral(lit: Int) extends SnailfishNum

  override def parse(str: String): SnailfishNum = SnailfishNum.parse(str)

  override def part1(input: SnailfishNum): Long = ???

  override def part2(input: SnailfishNum): Long = ???

  override lazy val input: String = ???
