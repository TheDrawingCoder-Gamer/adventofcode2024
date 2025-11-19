package gay.menkissing.advent
package y2015

import scala.collection.mutable

object Day07 extends Problem:
  type Input = Map[String, Op]
  type Output = Int

  enum Op:
    case Value(s: String)
    case And(x: String, y: String)
    case Or(x: String, y: String)
    case Not(x: String)
    case LShift(p: String, by: Int)
    case RShift(p: String, by: Int)

  def parse(str: String): Map[String, Op] =
    str.linesIterator.map:
      case s"$op -> $r" =>
        val o =
          op match
            case s"$x AND $y"    => Op.And(x, y)
            case s"$x OR $y"     => Op.Or(x, y)
            case s"$x LSHIFT $y" => Op.LShift(x, y.toInt)
            case s"$x RSHIFT $y" => Op.RShift(x, y.toInt)
            case s"NOT $x"       => Op.Not(x)
            case v               => Op.Value(v)
        (r, o)
    .toMap

  def calc(input: Map[String, Op]): String => Int =
    val memo = mutable.HashMap[String, Int]()

    def go(s: String): Int =
      def valueOrGet(v: String): Int =
        v.toIntOption match
          case Some(v) => v.toShort
          case None    => go(v)

      memo.getOrElseUpdate(
        s,
        (input(s) match
          case Op.Value(v)     => valueOrGet(v)
          case Op.And(a, b)    => valueOrGet(a) & valueOrGet(b)
          case Op.Or(a, b)     => valueOrGet(a) | valueOrGet(b)
          case Op.Not(a)       => ~valueOrGet(a)
          case Op.LShift(a, b) => valueOrGet(a) << b
          case Op.RShift(a, b) => valueOrGet(a) >>> b
        ) & 0xffff
      )

    go

  def part1(input: Map[String, Op]): Int =
    val go = calc(input)

    go("a")

  def part2(input: Map[String, Op]): Int =
    val go = calc(input)
    val a = go("a")
    val newInput = input.updated("b", Op.Value(a.toString))
    val go2 = calc(newInput)
    go2("a")

  def input: String = FileIO.getInput(2015, 7)
