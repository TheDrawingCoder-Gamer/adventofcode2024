package gay.menkissing.advent
package y2020

import cats.*
import cats.implicits.*
import gay.menkissing.common.{*, given}

import scala.collection.mutable

object Day14 extends Problem[List[Day14.Op], Long]:
  enum Op:
    case Mask(str: String)
    case Mem(idx: Int, value: Long)

  override def parse(str: String): List[Op] =
    str.linesIterator.map:
      case s"mask = $m" =>
        Op.Mask(m)
      case s"mem[$i] = $r" => Op.Mem(i.toInt, r.toLong)
      case _ => ???
    .toList



  override def part1(input: List[Op]): Long =
    input.foldLeft(((1L << 36L) - 1L, 0L, Vector.fill(65565)(0L))):
      case ((andM, orM, mem), op) =>
        op match
          case Op.Mask(m) =>
            (java.lang.Long.parseLong(m.replace("X", "1"), 2), java.lang.Long.parseLong(m.replace("X", "0"), 2), mem)
          case Op.Mem(idx, value) =>
            (andM, orM, mem.updated(idx, (value & andM) | orM))
    ._3.sum

  def padLeft(str: String, to: Int, c: Char): String =
    if str.length < to then
      (c.toString * (to - str.length)) + str
    else
      str

  // lobotomy victim
  def part2(input: List[Op]): Long =
    var mask = ""
    val mem = mutable.HashMap[Long, Long]()
    input.foreach:
      case Op.Mask(m) => mask = m
      case Op.Mem(idx, value) =>
        val appliedMask = (padLeft(idx.toBinaryString, 36, '0').toList, mask.toList).parMapN:
          case (i, '0') => i
          case (_,  m) => m

        val v = mask.count(_ == 'X')
        if v > 0 then
          (0 until (1 << v)).foreach: n =>
            var u = v - 1
            val memAddr = appliedMask.foldLeft(""): (a, c) =>
              c match
                case 'X' =>
                  val x = (n >> u) & 1
                  u -= 1
                  a + x.toString
                case i =>
                  a + i
            mem(java.lang.Long.parseLong(memAddr, 2)) = value
        else
          mem(idx) = value

    mem.values.sum


  override lazy val input: String = FileIO.getInput(2020, 14)

