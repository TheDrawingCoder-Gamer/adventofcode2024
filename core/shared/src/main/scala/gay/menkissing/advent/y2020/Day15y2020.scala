package gay.menkissing.advent
package y2020

import cats.*
import cats.implicits.*
import gay.menkissing.common.{*, given}

import scala.collection.mutable

object Day15y2020 extends Problem[List[Int], Int]:


  override def parse(str: String): List[Int] =
    str.split(",").map(_.toInt).toList


  enum Memory:
    case First(idx: Int)
    case Repeated(old: Int, recent: Int)

    def mostRecent: Int = this match
      case First(i) => i
      case Repeated(_, i) => i

  def setMap(map: Map[Int, Memory], num: Int, idx: Int): Map[Int, Memory] =
    if map.isDefinedAt(num) then
      map.updated(num, Memory.Repeated(map(num).mostRecent, idx))
    else
      map.updated(num, Memory.First(idx))

  def calc(input: List[Int]): Iterator[(Int, Int, Map[Int, Memory])] =
    val m = input.zipWithIndex.map[(Int, Memory)]((x, i) => (x, Memory.First(i))).toMap
    Iterator.iterate((input.last, input.length, m)): (lastNum, idx, map) =>
      map(lastNum) match
        case Memory.First(_) =>
          (0, idx + 1, setMap(map, 0, idx))
        case Memory.Repeated(i, r) =>
          val n = r - i
          (n, idx + 1, setMap(map, n, idx))

  override def part1(input: List[Int]): Int = {
    calc(input).drop(2020 - input.length).next()._1

  }


  // please optimize me
  def part2(input: List[Int]): Int =
    calc(input).drop(30000000 - input.length).next()._1


  override lazy val input: String = FileIO.getInput(2020, 15)

