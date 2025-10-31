package gay.menkissing.advent
package y2015

import collection.mutable

import cats.*
import cats.syntax.all.*

object Day13 extends Problem[List[((String, String), Int)], Int]:
  def parse(str: String): List[((String, String), Int)] =
    str.trim.linesIterator.map:
      case s"$p1 would gain $n happiness units by sitting next to $p2." =>
        ((p1, p2), n.toInt)
      case s"$p1 would lose $n happiness units by sitting next to $p2." =>
        ((p1, p2), -n.toInt)
    .toList

  def optimize(in: List[((String, String), Int)]): (Map[(Int, Int), Int], Int) =
    val seen = mutable.HashMap.empty[String, Int]
    var i = 0

    def getID(s: String): Int =
      seen.getOrElseUpdate(s,
        locally:
          val hold = i
          i += 1
          hold
      )

    val x =
      in.map:
        case ((s, o), n) => ((getID(s), getID(o)), n)
    (x.toMap, i)


  def calc(input: Map[(Int, Int), Int], nPeople: Int): Int =
    // yes WE are bruteforcing ts...
    (0 until nPeople).toList.permutations.map: ls =>
      val badSum = ls.sliding2.map((x, y) => input.getOrElse((x, y), 0) + input.getOrElse((y, x), 0)).sum
      val last = ls.last
      val edge = input.getOrElse((ls.head, last), 0) + input.getOrElse((last, ls.head), 0)
      badSum + edge
    .max

  def part1(input: List[((String, String), Int)]): Int =
    val (optInput, nPeople) = optimize(input)
    calc(optInput, nPeople)



  def part2(input: List[((String, String), Int)]): Int =
    val (optInput, nPeople) = optimize(input)
    // no need to update list, getOrElse works just fine
    calc(optInput, nPeople + 1)

  lazy val input: String = FileIO.getInput(2015, 13)
