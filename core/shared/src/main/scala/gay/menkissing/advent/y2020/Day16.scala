package gay.menkissing.advent
package y2020

import cats.*
import cats.implicits.*
import gay.menkissing.common.{*, given}

import scala.collection.mutable

object Day16 extends Problem:
  type Input = DaInput
  type Output = Long
  case class DaInput
    (
      validRanges: Map[String, (Range, Range)],
      ourTicket: List[Int],
      tickets: List[List[Int]]
    )

  override def parse(str: String): DaInput =
    val Array(ranges, ourTicket, tickets) = str.split("\n\n")
    val rangeMap =
      ranges.linesIterator.map:
        case s"$k: $r11-$r12 or $r21-$r22" =>
          k -> (r11.toInt to r12.toInt, r21.toInt to r22.toInt)
      .toMap
    val goodTicket =
      ourTicket.linesIterator.drop(1).next().split(",").map(_.toInt)
    val goodTickets =
      tickets.linesIterator.drop(1).map(_.split(",").map(_.toInt).toList)

    DaInput(rangeMap, goodTicket.toList, goodTickets.toList)

  override def part1(input: DaInput): Long =
    input.tickets.flatten.filter: x =>
      input.validRanges.values.forall(!_.contains(x) && !_.contains(x))
    .sum.toLong

  def part2(input: DaInput): Long =
    val goodTickets =
      input.tickets.filter: ticket =>
        ticket.forall: x =>
          input.validRanges.values.exists(_.contains(x) || _.contains(x))

    val ordered =
      goodTickets.prepended(input.ourTicket).transpose.map: xs =>
        input.validRanges.filter:
          case (_, (a, b)) => xs.forall(x => a.contains(x) || b.contains(x))
        .keys.toSet
    // observation of part 2 inputs:
    // ordered.map(_.size).sorted returns an ascending list with no gaps, meaning we can fairly easily do this shi without having
    // to do extra testing

    val usedSet = mutable.Set[String]()
    val realOrdered =
      ordered.zipWithIndex.sortBy(_._1.size).map: (x, i) =>
        val item = (x -- usedSet).head
        usedSet += item
        (item, i)
      .sortBy(_._2).map(_._1)

    // println(realOrdered)
    // println(go(ordered).value)

    input.ourTicket.zip(realOrdered).filter(_._2.startsWith("departure"))
      .map(_._1.toLong).product

  override lazy val input: String = FileIO.getInput(2020, 16)
