package gay.menkissing.advent
package y2022

import gay.menkissing.common.ArityN.*
import cats.parse.*
import cats.*
import cats.implicits.*

object Day13 extends Problem:
  type Input = Vector[Packet]
  type Output = Int

  sealed trait Packet extends Ordered[Packet]:
    def compare(that: Packet): Int =
      (this, that) match
        case (NestedPacket(left), NestedPacket(right)) =>
          lazy val goodLength = left.length.compare(right.length)

          left.zip(right).collectFirstSome: (l, r) =>
            val res = l.compare(r)
            Option.when(res != 0)(res)
          .getOrElse(goodLength)
        case (LeafPacket(left), LeafPacket(right)) => left `compare` right
        case (l @ LeafPacket(left), r @ NestedPacket(right)) =>
          NestedPacket(List(l)) `compare` r
        case (left @ NestedPacket(_), r @ LeafPacket(_)) =>
          left `compare` NestedPacket(List(r))

    override def equals(x: Any): Boolean =
      x match
        case p: Packet => this.compare(p) == 0
        case _         => false

  // order contains eq, which so happens to exactly match our universal equals impl
  given orderPacket: Order[Packet] = Order.fromOrdering(using Ordering.ordered)
  final case class NestedPacket(packets: List[Packet]) extends Packet
  final case class LeafPacket(n: Int) extends Packet

  val leafParser: Parser[LeafPacket] =
    Numbers.bigInt.map(it => LeafPacket(it.toInt))
  val packetParser: Parser[Packet] =
    Parser.recursive[Packet]: recur =>
      leafParser | Parser.string("[]").as(NestedPacket(List())) |
        recur.repSep(Parser.char(','))
          .between(Parser.char('['), Parser.char(']'))
          .map(it => NestedPacket(it.toList))

  def input = FileIO.getInput(2022, 13)

  def parse(str: String): Vector[Packet] =
    str.linesIterator.filterNot(_.isEmpty).map: it =>
      packetParser.parseAll(it) match
        case Left(_)  => assert(false)
        case Right(v) => v
    .toVector

  def part1(input: Vector[Packet]): Int =
    input.groupedN[2].zipWithIndex.map:
      case ((l, r), idx) =>
        if l <= r then idx + 1
        else 0
    .sum

  def part2(input: Vector[Packet]): Int =
    val divider1 = NestedPacket(List(NestedPacket(List(LeafPacket(2)))))
    val divider2 = NestedPacket(List(NestedPacket(List(LeafPacket(6)))))

    val dataP2 = input.prependedAll(Seq(divider1, divider2)).sorted

    (dataP2.indexOf(divider1) + 1) * (dataP2.indexOf(divider2) + 1)
