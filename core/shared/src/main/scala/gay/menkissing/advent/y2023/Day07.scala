package gay.menkissing.advent
package y2023

import cats.*
import cats.syntax.all.*

object Day07 extends Problem:
  type Input = List[(Hand, Int)]
  type Output = Int

  lazy val input: String = FileIO.getInput(2023, 7)

  override def parse(str: String): List[(Hand, Int)] =
    str.linesIterator.toList.map:
      case s"$hand $n" => (Hand(hand), n.toInt)
      case _           => ???

  enum HandKind:
    case HighCard
    case OnePair
    case TwoPair
    case ThreeOfAKind
    case FullHouse
    case FourOfAKind
    case FiveOfAKind

  given Order[HandKind] = Order.by(_.ordinal)

  case class Hand(value: String) extends AnyVal
  object Hand:
    def apply(n: String): Hand =
      assert(n.length == 5)
      new Hand(n)

    // VALID FULL HOUSES:
    // n.size == 2; 2 or 3 card types, including jonkler. if there were no second non jonkler, then a five of a kind would be chosen instead
    // INVALID: QJJJK, QQJJK; Four of a kind is stronger and will be picked instead. There will be a max of TWO jonklers, '
    // and they will cleanly fit in ONE group, otherwise it will be a four of a kind
    // VALID: QJJKK, QQJKK, QQQJK
    def validFullHouseP2(n: Map[Char, Int], jonklers: Int): Boolean =
      if n.size != 2 then false
      else
        val mn = n.values.min
        val mx = n.values.max
        jonklers match
          case 0 => mn == 2 && mx == 3
          case 1 => (mn == 1 && mx == 3) || (mn == 2 && mx == 2)
          case 2 =>
            // swapped roles, mn is the large group
            mn == 1 && mx == 2
          case _ => false

    // VALID TWO PAIRS
    // QQKKT
    // Any jonkler will likely cause it to increase in strength unconditionally as its too easy to increase strength of it
    // INVALID: QQJKT; Three pair is considered stronger and will be selected instead.
    // INVALID: QQKKJ; Full house will be selected instead
    // INVALID: QQJJT; Four pair will be selected instead
    def validTwoPairP2(n: Map[Char, Int], jonklers: Int): Boolean =
      jonklers == 0 && n.count(_._2 == 2) == 2

    extension (hand: Hand)
      def kindP2: HandKind =
        val m = hand.value.groupMapReduce(identity)(_ => 1)(_ + _)
        val jonklers = m.getOrElse('J', 0)
        val n = m.removed('J')

        // If the size is <= 1. If size == 1 and there are jonklers, the effect is the same
        if n.size <= 1 then HandKind.FiveOfAKind
        else if n.exists(_._2 >= 4 - jonklers) then HandKind.FourOfAKind
        else if validFullHouseP2(n, jonklers) then HandKind.FullHouse
        else if n.exists(_._2 >= 3 - jonklers) then HandKind.ThreeOfAKind
        else if validTwoPairP2(n, jonklers) then HandKind.TwoPair
        else if n.exists(_._2 >= 2 - jonklers) then HandKind.OnePair
        else HandKind.HighCard

      def kind: HandKind =
        val n = hand.value.groupMapReduce(identity)(_ => 1)(_ + _)
        if n.size == 1 then HandKind.FiveOfAKind
        else if n.exists(_._2 == 4) then HandKind.FourOfAKind
        else if n.exists(_._2 == 3) && n.exists(_._2 == 2) then
          HandKind.FullHouse
        else if n.exists(_._2 == 3) then HandKind.ThreeOfAKind
        else if n.count(_._2 == 2) == 2 then HandKind.TwoPair
        else if n.exists(_._2 == 2) then HandKind.OnePair
        else HandKind.HighCard
    private def charStrengthP2(c: Char): Byte =
      c match
        case 'J' => -1
        case '2' => 0
        case '3' => 1
        case '4' => 2
        case '5' => 3
        case '6' => 4
        case '7' => 5
        case '8' => 6
        case '9' => 7
        case 'T' => 8
        case 'Q' => 10
        case 'K' => 11
        case 'A' => 12

    private def charStrength(c: Char): Byte =
      c match
        case '2' => 0
        case '3' => 1
        case '4' => 2
        case '5' => 3
        case '6' => 4
        case '7' => 5
        case '8' => 6
        case '9' => 7
        case 'T' => 8
        case 'J' => 9
        case 'Q' => 10
        case 'K' => 11
        case 'A' => 12

    val orderHandP2: Order[Hand] =
      new Order[Hand]:
        override def compare(x: Hand, y: Hand): Int =
          val r = x.kindP2.compare(y.kindP2)
          if r != 0 then r
          else
            val left = x.value.toList.map(charStrengthP2)
            val right = y.value.toList.map(charStrengthP2)
            left.compare(right)

    given orderHand: Order[Hand] with
      override def compare(x: Hand, y: Hand): Int =
        val r = x.kind.compare(y.kind)
        if r != 0 then r
        else
          val left = x.value.toList.map(charStrength)
          val right = y.value.toList.map(charStrength)
          left.compare(right)

  import Hand.*
  override def part1(input: List[(Hand, Int)]): Int =
    val resInput = input.sortBy(_._1)(using Hand.orderHand.toOrdering)
    resInput.zipWithIndex.map:
      case ((_, bid), idx) => bid * (idx + 1)
    .sum

  override def part2(input: List[(Hand, Int)]): Int =
    input.sortBy(_._1)(using Hand.orderHandP2.toOrdering).zipWithIndex.map:
      case ((_, bid), idx) => bid * (idx + 1)
    .sum
