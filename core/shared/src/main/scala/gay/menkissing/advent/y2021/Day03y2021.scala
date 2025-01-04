package gay.menkissing.advent
package y2021

import gay.menkissing.common.*

object Day03y2021 extends Problem[(List[Int], Int), Int]:
  lazy val input = FileIO.getInput(2021, 3)

  def parse(input: String): (List[Int], Int) =
    (input.linesIterator.map(Integer.parseInt(_, 2)).toList, input.linesIterator.next().trim.length)

  def part1(input: (List[Int], Int)): Int =
    val (is, bitSize) = input
    val halfCount = is.length / 2
    val counts = (0 until bitSize).reverse.map: bit =>
      val num = 1 << bit
      is.count(it => (num & it) != 0) > halfCount

    val gamma = counts.foldLeft(0):
      case (acc, x) =>
        val r = if x then 1 else 0
        (acc << 1) + r


    val epsilon = gamma ^ ((1 << bitSize) - 1)

    epsilon * gamma

  def part2(input: (List[Int], Int)): Int =
    val (is, bitSize) = input
    val halfCount = is.length / 2


    val (o2, co2) = (0 until bitSize).reverseIterator.foldLeft((is, is)):
      case (z @ (List(_), List(_)), _) => z
      case ((o2r, co2r), bit) =>
        val num = 1 << bit

        val newO2 =
          if o2r.tail.nonEmpty then
            val compared = o2r.count(it => (num & it) != 0).toDouble `compareTo` (o2r.length.toDouble / 2)
            compared match
              case -1 =>
                o2r.filter(it => (num & it) == 0)
              case 0 | 1 =>
                o2r.filter(it => (num & it) != 0)
          else
            o2r
        val newCO2 =
          if co2r.tail.nonEmpty then
            val compared = co2r.count(it => (num & it) != 0).toDouble `compareTo` (co2r.length.toDouble / 2)

            compared match
              case -1 => co2r.filter(it => (num & it) != 0)
              case  1 | 0 => co2r.filter(it => (num & it) == 0)
          else
            co2r

        (newO2, newCO2)

    o2.head * co2.head
