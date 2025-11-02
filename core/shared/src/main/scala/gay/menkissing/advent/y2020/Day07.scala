package gay.menkissing.advent
package y2020

import cats.Show

object Day07 extends Problem:
  type Input = Rules
  type Output = Int

  case class BagDesc(color: String, amount: Int)
  type Rules = Map[String, Set[BagDesc]]

  override def parse(str: String): Rules =
    str.linesIterator.map:
      case s"$color bags contain $rest." =>
        color -> rest.split(',').foldLeft(Set.empty[BagDesc]):
          case (acc, r) =>
            r.trim match
              case s"no other bags"    => acc
              case s"$n $color2 bag$_" => acc + BagDesc(color2, n.toInt)
    .toMap

  override def part1(input: Rules): Int =
    def containsShinyGold(bag: String): Boolean =
      val rule = input(bag)
      rule.exists(_.color == "shiny gold") ||
      rule.exists(it => containsShinyGold(it.color))

    input.removed("shiny gold").count(it => containsShinyGold(it._1))

  override def part2(input: Rules): Int =
    def getBagAmount(bag: String): Int =
      val rule = input(bag)
      rule.foldLeft(1):
        case (acc, bag) => acc + (bag.amount * getBagAmount(bag.color))
    getBagAmount("shiny gold") - 1

  override lazy val input: String = FileIO.getInput(2020, 7)
