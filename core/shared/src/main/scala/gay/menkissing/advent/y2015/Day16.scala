package gay.menkissing.advent
package y2015

import gay.menkissing.common.*

object Day16 extends Problem[List[Day16.Aunt], Int]:
  val greaterValues = Set("cats", "trees")
  val lessValues = Set("pomeranians", "goldfish")
  val specialValues = greaterValues ++ lessValues
  case class Aunt(n: Int, values: Map[String, Int]):
    def validP1: Boolean =
      values.forall((k, v) => giftSue(k) == v)
    def validP2: Boolean =
      values.filter((k, _) => !specialValues(k)).forall((k, v) => giftSue(k) == v)
      && values.filter((k, _) => greaterValues(k)).forall((k, v) => giftSue(k) < v)
      && values.filter((k, _) => lessValues(k)).forall((k, v) => giftSue(k) > v)


  lazy val input: String = FileIO.getInput(2015, 16)

  val giftSue = 
    Map(
      "children" -> 3,
      "cats" -> 7,
      "samoyeds" -> 2,
      "pomeranians" -> 3,
      "akitas" -> 0,
      "vizslas" -> 0,
      "goldfish" -> 5,
      "trees" -> 3,
      "cars" -> 2,
      "perfumes" -> 1
    )
  def parse(str: String): List[Aunt] = 
    str.linesIterator.map:
      case s"Sue $n: $values" => 
        Aunt(n.toInt,
          values.split(", ").map:
            case s"$k: $v" => k -> v.toInt
            case _ => whatTheScallop.!
          .toMap
          )
      case _ => whatTheScallop.!
    .toList

  def part1(input: List[Aunt]): Int =
    input.find(_.validP1).get.n
  
  def part2(input: List[Aunt]): Int =
    input.find(_.validP2).get.n
