package gay.menkissing.advent
package y2020

import cats.*
import cats.implicits.*
import gay.menkissing.common.{*, given}
import parsley.*

import scala.collection.mutable

object Day19 extends Problem[(Map[Int, Day19.RawRule], List[String]), Int]:

  enum RawRule:
    case AChar(x: Char)
    case One(rules: List[Int])
    case Two(left: List[Int], right: List[Int])

  override def parse(str: String):(Map[Int, RawRule], List[String]) =
    val List(rules, messages) = str.split("\n\n").toList
    val ruleMap =
      rules.linesIterator.map:
        case s"$n: \"$c\"" => (n.toInt, RawRule.AChar(c.head))
        case s"$n: $l | $r" => (n.toInt, RawRule.Two(l.split(" ").map(_.toInt).toList, r.split(" ").map(_.toInt).toList))
        case s"$n: $x" => (n.toInt, RawRule.One(x.split(" ").map(_.toInt).toList))
      .toMap
    (ruleMap, messages.linesIterator.toList)


  def getRule(rules: Map[Int, RawRule], k: Int): List[String] =
    def calcResults(f: List[List[String]]): List[String] =
      f.foldLeft(List[String]("")): (acc, r) =>
        // intentionally using `mapN` and not `parMapN`
        (acc, r).mapN(_ + _)
    def go(n: Int): List[String] =
      rules(n) match
        case RawRule.AChar(c) => List(c.toString)
        case RawRule.One(x) =>
          calcResults(x.map(go))
        case RawRule.Two(l, r) =>
          calcResults(l.map(go)) ++ calcResults(r.map(go))

    go(k)


  override def part1(input: (Map[Int, RawRule], List[String])): Int = {
    val (rules, messages) = input





    val valid = getRule(rules, 0)
    // println(valid)
    val r = messages.intersect(valid)
    // println(r)
    r.size
  }

  def part2(input: (Map[Int, RawRule], List[String])): Int =
    import parsley.combinator.*

    val (rules, messages) = input
    assert(rules(0) == RawRule.One(List(8, 11)))
    val r42 = getRule(rules,42)
    //println(r42)
    val r31 = getRule(rules,31)
    //println(r31)
    //println(r42.intersect(r31))
    // yes WE are pulling in parsley again

    val r42Parser = choice(r42.map(x => Parsley.atomic(parsley.character.string(x)))*)
    val r31Parser = choice(r31.map(x => Parsley.atomic(parsley.character.string(x)))*)

    // 8: 42 | 42 8
    // 11: 42 31 | 42 11 31
    // 0: 8 11
    // all that matters is that we have less 31s at the end than 42s at the start

    val r0Parser = (countSome(r42Parser) <~> countSome(r31Parser)).filter(_ > _) *> Parsley.eof


    messages.count(it => r0Parser.parse(it).isSuccess)



  override lazy val input: String = FileIO.getInput(2020, 19)

