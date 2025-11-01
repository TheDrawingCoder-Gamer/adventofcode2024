package gay.menkissing.advent
package y2015

import cats.*
import cats.implicits.*
import cats.data.Chain

import gay.menkissing.common.*
import alleycats.std.set.*

import scala.collection.mutable
import scala.annotation.tailrec

object Day19 extends Problem[(Map[String, List[String]], String), Int]:
  lazy val input = FileIO.getInput(2015, 19)

  def splitElements(str: String): List[String] =
    val builder = List.newBuilder[String]
    var cur = str
    while cur.nonEmpty do
      val (valueT, rest) = cur.tail.span(_.isLower)
      builder += cur.head +: valueT
      cur = rest
    builder.result()

  def parse(str: String): (Map[String, List[String]], String) =
    val Array(mapLines, pat) = str.split("\n\n").array: @unchecked
    val daMap =
      mapLines.linesIterator.map:
        case s"$k => $v" => (k, v)
      .toList.groupMap(_._1)(_._2)
    (daMap, pat.trim)

  def uniqueReplacements
    (
      str: String,
      replace: String,
      withValue: String
    ): Set[String] =

    @tailrec def go(prev: String, rest: String, acc: Set[String]): Set[String] =
      if rest.isEmpty then acc
      else if rest.startsWith(replace) then
        go(
          prev + rest.head,
          rest.tail,
          acc + (prev + withValue + rest.drop(replace.length))
        )
      else go(prev + rest.head, rest.tail, acc)
    go("", str, Set.empty)

  def part1(input: (Map[String, List[String]], String)): Int =
    val (daMap, pattern) = input

    val values =
      daMap.map: (k, v) =>
        v.map: value =>
          uniqueReplacements(pattern, k, value)
        .reduce(_ ++ _)
      .reduce(_ ++ _)

    values.size

  def part2(input: (Map[String, List[String]], String)): Int =
    val (_, pattern) = input

    val elems = splitElements(pattern)
    val lParen = elems.count(_ == "Rn")
    val rParen = elems.count(_ == "Ar")
    val commas = elems.count(_ == "Y")

    // we only have to handle rules passed, and:
    // only operations are
    // e => TT or T => TT
    // T => T(T)
    // T => T(T,T)
    // T => T(T,T,T)
    // Where T is a singleton "element"
    // and Rn maps to `(`, Y maps to `,` and Ar maps to `)`
    // our test input violates our first rule, so its not particularly useful for this
    // Here we are basically doing the opposite of this, but we only really care about
    // the count of steps.
    // We start with 2 elements from `e`, and with T => TT operations we add 1 element
    // each step.
    // Inverting this, we can remove one element every step.
    // So AAAAA => AAAA => AAA => AA => A
    // This takes four steps, which is one less than our starting char count.
    // With T => T(T), its similar but we get 2 elements for free
    // so A(A(A(A(A)))) => A(A(A(A))) => A(A(A)) => A(A) => A
    // In this case, we can just ignore the parens and only count elements, and apply our previous rule.
    // T => T(T,T)
    // A(A(A,A),A(A,A)) => A(A(A,A),A) => A(A,A) => A
    // As always, we can ignore parens. Elems: 7, commas: 3.
    // We subtract commas from our elems count, as each extra argument in an application lets us take 2 tokens, one of them being the comma.
    // Equivilantly, we can do total tokens - parens - 2 * commas - 1.
    elems.size - (lParen + rParen) - 2 * commas - 1
