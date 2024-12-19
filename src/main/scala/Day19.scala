import gay.menkissing.advent.Problem

import scala.annotation.tailrec
import scala.io.Source
import cats.Eval
import cats.syntax.all.*
import scala.collection.mutable as mut
import scala.collection.parallel.CollectionConverters.*

object Day19 extends Problem[(Day19.Towels, Day19.Designs), Long]:
  type Towel = String

  type Towels = List[Towel]

  type Designs = List[String]

  override def parse(str: String): (Towels, Designs) =
    val Array(towelsStr, designStr) = str.split("\n\n")

    val towels = towelsStr.split(',').map(_.trim).toList
    val designs = designStr.split('\n').toList

    (towels.sortBy(_.length)(using Ordering[Int].reverse), designs)

  def firstDesignCombo(towels: Towels, design: String): Option[List[Towel]] =
    def go(curTowels: Towels, restDesign: String, acc: List[Towel]): Option[List[Towel]] =
      if restDesign.isEmpty then
        Some(acc.reverse)
      else
        curTowels match
          case head :: next =>
            if restDesign.startsWith(head) then
              val nextDesign = restDesign.drop(head.length)
              val res = go(towels, nextDesign, acc.prepended(head))

              if res.isDefined then
                res
              else
                // backtrack
                go(next, restDesign, acc)
            else
              go(next, restDesign, acc)
          case Nil => None
    go(towels, design, List())

  def parseDesign(towels: Towels, design: String): Boolean =
    firstDesignCombo(towels, design).isDefined
  def getCombos(towels: Towels, design: String): List[List[Towel]] =
    def go(curTowels: Towels, restDesign: String, acc: List[Towel]): Eval[List[List[Towel]]] =
      if restDesign.isEmpty then
        Eval.now(List(acc.reverse))
      else
        curTowels match
          case head :: next =>
            val firstPart =
              if restDesign.startsWith(head) then
                go(towels, restDesign.drop(head.length), acc.prepended(head))
              else
                Eval.now(List[List[Towel]]())
            firstPart.flatMap: combos =>
              go(next, restDesign, acc).map(combos ++ _)
          case Nil => Eval.now(List())

    go(towels, design, List()).value
  def simplifyTowels(towels: Towels): List[(Towel, (Int, Set[Towel]))] =
    towels.map: towel =>
      val combos = getCombos(towels, towel)
      val count = combos.size
      towel -> (count, combos.flatten.toSet)
  def countDesigns(towels: Towels, design: String): Long =
    def go(pattern: String, total: Long, cache: Map[String, Long]): (Long, Map[String, Long]) =
      cache.get(pattern) match
        case Some(count) => (total + count, cache)
        case _ =>
          val (count, cache2) = towels.foldLeft(0L -> cache):
            case ((count, cache), towel) =>
              if pattern.startsWith(towel) then
                go(pattern.drop(towel.length), count, cache)
              else
                (count, cache)
          (total + count, cache2 + (pattern -> count))


    go(design, 0L, Map("" -> 1L))._1


  override def part1(input: (Towels, Designs)): Long =
    val (towels, designs) = input
    designs.count: design =>
      parseDesign(towels, design)
    .toLong

  override def part2(input: (Towels, Designs)): Long =
    val (towels, designs) = input
    designs.map: design =>
      countDesigns(towels, design)
    .sum

  override def input: String = Source.fromResource("day19.txt").mkString


@main def main(): Unit =
  Day19.debugAndTimeP1()
  Day19.debugAndTimeP2()