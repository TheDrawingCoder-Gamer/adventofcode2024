package gay.menkissing.advent
package y2021

import gay.menkissing.common.*, ArityN.*
import collection.mutable

object Day14 extends Problem:
  type Input = (String, Map[CharPair, Char])
  type Output = Long

  type CharPair = (Char, Char)

  type Occurrences = Map[Char, Long]
  lazy val input = FileIO.getInput(2021, 14)
  def parse(str: String): Input =
    val Array(template, patterns) = str.split("\n\n").toArray: @unchecked
    val goodPats =
      patterns.linesIterator.map:
        case s"$a -> $b" => (a(0), a(1)) -> b.head
        case _           => whatTheScallop.!
      .toMap
    (template.trim, goodPats)

  extension (self: Map[CharPair, Char])
    def advanceP1(n: String): String =
      n.toList.slidingN[2].map: s =>
        self.get(s) match
          case Some(v) => s(0) + v
          case _       => s.head
      .mkString("") + n.last

  def addOccurrences(l: Occurrences, r: Occurrences): Occurrences =
    r.foldLeft(l):
      case (prev, (char, occ)) =>
        prev + (char -> (prev.getOrElse(char, 0L) + occ))

  // once a pair (eventually) stops producing elements inside it, it will never produce elements
  // inside it again
  // so if i were to precompute what each input pair expands to after N steps, i can easily figure out
  // occurances
  def part1(input: (String, Map[CharPair, Char])): Long =
    val (template, patterns) = input
    val zs = patterns.advanceP1.repeated(10)(template)
    val occurs = zs.groupMapReduce(identity)(_ => 1)(_ + _)
    (occurs.values.max - occurs.values.min).toLong

  // shamelessly copied from the scala center site, but to be fair I had already solved this before,
  // just had no code to prove it
  def part2(input: (String, Map[CharPair, Char])): Long =
    val (template, patterns) = input
    val map = mutable.Map.empty[(CharPair, Int), Occurrences]

    patterns.foreach:
      case (pair @ (fst, snd), inserted) => map((pair, 0)) = Map(snd -> 1L)

    (1 to 40).foreach: n =>
      patterns.foreach: (pair, inserted) =>
        val (fst, snd) = pair
        val z = patterns(pair)
        map((pair, n)) =
          addOccurrences(map((fst, z), n - 1), map((z, snd), n - 1))

    val pairsInPolymer = template.toList.slidingN[2]
    val polymerS =
      pairsInPolymer.map(pair => map((pair, 40))).reduce(addOccurrences)

    val occurrences = addOccurrences(polymerS, Map(template.head -> 1L))

    occurrences.values.max - occurrences.values.min
