package gay.menkissing.advent
package y2025

import cats.data.*
import cats.syntax.all.*

object Day07 extends Problem:
  type Input = (start: Int, splitters: List[Set[Int]])
  type Output = Long

  def input: String = FileIO.getInput(2025, 7)

  def parse(str: String): Input =
    val start :: rest = str.linesIterator.toList.runtimeChecked
    val startIdx = start.indexOf("S")
    val splitters = rest.map(_.zipWithIndex.filter(_._1 == '^').map(_._2).toSet)
    (start = startIdx, splitters = splitters)

  def advance(curSet: Set[Int], splitSet: Set[Int]): (Int, Set[Int]) =
    var c = 0
    val r =
      curSet.flatMap: i =>
        if splitSet(i) then
          c += 1
          Set(i - 1, i + 1)
        else Set(i)
    (c, r)

  def part1(input: Input): Long =
    val startSet = Set(input.start)
    input.splitters.foldLeft((0, startSet)):
      case ((splits, acc), splitters) =>
        val (newSplits, newAcc) = advance(acc, splitters)
        (splits + newSplits, newAcc)
    ._1

  def advancePart2(splitters: Set[Int]): Kleisli[Chain, Int, Int] =
    Kleisli: point =>
      if splitters(point) then Chain(point - 1, point + 1)
      else Chain.one(point)

  def part2(input: Input): Long =

    input.splitters.foldLeft(Map(input.start -> 1L)): (curBeams, splitters) =>
      curBeams.iterator.map: (idx, n) =>
        if splitters(idx) then Chain(idx - 1 -> n, idx + 1 -> n)
        else Chain.one(idx -> n)
      .toList.combineAll.foldLeft(Map.empty[Int, Long]):
        case (acc, (k, v)) =>
          acc.updatedWith(k):
            case Some(x) => Some(x + v)
            case None    => Some(v)
    .values.sum
