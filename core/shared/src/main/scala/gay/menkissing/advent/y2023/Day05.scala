package gay.menkissing.advent
package y2023

import cats.data.{NonEmptyList, NonEmptyChain, Ior}
import cats.syntax.all.*
import gay.menkissing.common.*, ArityN.*

object Day05 extends Problem:
  // was already here and declared as input so it ALL WORKS OUT : )
  type Input = (List[Long], NonEmptyList[NonEmptyList[MapRange]])
  type Output = Long

  type RangeResult =
    Ior[NonEmptyChain[(Long, Long)], NonEmptyChain[(Long, Long)]]

  def chainRangeResult
    (
      f: ((Long, Long)) => RangeResult,
      g: ((Long, Long)) => RangeResult
    ): ((Long, Long)) => RangeResult =
    f.andThen[RangeResult]:
      case Ior.Left(v)    => Ior.Left(v)
      case Ior.Right(vs)  => vs.map(g).reduce
      case Ior.Both(l, r) => r.map(g).reduce.addLeft(l)

  final case class MapRange(destStart: Long, srcStart: Long, len: Long):
    val single: PartialFunction[Long, Long] =
      // ok i geniunely didnt know u could drop the brackets around partial functions
      // the more you know?
      case x if x >= srcStart && x < (srcStart + len) =>
        (x - srcStart) + destStart
    val singleOpt = single.lift
    // Left represents mapped, Right is unmapped
    def range(t: (Long, Long)): RangeResult =
      val (min, max) = t
      (singleOpt(min), singleOpt(max)) match
        case (Some(mn), Some(mx)) => Ior.Left(NonEmptyChain.one((mn, mx)))
        case (Some(mn), None)     =>
          // Our max is outside the bounds. Return two, one mapped, other not
          Ior.Both(
            NonEmptyChain.one((mn, single(srcStart + len - 1))),
            NonEmptyChain.one((srcStart + len, max))
          )
        case (None, Some(mx)) =>
          // Our min is outside the bounds. Return two, one mapped, other not
          Ior.Both(
            NonEmptyChain.one((single(srcStart), mx)),
            NonEmptyChain.one((min, srcStart - 1))
          )
        case (None, None) =>
          // Either out of range, or encompass full range
          if max >= srcStart + len && min < srcStart then
            Ior.Both(
              NonEmptyChain.one((single(srcStart), single(srcStart + len - 1))),
              NonEmptyChain((min, srcStart - 1), (srcStart + len, max))
            )
          else if max < srcStart || min >= srcStart + len then
            Ior.Right(NonEmptyChain.one((min, max)))
          else !!!

  def input: String = FileIO.getInput(2023, 5)
  // List instead of map - input is a continuous chain
  def parse(str: String): (List[Long], NonEmptyList[NonEmptyList[MapRange]]) =
    val head :: rest = str.split("\n\n").toList.runtimeChecked
    val seeds =
      head match
        case s"seeds: $ss" => ss.trim.split(" ").map(it => it.toLong).toList
        case _             => ???
    val mapRanges =
      rest.map: group =>
        val ranges =
          NonEmptyList.fromListUnsafe(group.linesIterator.toList.tail)
        ranges.map:
          case s"$ds $ss $l" => MapRange(ds.toLong, ss.toLong, l.toLong)
    (seeds, NonEmptyList.fromListUnsafe(mapRanges))

  def part1(input: (List[Long], NonEmptyList[NonEmptyList[MapRange]])): Long =
    val (seeds, ranges) = input
    val seedToLocationFunc =
      ranges.map(
        _.map(_.single).reduceLeft((l, r) => l.orElse(r))
          .orElse(PartialFunction.fromFunction(identity))
      ).reduceLeft((l, r) => l.andThen(r))

    seeds.map(seedToLocationFunc.apply).min

  def part2(input: (List[Long], NonEmptyList[NonEmptyList[MapRange]])): Long =
    val (seeds, ranges) = input
    val seedRanges =
      seeds.groupedN[2].map:
        case (l, r) => (l, l + r)
    val seedToLocationFunc =
      ranges.map(
        _.map(_.range).reduceLeft((l, r) => chainRangeResult(l, r))
          .andThen(_.merge)
      ).reduceLeft((l, r) => l.andThenF(r))
    NonEmptyChain.fromSeq(seedRanges).get
      .flatMap[(Long, Long)](it => seedToLocationFunc(it)).minimumBy(_._1)._1
