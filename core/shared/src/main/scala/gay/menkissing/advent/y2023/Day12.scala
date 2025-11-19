package gay.menkissing.advent
package y2023

import gay.menkissing.common.*
import cats.implicits.*

object Day12 extends Problem:
  type Input = List[(List[Option[Boolean]], List[Int])]
  type Output = Long
  def input = FileIO.getInput(2023, 12)

  def parse(str: String): List[(List[Option[Boolean]], List[Int])] =
    str.linesIterator.map:
      case s"$pat $is" =>
        (
          pat.map:
            case '.' => Some(false)
            case '#' => Some(true)
            case '?' => None
          .toList,
          is.split(",").map(_.toInt).toList
        )
    .toList

  val count: (List[Option[Boolean]], List[Int], Int) => Long =
    Memo.memoize: (input, ds, d) =>
      if input.isEmpty then
        if ds.isEmpty && d == 0 then 1L
        else if ds.length == 1 && ds.head == d then 1L
        else 0
      else
        def good() =
          if d == 0 then count(input.tail, ds, 0)
          else if !ds.isEmpty && ds.head == d then count(input.tail, ds.tail, 0)
          else 0L
        def bad() =
          if ds.isEmpty then 0L
          else if d == ds.head then 0L
          else count(input.tail, ds, d + 1)
        input.head match
          case Some(true)  => bad()
          case Some(false) => good()
          case None        => bad() + good()

  def part1(input: List[(List[Option[Boolean]], List[Int])]): Long =
    input.map: (pat, groups) =>
      count(pat, groups, 0)
    .sum

  def unfoldRow(row: List[Option[Boolean]]): List[Option[Boolean]] =
    List.fill(5)(row).intercalate(List(None))

  def part2(input: List[(List[Option[Boolean]], List[Int])]): Long =
    input.map: (pat, groups) =>
      count(unfoldRow(pat), List.fill(5)(groups).flatten, 0)
    .sum
