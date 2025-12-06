package gay.menkissing.advent
package y2025

import gay.menkissing.common.*

object Day06 extends ProblemUniqueInputs:

  type SquidProblem = (terms: List[Long], operator: Char)

  type InputP1 = List[SquidProblem]
  type InputP2 = List[SquidProblem]
  type Output = Long

  def input: String = FileIO.getInput(2025, 6)

  def parseP1(str: String): List[SquidProblem] =
    str.linesIterator.toList.map(_.split(raw"\s+").toList).transpose.map:
      problem =>
        (terms = problem.init.map(_.toLong), operator = problem.last.head)

  def parseP2(str: String): List[SquidProblem] =
    val ls = str.linesIterator.toList
    val termColumns =
      ls.init.map:
        _.map(it => Option.unless(it.isSpaceChar)(it.asDigit)).toList
      .transpose.map:
        _.flatten match
          case Nil => -1
          case vs  => vs.foldLeft(0)((acc, n) => acc * 10 + n)
    val groupedTerms = termColumns.splitOn(-1)
    val operators = ls.last.trim.split(raw"\s+").map(_.head)
    groupedTerms.zip(operators).map: (terms, operator) =>
      (terms = terms.map(_.toLong), operator = operator)

  def calculate(input: List[SquidProblem]): Long =
    input.map:
      case (terms, '*') => terms.product
      case (terms, '+') => terms.sum
    .sum
  def part1(input: List[SquidProblem]): Long = calculate(input)

  def part2(input: List[SquidProblem]): Long = calculate(input)
