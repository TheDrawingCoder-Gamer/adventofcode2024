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
    val arr = str.linesIterator.toArray
    val terms =
      arr.init.map(_.trim.split(raw"\s+").map(_.toLong).toList).toList.transpose
    val operators = arr.last.trim.split(raw"\s+").map(_.head)
    terms.zip(operators).map: (terms, operator) =>
      (terms = terms, operator = operator)
    .toList

  def parseP2(str: String): List[SquidProblem] =
    val arr = str.linesIterator.toArray
    val termColumns =
      arr.init.map:
        _.map(it => Option.unless(it.isSpaceChar)(it.asDigit)).toList
      .toList.transpose.map: it =>
        val flattened = it.flatten
        // if our entire thing is empty then we got a blank column
        if flattened.isEmpty then -1
        // otherwise we have a number
        else flattened.foldLeft(0)((acc, n) => acc * 10 + n)
    val groupedTerms = termColumns.splitOn(-1)
    val operators = arr.last.trim.split(raw"\s+").map(_.head)
    groupedTerms.zip(operators).map: (terms, operator) =>
      (terms = terms.map(_.toLong), operator = operator)

  def calculate(input: List[SquidProblem]): Long =
    input.map:
      case (terms, '*') => terms.product
      case (terms, '+') => terms.sum
    .sum
  def part1(input: List[SquidProblem]): Long = calculate(input)

  def part2(input: List[SquidProblem]): Long = calculate(input)
