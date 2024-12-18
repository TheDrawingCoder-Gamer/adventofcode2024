package gay.menkissing.advent

import gay.menkissing.common.debugTiming

trait Problem[Input, Output] extends ProblemAdv[Input, Output, Output]

trait ProblemAdv[Input, OutputP1, OutputP2] {
  def parse(str: String): Input

  def part1(input: Input): OutputP1
  def part2(input: Input): OutputP2

  def input: String

  def fullPart1: OutputP1 = part1(parse(input))
  def fullPart2: OutputP2 = part2(parse(input))


  def debugAndTimeP1(): Unit =
    val res = debugTiming(fullPart1)
    println(res)

  def debugAndTimeP2(): Unit =
    val res = debugTiming(fullPart2)
    println(res)
}
