package gay.menkissing.advent

import gay.menkissing.common.debugTiming


trait Problem[Input, Output] {
  def parse(str: String): Input

  def part1(input: Input): Output
  def part2(input: Input): Output

  def input: String

  def fullPart1: Output = part1(parse(input))
  def fullPart2: Output = part2(parse(input))


  def debugAndTimeP1(): Unit =
    val res = debugTiming(fullPart1)
    println(res)

  def debugAndTimeP2(): Unit =
    val res = debugTiming(fullPart2)
    println(res)
}
