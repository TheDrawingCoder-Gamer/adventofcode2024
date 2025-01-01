package gay.menkissing.advent

import gay.menkissing.common.debugTiming

trait Problem[Input, +Output] extends ProblemAdv[Input, Output, Output]


trait WithParser[Input] {
  def parse(str: String): Input
}

trait WithPart1[Input, +Output] {
  def part1(input: Input): Output
}

trait WithPart2[Input, +Output] {
  def part2(input: Input): Output
}

trait WithInput {
  lazy val input: String
}

extension[Input, Output] (x: WithPart1[Input, Output] & WithInput & WithParser[Input])
  def fullPart1: Output = x.part1(x.parse(x.input))
  def debugAndTimeP1(): Unit =
    val res = debugTiming(fullPart1)
    println(res)

extension[Input, Output](x: WithPart2[Input, Output] & WithInput & WithParser[Input])
  def fullPart2: Output = x.part2(x.parse(x.input))
  def debugAndTimeP2(): Unit =
    val res = debugTiming(fullPart2)
    println(res)

trait HalfDay[Input, +Output] extends WithParser[Input], WithPart1[Input, Output], WithInput {
  def main(args: Array[String]): Unit =
    this.debugAndTimeP1()
}

trait ProblemAdv[Input, +OutputP1, +OutputP2] extends HalfDay[Input, OutputP1], WithPart2[Input, OutputP2] {
  def parse(str: String): Input

  def part1(input: Input): OutputP1
  def part2(input: Input): OutputP2

  lazy val input: String
  
  
  override def main(args: Array[String]): Unit =
    this.debugAndTimeP1()
    this.debugAndTimeP2()
}

trait WriteupAdv[Input, +OutputP1, +OutputP2] {
  def parse(str: String): Input
  
  def part1(str: String): OutputP1
  def part2(str: String): OutputP2
  
  lazy val input: String

  def fullPart1: OutputP1 = part1(input)
  def fullPart2: OutputP2 = part2(input)
  
  def debugAndTimeP1(): Unit =
    val res = debugTiming(fullPart1)
    println(res)

  def debugAndTimeP2(): Unit =
    val res = debugTiming(fullPart2)
    println(res)
  
  final def main(args: Array[String]): Unit =
    debugAndTimeP1()
    debugAndTimeP2()
}

trait Writeup[Input, Output] extends WriteupAdv[Input, Output, Output]
