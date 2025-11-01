package gay.menkissing.advent

import gay.menkissing.common.debugTiming

trait Problem[Input, +Output] extends ProblemAdv[Input, Output, Output]

trait NewProblem[Input, +Output]
    extends ProblemSuperAdv[Input, Input, Output, Output]

trait ParseP1[Input]:
  def parseP1(str: String): Input

trait ParseP2[Input]:
  def parseP2(str: String): Input

trait WithParser[Input] extends ParseP1[Input], ParseP2[Input]:
  override def parseP1(str: String): Input = parse(str)
  override def parseP2(str: String): Input = parse(str)

  def parse(str: String): Input

trait WithPart1[Input, +Output]:
  def part1(input: Input): Output

trait WithPart2[Input, +Output]:
  def part2(input: Input): Output

trait WithInput:
  lazy val input: String

extension [Input, Output]
  (
    x: WithPart1[Input, Output] & WithInput & ParseP1[Input]
  )
  def fullPart1: Output = x.part1(x.parseP1(x.input))
  def debugAndTimeP1(): Unit =
    val res = debugTiming(fullPart1)
    println(res)

extension [Input, Output]
  (
    x: WithPart2[Input, Output] & WithInput & ParseP2[Input]
  )
  def fullPart2: Output = x.part2(x.parseP2(x.input))
  def debugAndTimeP2(): Unit =
    val res = debugTiming(fullPart2)
    println(res)

trait NewHalfDay[Input, +Output]
    extends ParseP1[Input],
      WithPart1[Input, Output],
      WithInput:
  def main(args: Array[String]): Unit = this.debugAndTimeP1()

// HACK HACK HACK!!!!
trait HalfDay[Input, +Output]
    extends NewHalfDay[Input, Output],
      WithParser[Input]

trait ProblemSuperAdv[InputP1, InputP2, +OutputP1, +OutputP2]
    extends NewHalfDay[InputP1, OutputP1],
      WithPart2[InputP2, OutputP2],
      ParseP2[InputP2]:
  override def main(args: Array[String]): Unit =
    this.debugAndTimeP1()
    this.debugAndTimeP2()

trait ProblemUniqueInputs[InputP1, InputP2, +Output]
    extends ProblemSuperAdv[InputP1, InputP2, Output, Output]

trait ProblemAdv[Input, +OutputP1, +OutputP2]
    extends ProblemSuperAdv[Input, Input, OutputP1, OutputP2],
      HalfDay[Input, OutputP1]

trait WriteupAdv[Input, +OutputP1, +OutputP2]:
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

trait Writeup[Input, Output] extends WriteupAdv[Input, Output, Output]
