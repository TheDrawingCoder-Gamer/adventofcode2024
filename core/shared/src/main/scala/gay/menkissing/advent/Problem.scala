package gay.menkissing.advent

import gay.menkissing.common.debugTiming

trait Problem extends ProblemSuperAdv, WithParser, HasSharedOutput
trait NewProblem extends ProblemSuperAdv, HasSharedInput, HasSharedOutput

// final day that only has 1
trait HalfDay extends IncompleteProblem, WithParser, HasSharedOutput

trait HasInputP1:
  type InputP1
trait HasInputP2:
  type InputP2
trait HasSharedInput extends HasInputP1, HasInputP2:
  type Input
  type InputP1 = Input
  type InputP2 = Input
trait HasOutputP1:
  type OutputP1
object HasOutputP1:
  type Aux[O] = HasOutputP1 { type OutputP1 = O }
trait HasOutputP2:
  type OutputP2
object HasOutputP2:
  type Aux[O] = HasOutputP2 { type OutputP2 = O }
trait HasSharedOutput extends HasOutputP1, HasOutputP2:
  type Output
  type OutputP1 = Output
  type OutputP2 = Output

trait ParseP1 extends HasInputP1:
  def parseP1(str: String): InputP1

trait ParseP2 extends HasInputP2:
  def parseP2(str: String): InputP2

trait FormatP1 extends HasOutputP1:
  def formatP1(out: OutputP1): String

trait FormatP2 extends HasOutputP2:
  def formatP2(out: OutputP2): String

trait WithParser extends ParseP1, ParseP2, HasSharedInput:

  override def parseP1(str: String): Input = parse(str)
  override def parseP2(str: String): Input = parse(str)

  def parse(str: String): Input

trait WithPart1 extends HasInputP1, HasOutputP1:
  def part1(input: InputP1): OutputP1

object WithPart1:
  type WithOutput[O] = WithPart1 { type OutputP1 = O }

trait WithPart2 extends HasInputP2, HasOutputP2:
  def part2(input: InputP2): OutputP2

object WithPart2:
  type WithOutput[O] = WithPart2 { type OutputP2 = O }

trait WithInput:
  lazy val input: String

extension [Output](x: HasOutputP1.Aux[Output])
  def tryFormatP1(out: Output): String =
    x match
      case i: FormatP1 => i.formatP1(out)
      case _           => out.toString

extension [Output](x: HasOutputP2.Aux[Output])
  def tryFormatP2(out: Output): String =
    x match
      case i: FormatP2 => i.formatP2(out)
      case _           => out.toString

extension [Output]
  (
    x: WithPart1.WithOutput[Output] & WithInput & ParseP1
  )
  def fullPart1: Output = x.part1(x.parseP1(x.input))
  def debugAndTimeP1(): Unit =
    val input = x.input
    val res = debugTiming(x.part1(x.parseP1(input)))
    println(x.tryFormatP1(res))

extension [Output]
  (
    x: WithPart2.WithOutput[Output] & WithInput & ParseP2
  )
  def fullPart2: Output = x.part2(x.parseP2(x.input))
  def debugAndTimeP2(): Unit =
    val input = x.input
    val res = debugTiming(x.part2(x.parseP2(input)))
    println(x.tryFormatP2(res))

trait IncompleteProblem extends ParseP1, WithPart1, WithInput:
  def main(args: Array[String]): Unit = this.debugAndTimeP1()

trait ProblemSuperAdv extends IncompleteProblem, WithPart2, ParseP2:
  override def main(args: Array[String]): Unit =
    this.debugAndTimeP1()
    this.debugAndTimeP2()

trait ProblemUniqueInputs extends ProblemSuperAdv, HasSharedOutput

trait ProblemAdv extends ProblemSuperAdv, WithParser

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
