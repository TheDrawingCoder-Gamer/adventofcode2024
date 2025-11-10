package gay.menkissing.advent
package y2024

import gay.menkissing.common.*
import cats.*

import scala.annotation.tailrec

object Day17 extends ProblemAdv, FormatP1:
  type Input = ComputerState
  type OutputP1 = List[Byte]
  type OutputP2 = Long

  def formatP1(out: List[Byte]): String = out.mkString(",")

  final case class ComputerState
    (
      ip: Int,
      program: Vector[Byte],
      regA: Long,
      regB: Long,
      regC: Long,
      outputs: List[Byte]
    ):
    def advancePtr: ComputerState = copy(ip = ip + 2)

    @tailrec
    def completeState: ComputerState =
      if ip < program.size then step.completeState
      else copy(outputs = outputs.reverse)
    def complete: List[Byte] = completeState.outputs

    def completeP2: Boolean =
      @tailrec
      def go(i: ComputerState): Boolean =
        if i.ip < i.program.size then
          i.stepP2 match
            case Some(x) => go(x)
            case None    => false
        else i.outputs.reverse.toVector == i.program

      if (regA.binaryDigits ceilDiv 3) != program.size then false
      else go(this)

    def stepP2: Option[ComputerState] =
      val goodStep = step
      if program(ip) == 5 then
        val size = goodStep.outputs.size
        if program(size - 1) != goodStep.outputs.head then None
        else Some(goodStep)
      else Some(goodStep)

    def step: ComputerState =
      val opcode = program(ip)
      val operand = program(ip + 1)
      def comboOperand =
        operand match
          case 0 => 0L
          case 1 => 1L
          case 2 => 2L
          case 3 => 3L
          case 4 => regA
          case 5 => regB
          case 6 => regC
          case 7 => !!!
      opcode match
        // adv
        // this is the only place that regA gets set :thinking:
        case 0 => advancePtr.copy(regA = regA >> comboOperand)
        // bxl bitwise xor b
        case 1 => advancePtr.copy(regB = regB ^ operand)
        // bst modulo 8
        case 2 => advancePtr.copy(regB = comboOperand & 0b111)
        // jnz
        case 3 => if regA == 0 then advancePtr else copy(ip = operand)
        // bxc
        case 4 => advancePtr.copy(regB = regB ^ regC)
        // out
        case 5 =>
          advancePtr
            .copy(outputs = outputs.prepended((comboOperand & 0b111).toByte))
        // bdv
        case 6 => advancePtr.copy(regB = regA >> comboOperand)
        // cdv
        case 7 => advancePtr.copy(regC = regA >> comboOperand)

  override def parse(str: String): ComputerState =
    val Array(regs, program) = str.split("\n\n")
    val List(regA, regB, regC) =
      regs.linesIterator.map:
        case s"Register $_: $a" => a.toLong
      .toList
    val s"Program: $programStr" = program.trim: @unchecked

    ComputerState(
      0,
      programStr.split(',').map(_.toByte).toVector,
      regA,
      regB,
      regC,
      List()
    )

  override def part1(input: ComputerState): List[Byte] = input.complete

  override def part2(input: ComputerState): Long =
    Iterator.iterate(1L): a =>
      if input.program.endsWith(input.copy(regA = a).complete) then a << 3
      else if a % 8 < 7 then a + 1
      else (a >> 3) + 1
    .find(it => input.copy(regA = it).completeP2).get

  override lazy val input: String = FileIO.getInput(2024, 17)
