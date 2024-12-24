package gay.menkissing.advent

import gay.menkissing.common.*

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.Source

object Day17 extends ProblemAdv[Day17.ComputerState, String, Long]:
  case class ComputerState(ip: Int, program: Vector[Byte], regA: Long, regB: Long, regC: Long, outputs: List[Byte]):
    def advancePtr: ComputerState = copy(ip = ip + 2)

    @tailrec
    final def completeState: ComputerState =
      if (ip < program.size)
        step.completeState
      else
        copy(outputs = outputs.reverse)
    final def complete: List[Byte] =
      completeState.outputs


    final def completeP2: Boolean =
      @tailrec
      def go(i: ComputerState): Boolean =
        if (i.ip < i.program.size)
          i.stepP2 match
            case Some(x) => go(x)
            case None => false
        else
          i.outputs.reverse.toVector == i.program

      if ((regA.binaryDigits ceilDiv 3) != program.size)
        false
      else
        go(this)

    def stepP2: Option[ComputerState] =
      val goodStep = step
      if program(ip) == 5 then
        val size = goodStep.outputs.size
        if program(size - 1) != goodStep.outputs.head then
          None
        else
          Some(goodStep)
      else
        Some(goodStep)

    def step: ComputerState =
      val Vector(opcode, operand) = program.slice(ip, ip + 2)
      def comboOperand =
        operand match
          case 0 => 0L
          case 1 => 1L
          case 2 => 2L
          case 3 => 3L
          case 4 => regA
          case 5 => regB
          case 6 => regC
          case 7 => assert(false)
          case _ => ???
      opcode match
        // adv
        // this is the only place that regA gets set :thinking:
        case 0 => advancePtr.copy(regA = regA >> comboOperand)
        // bxl bitwise xor b
        case 1 => advancePtr.copy(regB = regB ^ operand)
        // bst modulo 8
        case 2 => advancePtr.copy(regB = comboOperand & 0b111)
        // jnz
        case 3 => if (regA == 0) advancePtr else copy(ip = operand)
        // bxc
        case 4 => advancePtr.copy(regB = regB ^ regC)
        // out
        case 5 => advancePtr.copy(outputs = outputs.prepended((comboOperand & 0b111).toByte))
        // bdv
        case 6 => advancePtr.copy(regB = regA >> comboOperand)
        // cdv
        case 7 => advancePtr.copy(regC = regA >> comboOperand)

  override def parse(str: String): ComputerState =
    val Array(regs, program) = str.split("\n\n")
    val List(regA, regB, regC) = regs.linesIterator.map:
      case s"Register $_: $a" => a.toLong
    .toList
    val s"Program: $programStr" = program.trim : @unchecked

    ComputerState(0, programStr.split(',').map(_.toByte).toVector, regA, regB, regC, List())

  override def part1(input: ComputerState): String =
    input.complete.mkString("", ",", "")

  override def part2(input: ComputerState): Long =
    Iterator.iterate(1L): a =>
      if (input.program.endsWith(input.copy(regA = a).complete)) a << 3 else if (a % 8 < 7) a + 1 else (a >> 3) + 1
    .find(it => input.copy(regA = it).completeP2).get

  override val input: String = Source.fromResource("day17.txt").mkString
  /*
  def testPrograms(): Unit = {
    {
      val data = ComputerState(0, Vector(2, 6), 0, 0, 9, List())
      assert(data.completeState.regB == 1)
    }
    {
      val data = ComputerState(0, Vector(5,0,5,1,5,4), 10, 0, 0, List())
      assert(data.complete == List(0, 1, 2))
    }
    {
      val data = ComputerState(0, Vector(0, 1, 5, 4, 3, 0), 2024, 0, 0, List())
      val finalState = data.completeState
      assert(finalState.outputs == List(4,2,5,6,7,7,7,7,3,1,0))
      // assert(finalState.regA == 0)
    }
    {
      val data = ComputerState(0, Vector(1, 7), 0, 29, 0, List())
      // assert(data.completeState.regB == 26)
    }
    {
      val data = ComputerState(0, Vector(4, 0), 0, 2024, 43690, List())
      // assert(data.completeState.regB == 44354)
    }

  }
  */

/*
@main def main(): Unit =
  // Day17.testPrograms()
  Day17.debugAndTimeP1()
  Day17.debugAndTimeP2()
*/