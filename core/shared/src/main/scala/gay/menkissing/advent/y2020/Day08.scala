package gay.menkissing.advent
package y2020

import collection.mutable as mut
import gay.menkissing.common.*

object Day08 extends Problem[Vector[Day08.Instruction], Int]:
  enum Instruction:
    case Acc(n: Int)
    case Jmp(n: Int)
    case Nop(n: Int)

  lazy val input = FileIO.getInput(2020, 8)

  def parse(input: String): Vector[Instruction] =
    input.linesIterator.map:
      case s"acc $n" => Instruction.Acc(n.toInt)
      case s"jmp $n" => Instruction.Jmp(n.toInt)
      case s"nop $n" => Instruction.Nop(n.toInt)
    .toVector

  case class CPU(acc: Int, ip: Int)

  import Instruction.*

  // Solves the halting problem!
  def execute(input: Vector[Instruction]): Either[Int, Int] =
    val visited = mut.Set.empty[Int]
    var acc = 0
    var ip = 0
    while !visited(ip) && ip < input.length do
      visited += ip
      input(ip) match
        case Acc(by) =>
          acc += by
          ip += 1
        case Jmp(by) => ip += by
        case Nop(by) => ip += 1
    if ip >= input.length then Right(acc)
    else Left(acc)

  def part1(input: Vector[Instruction]): Int = execute(input).leftOrDie

  def part2(input: Vector[Instruction]): Int =
    input.zipWithIndex.findMap:
      case (Acc(by), idx) => None
      case (Jmp(by), idx) => execute(input.updated(idx, Nop(by))).toOption
      case (Nop(by), idx) => execute(input.updated(idx, Jmp(by))).toOption
    .get
