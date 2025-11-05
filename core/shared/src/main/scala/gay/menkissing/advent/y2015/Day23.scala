package gay.menkissing.advent
package y2015

import gay.menkissing.common.*

object Day23 extends Problem:
  type Input = Vector[Instruction]
  type Output = Int

  case class State(a: Int, b: Int, pc: Int):
    def getRegister(c: Char): Int =
      c match
        case 'a' => a
        case 'b' => b
        case _   => whatTheScallop.!
    def updateRegister(c: Char, f: Int => Int): State =
      c match
        case 'a' => copy(a = f(a))
        case 'b' => copy(b = f(b))
        case _   => whatTheScallop.!
    def incrPC: State = copy(pc = pc + 1)
    def jump(offset: Int): State = copy(pc = pc + offset)

  enum Instruction:
    case Half(r: Char)
    case Triple(r: Char)
    case Incr(r: Char)
    case Jump(offset: Int)
    case JumpIfEven(r: Char, offset: Int)
    case JumpIfOne(r: Char, offset: Int)

    def advance(state: State): State =
      this match
        case Half(r)               => state.updateRegister(r, _ / 2).incrPC
        case Triple(r)             => state.updateRegister(r, _ * 3).incrPC
        case Incr(r)               => state.updateRegister(r, _ + 1).incrPC
        case Jump(offset)          => state.jump(offset)
        case JumpIfEven(r, offset) =>
          if (state.getRegister(r) & 1) == 0 then state.jump(offset)
          else state.incrPC

        case JumpIfOne(r, offset) =>
          if state.getRegister(r) == 1 then state.jump(offset)
          else state.incrPC

  lazy val input = FileIO.getInput(2015, 23)

  def parse(str: String): Input =
    str.linesIterator.map:
      case s"hlf $c"          => Instruction.Half(c.head)
      case s"tpl $c"          => Instruction.Triple(c.head)
      case s"inc $r"          => Instruction.Incr(r.head)
      case s"jmp $offset"     => Instruction.Jump(offset.toInt)
      case s"jie $r, $offset" => Instruction.JumpIfEven(r.head, offset.toInt)
      case s"jio $r, $offset" => Instruction.JumpIfOne(r.head, offset.toInt)
    .toVector

  def run(in: Vector[Instruction], init: State): State =
    var state = init

    while in.isDefinedAt(state.pc) do state = in(state.pc).advance(state)

    state

  def part1(input: Vector[Instruction]): OutputP1 = run(input, State(0, 0, 0)).b

  def part2(input: Vector[Instruction]): OutputP2 = run(input, State(1, 0, 0)).b
