package gay.menkissing.advent
package y2022

import gay.menkissing.common.Grid
import cats.implicits.*
import cats.*
import cats.data.*


object Day10 extends ProblemAdv[List[Day10.Operation], Int, String]:
  // I just KNOW it will tell me there is more than one register for part two
  case class CPU(tick: Int, register: Int)

  sealed trait Operation
  case object Noop extends Operation
  case class Addx(n: Int) extends Operation

  def opTakesCycles(op: Operation): Int =
    op match
      case Noop => 1
      case Addx(_) => 2

  lazy val input = FileIO.getInput(2022, 10)

  def parse(input: String): List[Operation] =
    input.linesIterator.map:
      case "noop" => Noop
      case s"addx $x" =>
        Addx(x.toInt)
      case _ => assert(false)
    .toList


  type CPUState[A] = State[CPU, A]

  def debugPrintInput(input: List[Operation]): String =
    val vec = input.zipWithIndex.traverse((l, i) => executeOp(l).map(z => (z, i))).runA(CPU(0, 1)).value.toVector


    input.zipWithIndex.map:
      case (op, idx) =>
        val c = vec.find(_._2 == idx)
        c match
          case Some((cpu, _)) => s"$op // tick ${cpu.tick}, reg ${cpu.register}"
          case _ => op.toString
    .mkString("\n")


  def generateCpus(input: List[Operation]): Vector[CPU] =
    input.traverse(executeOp).runA(CPU(0, 1)).value.toVector

  def executeOp(op: Operation): State[CPU, CPU] = State { it =>
    val newRegister =
      op match
        case Noop => it.register
        case Addx(n) =>  it.register + n
    val newCpu = CPU(it.tick + opTakesCycles(op), newRegister)
    (newCpu, newCpu)
  }

  def getCycle(cpus: Vector[CPU])(c: Int): CPU = {

    lazy val firstIndex = cpus.indexWhere(_.tick >= c)

    if firstIndex == -1 then
      cpus.last
    else
      cpus.get(firstIndex - 1).getOrElse(CPU(0, 1))
  }
  def getAndNormalize(cpus: Vector[CPU])(c: Int): CPU = {
    val cpu = getCycle(cpus)(c)
    cpu.copy(tick = c)
  }
  def signalStrengthPart1(cycle: Int)(cpu: CPU): Int =
    cycle * cpu.register


  def part1(input: List[Operation]): Int =
    val cpus = generateCpus(input)
    // println(getCycle(cpus)(223))
    (20 to 220 by 40).map: c =>

      val str = signalStrengthPart1(c)(getCycle(cpus)(c))
      // println(getCycle(cpus)(c))
      // println(s"$c: $str")
      str
    .sum

  type CRT = Grid[Boolean]

  // This works. Why.
  def part2(input: List[Operation]): String =
    val cpus = generateCpus(input)
    val normalizedCpus = {
      val lastCycle = cpus.last.tick
      (1 to math.min(lastCycle, 240)).map(getAndNormalize(cpus)).toVector
    }
    val solved2 = normalizedCpus.traverse(processCPU).map(_.last)
    val crt = solved2.runA(Grid[Boolean](List.fill(6, 40)(false))).value
    prettyShowBoolGrid(crt)
  def processCPU(cpu: CPU): State[CRT, CRT] = State { crt =>
    val zeroTick = cpu.tick - 1
    val good = (cpu.register - 1 to cpu.register + 1).contains(zeroTick % crt.width)
    val newCrt = crt.updated(zeroTick)(good)
    (newCrt, newCrt)
  }

  def prettyShowBoolGrid(grid: Grid[Boolean]): String = {
    grid.rows.map { it =>
      it.map(if (_) '#' else '.').mkString
    }.mkString("\n")
  }
