package gay.menkissing.advent
package y2022

import gay.menkissing.common.repeat

import scala.collection.mutable

// I totally forgot why this works - figure it out later!
object Day11 extends Problem:
  type Input = Vector[Monkey]
  type Output = Long

  final class Monkey
    (
      val n: Int,
      val items: mutable.ListBuffer[Long],
      val op: MonkeyOp,
      val divisor: Int,
      val throwTrue: Int,
      val throwFalse: Int,
      var inspected: Long = 0
    )

  enum MonkeyOp(l: Option[Long], r: Option[Long]):
    case Addition(l: Option[Long], r: Option[Long]) extends MonkeyOp(l, r)
    case Multiply(l: Option[Long], r: Option[Long]) extends MonkeyOp(l, r)

    def apply(worry: Long): Long =
      val gl = l.getOrElse(worry)
      val gr = r.getOrElse(worry)
      this match
        case Addition(_, _) => gl + gr
        case Multiply(_, _) => gl * gr

  lazy val input = FileIO.getInput(2022, 11)

  def parse(str: String): Vector[Monkey] =
    str.split("\n\n").map: it =>
      val xs = it.trim.linesIterator.toVector
      xs.map(_.trim).runtimeChecked match
        case Vector(
              s"Monkey $name:",
              s"Starting items:$startingItems",
              s"Operation: new = $operation",
              s"Test: divisible by $div",
              s"If true: throw to monkey $trueMonkey",
              s"If false: throw to monkey $falseMonkey"
            ) =>
          val goodOp =
            operation.trim match
              case s"$l $op $r" =>
                val cons =
                  op match
                    case "*" => MonkeyOp.Multiply.apply
                    case "+" => MonkeyOp.Addition.apply
                cons(l.toLongOption, r.toLongOption)

          Monkey(
            name.toInt,
            startingItems.split(',').map(_.trim.toLong).to(mutable.ListBuffer),
            goodOp,
            div.toInt,
            trueMonkey.toInt,
            falseMonkey.toInt
          )
    .toVector

  def calculate(monkeys: Vector[Monkey], sanityLoss: Int, rounds: Int): Long =
    val modulus = monkeys.map(_.divisor).product
    repeat(rounds):
      monkeys.foreach: m =>
        m.items.foreach: i =>
          val res = m.op(i)
          val res2 = (res / sanityLoss) % modulus
          val bool = res2 % m.divisor == 0
          val thingie = if bool then m.throwTrue else m.throwFalse
          m.inspected += 1
          monkeys(thingie).items.prepend(res2)
        m.items.clear()
    val monkeySums = monkeys.map(_.inspected)
    val Vector(l, r) = monkeySums.sorted.takeRight(2): @unchecked
    l * r

  override def part1(input: Vector[Monkey]): Long = calculate(input, 3, 20)

  override def part2(input: Vector[Monkey]): Long = calculate(input, 1, 10000)
