package gay.menkissing.advent
package y2024

import gay.menkissing.common.*

object Day07 extends Problem[List[Day07.Equation], Long]:
  lazy val input = FileIO.getInput(2024, 7)

  case class Equation(result: Long, inputs: List[Long]):
    def canBeTrue: Boolean =
      assert(inputs.sizeIs > 1)
      calculateTruth(inputs, result, 0)

    def canBeTrueP2: Boolean =
      assert(inputs.sizeIs > 1)
      calculateTruth(inputs, result, 0, true)

    def applyCombo(combo: String): Long =
      assert(inputs.sizeIs == combo.length + 1)
      inputs.tail.zipWithIndex.foldLeft(inputs.head):
        case (l, (r, idx)) =>
          combo(idx) match
            case '*' => l * r
            case '+' => l + r
            case '|' => (l.toString ++ r.toString).toLong
            case _   => ???

  override def parse(str: String): List[Equation] =
    str.linesIterator.map:
      case s"$a: $nums" =>
        Equation(a.toLong, nums.trim.split(' ').map(_.toLong).toList)
      case _ => whatTheScallop.!
    .toList

  def combinationsOfGeneric(n: Int, availableChars: Seq[Char]): Seq[String] =
    @annotation.tailrec
    def intlCombosOf(m: Int, curOnes: Seq[String]): Seq[String] =
      m match
        case 0 => curOnes
        case _ =>
          val newOnes =
            availableChars.map(c => curOnes.map(_ + c)).reduce(_ ++ _)
          intlCombosOf(m - 1, newOnes)

    if n == 0 then Seq()
    else intlCombosOf(n - 1, availableChars.map(_.toString))

  def concatLong(l: Long, r: Long): Long =
    val rDigits = math.log10(r.toDouble).toInt + 1
    (l * math.pow(10, rDigits)).toLong + r

  // possibly breaks if values contains a 0
  def calculateTruth
    (
      values: List[Long],
      finalResult: Long,
      runningResult: Long,
      part2: Boolean = false
    ): Boolean =
    values match
      case Nil       => finalResult == runningResult
      case v :: next =>
        (runningResult <= finalResult) &&
        (
          calculateTruth(next, finalResult, runningResult + v, part2) ||
            calculateTruth(next, finalResult, runningResult * v, part2) ||
            (if part2 then
               calculateTruth(
                 next,
                 finalResult,
                 concatLong(runningResult, v),
                 part2
               )
             else false)
        )

  def combinationsOfP1(n: Int): Seq[String] =
    combinationsOfGeneric(n, Seq('+', '*'))
  def combinationsOfP2(n: Int): Seq[String] =
    combinationsOfGeneric(n, Seq('+', '*', '|'))

  override def part1(input: List[Equation]): Long =
    input.withFilter(_.canBeTrue).map(_.result).sum

  override def part2(input: List[Equation]): Long =
    input.withFilter(_.canBeTrueP2).map(_.result).sum
