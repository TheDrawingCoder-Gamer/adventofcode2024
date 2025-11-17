package gay.menkissing.advent
package y2020

import gay.menkissing.common.*
import collection.mutable

object Day23 extends Problem:
  type Input = Vector[Int]
  type Output = Long

  lazy val input = FileIO.getInput(2020, 23)

  val debugger = Debuginator(active = false)

  def parse(str: String): Input = str.trim.map(_.asDigit).toVector

  def wrapBounds(min: Int, max: Int)(v: Int): Int = if v < min then max else v

  def wrap(v: Int): Int =
    val x = (v % 9)
    if x == 0 then 9 else x
  def selectDest(start: Int, invalid: Vector[Int]): Int =
    if !invalid.contains(start) then start
    else selectDest(wrap(start - 1), invalid)

  def extract
    (v: Vector[Int], from: Int, until: Int): (Vector[Int], Vector[Int]) =
    val start = v.take(from)
    val middle = v.slice(from, until)
    val end = v.drop(until)
    (start ++ end, middle)
  def inserted(v: Vector[Int], c: Vector[Int], at: Int): Vector[Int] =
    v.take(at) ++ c ++ v.drop(at)

  def extractX(v: Vector[Int], start: Int): (Vector[Int], Vector[Int]) =
    if start >= v.length - 3 then
      val pickUpL = v.drop(start)
      val takeFirst = (start + 3) % v.length
      val pickUpR = v.take(takeFirst)
      val middle = v.drop(takeFirst).take(v.length - 3)
      (middle, pickUpL ++ pickUpR)
    else extract(v, start, start + 3)

  def step(input: Vector[Int], curIdx: Int): (Vector[Int], Int) =
    val label = input(curIdx)
    val (rem, pickedUp) = extractX(input, curIdx + 1)
    debugger.verbose(input)
    debugger.verbose(rem)
    debugger.verbose(pickedUp)
    val destLabel = selectDest(wrap(label - 1), pickedUp)
    val destIdx = rem.indexOf(destLabel)
    val ins = inserted(rem, pickedUp, destIdx + 1)
    debugger.verbose(ins)
    if ins.length != input.length then ???
    (ins, (ins.indexOf(label) + 1) % 9)

  def part1(input: Vector[Int]): OutputP1 =
    val res = step.tupled.repeated(100)((input, 0))
    ForeverIterator(res._1.iterator).dropWhile(_ != 1).drop(1).take(8).toList
      .mkString("").toInt

  def part2(input: Vector[Int]): Long =
    val paddedInput = input ++ Vector.range(10, 1_000_001)
    val nextCup =
      paddedInput.zip(paddedInput.tail.appended(paddedInput.head))
        .to(mutable.Map)
    val min = paddedInput.min
    val max = paddedInput.max
    def grabNextItems(nextCup: mutable.Map[Int, Int], c: Int): Vector[Int] =
      val v1 = nextCup(c)
      val v2 = nextCup(v1)
      val v3 = nextCup(v2)
      Vector(v1, v2, v3)
    val _ = (0 until 10_000_000).foldLeft(paddedInput.head):
      case (current, _) =>
        val pickedUp = grabNextItems(nextCup, current)
        val dest =
          Iterator.iterate(current - 1)(x => wrapBounds(min, max)(x - 1))
            .find(it => it >= min && !pickedUp.contains(it)).get
        nextCup(current) = nextCup(pickedUp(2))
        nextCup(pickedUp(2)) = nextCup(dest)
        nextCup(dest) = pickedUp(0)
        nextCup(current)
    val c1 = nextCup(1)
    val c2 = nextCup(c1)
    c1.toLong * c2.toLong
