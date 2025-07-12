package gay.menkissing.advent.y2024

import cats.*
import cats.implicits.*
import gay.menkissing.advent.{FileIO, Problem}
import gay.menkissing.common
import gay.menkissing.common.*

import scala.annotation.experimental
import scala.collection.mutable as mut
import scala.io.Source

object Day21 extends Problem[List[String], Long]:
  override def parse(str: String): List[String] =
    str.linesIterator.toList


  trait Pad:
    val layout: Vector[String]

    def apply(loc: Vec2i): Char = layout(loc.y)(loc.x)
    def apply(key: Char): Vec2i

  object Numpad extends Pad:
    val layout: Vector[String] = Vector("789", "456", "123", " 0A")

    override def apply(key: Char): Vec2i =
      key match
        case '7' => Vec2i(0, 0)
        case '8' => Vec2i(1, 0)
        case '9' => Vec2i(2, 0)
        case '4' => Vec2i(0, 1)
        case '5' => Vec2i(1, 1)
        case '6' => Vec2i(2, 1)
        case '1' => Vec2i(0, 2)
        case '2' => Vec2i(1, 2)
        case '3' => Vec2i(2, 2)
        case ' ' => Vec2i(0, 3)
        case '0' => Vec2i(1, 3)
        case 'A' => Vec2i(2, 3)
        case _ => assert(false)
    def explodes(start: Char, path: String): Boolean =
      val startPos = numpadKeyToPoint(start)
      val deadPos = numpadKeyToPoint(' ')
      path.foldLeft((false, startPos)):
        case ((explodes, pos), c) =>
          if explodes then
            (true, pos)
          else
            val newPoint = offsetByDirection(pos, c)
            if newPoint == deadPos then
              (true, pos)
            else
              (false, newPoint)
      ._1

  def dirToChar(dir: Direction2D): Char = {
    dir match
      case Direction2D.Up => '^'
      case Direction2D.Down => 'v'
      case Direction2D.Left => '<'
      case Direction2D.Right => '>'
  }

  object ArrowPad extends Pad:
    val layout: Vector[String] = Vector(" ^A", "<v>")

    override def apply(key: Char): Vec2i =
      key match
        case ' ' => Vec2i(0, 0)
        case '^' => Vec2i(1, 0)
        case 'A' => Vec2i(2, 0)
        case '<' => Vec2i(0, 1)
        case 'v' => Vec2i(1, 1)
        case '>' => Vec2i(2, 1)
        case _ => assert(false)

    def explodes(start: Char, path: String): Boolean =
      val startPos = arrowKeyToPoint(start)
      val deadPos = arrowKeyToPoint(' ')
      path.foldLeft((false, startPos)):
        case ((explodes, pos), c) =>
        if explodes then
          (true, pos)
        else
          val newPoint = offsetByDirection(pos, c)
          if newPoint == deadPos then
            (true, pos)
          else
            (false, newPoint)

      ._1

  def numpadKeyToPoint(key: Char): Vec2i =
    key match
      case '7' => Vec2i(0, 0)
      case '8' => Vec2i(1, 0)
      case '9' => Vec2i(2, 0)
      case '4' => Vec2i(0, 1)
      case '5' => Vec2i(1, 1)
      case '6' => Vec2i(2, 1)
      case '1' => Vec2i(0, 2)
      case '2' => Vec2i(1, 2)
      case '3' => Vec2i(2, 2)
      case ' ' => Vec2i(0, 3)
      case '0' => Vec2i(1, 3)
      case 'A' => Vec2i(2, 3)
      case _ => assert(false)

  def offsetByDirChar(start: Char, key: Char, isArrowPad: Boolean): Char =
    val sp = if isArrowPad then arrowKeyToPoint(start) else numpadKeyToPoint(start)
    val res = offsetByDirection(sp, key)
    val pad = if isArrowPad then ArrowPad.layout else Numpad.layout
    pad(res.y)(res.x)
  def offsetByDirection(pos: Vec2i, key: Char): Vec2i =
    val mag = key match
      case '^' => Vec2i(0, -1)
      case 'v' => Vec2i(0, 1)
      case '>' => Vec2i(1, 0)
      case '<' => Vec2i(-1, 0)
      case 'A' => Vec2i(0, 0)
      case _ => assert(false)
    pos + mag
  def arrowKeyToPoint(key: Char): Vec2i =
    key match
      case ' ' => Vec2i(0, 0)
      case '^' => Vec2i(1, 0)
      case 'A' => Vec2i(2, 0)
      case '<' => Vec2i(0, 1)
      case 'v' => Vec2i(1, 1)
      case '>' => Vec2i(2, 1)
      case _ => assert(false)


  def stepArrowPad(str: String): String =
    str.prepended('A').sliding(2).map: s =>
      arrowMovementRaw(s(0), s(1))
    .foldLeft(List[String]("")): (acc, chunks) =>
      acc.flatMap: s =>
        chunks.map: c =>
          s ++ c
    .minBy(_.length)

  def arrowMovementRaw(start: Char, end: Char): List[String] =
    val startPoint = arrowKeyToPoint(start)
    val endPoint = arrowKeyToPoint(end)
    val mag = endPoint - startPoint
    val goingDown = mag.y > 0
    val goingRight = mag.x > 0
    val vertStr = if goingDown then "v".repeat(mag.y) else "^".repeat(math.abs(mag.y))
    val horzStr = if goingRight then ">".repeat(mag.x) else "<".repeat(math.abs(mag.x))


    List(horzStr + vertStr + "A", vertStr + horzStr + "A").distinct.filter(!ArrowPad.explodes(start, _))

  def arrowMovement(start: Char, end: Char): String =
    arrowMovementRaw(start, end) match
      case x @ List(_, _) => x.minBy(it => stepArrowPad(it).length)
      case y => y.head

  val cachedArrowMovement: (Char, Char) => String = Memo.memoize(arrowMovement)

  def numpadMovement(start: Char, end: Char): List[String] =
    val startPoint = numpadKeyToPoint(start)
    val endPoint = numpadKeyToPoint(end)
    val mag = endPoint - startPoint
    val goingDown = mag.y > 0
    val goingRight = mag.x > 0
    // we will never end up interleaving
    val vertStr = if goingDown then "v".repeat(mag.y) else "^".repeat(math.abs(mag.y))
    val horzStr = if goingRight then ">".repeat(mag.x) else "<".repeat(math.abs(mag.x))

    List(horzStr + vertStr + "A", vertStr + horzStr + "A").distinct.filter(!Numpad.explodes(start, _))

  val cachedNumpadMovement: (Char, Char) => List[String] = Memo.memoize(numpadMovement)



  def codeNum(code: String): Long =
    code.dropRight(1).toLong


  def processCode(code: String, n: Int = 2): Long =
    val solveCache = mut.HashMap[(Vec2i, Vec2i, Int), Long]()
    def shortestMove(start: Vec2i, end: Vec2i, level: Int): Long = solveCache.memo((start, end, level)):
      given Monoid[Long] = new Monoid[Long] {
        def empty: Long = Long.MaxValue
        def combine(x: Long, y: Long): Long = x min y
      }
      val pad = if level == 0 then Numpad else ArrowPad
      bfsFoldl((start, Vector.empty[Char])): (loc, keys) =>
        if loc == end then
          Right:
            if level < n then
              shortedSolution(keys :+ 'A', level + 1)
            else
              keys.length + 1L
        else
          Left:
            loc.stepsTowards(end).filterNot(c => loc.offset(c) == pad(' ')).map(d => (loc.offset(d), keys :+ dirToChar(d)))

    def shortedSolution(seq: Vector[Char], level: Int): Long =
      val pad = if level == 0 then Numpad else ArrowPad
      ('A' +: seq).map(pad.apply).sliding(2).map:
        case Vector(src, dst) => shortestMove(src, dst, level)
      .sum


    shortedSolution(code.toVector, 0)
    // solveArrowGood(firstRobot, n)



  override def part1(input: List[String]): Long =
    input.map: code =>
      val num = codeNum(code)
      val res = processCode(code)
      num * res
    .sum


  override def part2(input: List[String]): Long =
    input.map: code =>
      val num = codeNum(code)
      val res = processCode(code, 25)

      num * res
    .sum

  override lazy val input: String = FileIO.getInput(2024, 21)