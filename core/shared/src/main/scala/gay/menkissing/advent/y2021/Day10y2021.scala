package gay.menkissing.advent
package y2021

import scala.annotation.tailrec

object Day10y2021 extends Problem[List[String], Long]:
  lazy val input = FileIO.getInput(2021, 10)

  @tailrec
  def matchLine(line: String, stack: List[Char] = List()): Either[Char, String] =
    if line.isEmpty then
      Right(stack.mkString)
    else
      line.head match
        case '[' => matchLine(line.tail, '[' :: stack)
        case '(' => matchLine(line.tail, '(' :: stack)
        case '<' => matchLine(line.tail, '<' :: stack)
        case '{' => matchLine(line.tail, '{' :: stack)
        case ']' =>
          if stack.head == '[' then
            matchLine(line.tail, stack.tail)
          else
            Left(']')

        case ')' =>
          if stack.head == '(' then
            matchLine(line.tail, stack.tail)
          else
            Left(')')
        case '>' =>
          if stack.head == '<' then
            matchLine(line.tail, stack.tail)
          else
            Left('>')

        case '}' =>
          if stack.head == '{' then
            matchLine(line.tail, stack.tail)
          else
            Left('}')
        case _ => assert(false)

  def parse(input: String): List[String] = input.linesIterator.toList

  def score(c: Char): Int =
    c match
      case '>' => 25137
      case ')' => 3
      case ']' => 57
      case '}' => 1197
      case  _  => assert(false)

  def scoreComplete(str: String): Long =
    def scoreC(c: Char): Long =
      c match
        case '(' => 1L
        case '[' => 2L
        case '{' => 3L
        case '<' => 4L
        case  _  => assert(false)
    str.foldLeft(0L):
      case (acc, p) => (acc * 5L) + scoreC(p)

  def median(ls: List[Long]): Long =
    val len = ls.length
    val l = len / 2
    ls.sorted.apply(l)

  def part1(input: List[String]): Long =
    val linesl = input.map(matchLine(_)).flatMap(_.left.toOption)
    linesl.foldLeft(0):
      case (acc, p) => acc + score(p)

  def part2(input: List[String]): Long =
    val linesr = input.map(matchLine(_)).flatMap(_.toOption)
    median(linesr.map(scoreComplete))
