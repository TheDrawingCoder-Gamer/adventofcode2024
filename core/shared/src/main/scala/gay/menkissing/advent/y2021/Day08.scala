package gay.menkissing.advent
package y2021

import cats.implicits.*
import cats.syntax.all.*

object Day08 extends Problem:
  type Input = List[(List[Int], List[Int])]
  type Output = Int

  def input = FileIO.getInput(2021, 8)

  val aBit: Byte = 0b1000000
  val bBit: Byte = 0b0100000
  val cBit: Byte = 0b0010000
  val dBit: Byte = 0b0001000
  val eBit: Byte = 0b0000100
  val fBit: Byte = 0b0000010
  val gBit: Byte = 0b0000001

  def parseSegment(str: String): Int =
    assert(str.length >= 2)
    str.foldLeft(0):
      case (acc, c) =>
        c match
          case 'a' => acc | aBit
          case 'b' => acc | bBit
          case 'c' => acc | cBit
          case 'd' => acc | dBit
          case 'e' => acc | eBit
          case 'f' => acc | fBit
          case 'g' => acc | gBit

  def parse(input: String): List[(List[Int], List[Int])] =
    input.linesIterator.map:
      case s"$l | $r" =>
        (
          l.split(' ').map(parseSegment).toList,
          r.split(' ').map(parseSegment).toList
        )
    .toList

  def digitCouldBe(n: Int): List[Int] =
    bitCount(n) match
      case 2 => List(1)
      case 3 => List(7)
      case 4 => List(4)
      case 5 => List(2, 3, 5)
      case 6 => List(0, 6, 9)
      case 7 => List(8)
      case _ => assert(false)

  def bitCount(n: Int): Int =
    List(aBit, bBit, cBit, dBit, eBit, fBit, gBit).count(it => (it & n) != 0)

  def unary[A](ls: List[A]): Boolean =
    ls match
      case _ :: Nil => true
      case _        => false

  inline def memberOf(member: Int, group: Int): Boolean =
    (group & member) == member

  def calcDigit(mapping: Vector[Int], digit: Int): Int =
    mapping.indexWhere(_ == digit)

  def calcDigits(uniqueDigits: List[Int], displayed: List[Int]): Int =
    val n1 = uniqueDigits.find(bitCount.andThen(_ == 2)).get
    val n4 = uniqueDigits.find(bitCount.andThen(_ == 4)).get
    val n7 = uniqueDigits.find(bitCount.andThen(_ == 3)).get
    val n8 = uniqueDigits.find(bitCount.andThen(_ == 7)).get
    val n3 = uniqueDigits.find(x => memberOf(n1, x) && bitCount(x) == 5).get
    val n0 =
      uniqueDigits.find: x =>
        bitCount(x) == 6 && memberOf(n1, x) && memberOf(n7, x) &&
          !memberOf(n3, x)
      .get
    val l5 = uniqueDigits.filter(x => bitCount(x) == 5 && x != n3)
    val l6 = uniqueDigits.filter(x => bitCount(x) == 6 && x != n0)
    val s5n6 = (l5, l6).tupled
    val (n5, _) = s5n6.find(memberOf).get
    val n6 = l6.find(it => !memberOf(n1, it)).get
    val n2 = l5.find(x => x != n3 && x != n5).get
    val n9 = l6.find(x => x != n6 && x != n0).get

    val mapping = Vector(n0, n1, n2, n3, n4, n5, n6, n7, n8, n9)
    displayed.map(it => calcDigit(mapping, it)).foldLeft(0):
      case (acc, num) => (acc * 10) + num

  def part1(input: List[(List[Int], List[Int])]): Int =
    val realDigits = input.flatMap(_._2)
    realDigits.count(digitCouldBe.andThen(unary))

  def part2(input: List[(List[Int], List[Int])]): Int =
    input.map(calcDigits).sum
