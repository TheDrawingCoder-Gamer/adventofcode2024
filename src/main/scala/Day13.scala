import gay.menkissing.advent.Problem
import gay.menkissing.common.*

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable as mut


extension (a: Long) {
  infix def safeDiv(b: Long): Option[Long] = Option.when(b != 0 && a % b == 0)(a / b)
}


case class CraneMachine(buttonA: Vec2l, buttonB: Vec2l, prize: Vec2l) {
  // Test if it's even worth trying to solve this
  // IDK Figure this out later
  def isPossiblySolvable: Boolean = true

  def minimumTokens: Option[Long] = {
    // 1000 test cases : (
    (0 to 100).flatMap { a =>
      (0 to 100).filter { b =>
        Vec2l(buttonA.x * a + buttonB.x * b, buttonA.y * a + buttonB.y * b) == prize
      }.map(b => a * 3 + b)
    }.minOption.map(_.toLong)
  }

  // system of equations
  // A * ax + B * bx = x
  // A * ay + B * by = y
  // A = (x - B * bx) / ax
  // B = (x * ay - y * ax) / (bx * ay - by * ax)
  def minimumTokensP2: Option[Long] = {
    for {
      b <- (prize.x * buttonA.y - prize.y * buttonA.x) safeDiv (buttonB.x * buttonA.y - buttonB.y * buttonA.x)
      a <- (prize.x - b * buttonB.x) safeDiv buttonA.x
    } yield (a * 3 + b)
  }

  def correctUnitError: CraneMachine = copy(prize = prize + Vec2l(10000000000000L, 10000000000000L))
}

object CraneMachine {
  def parse(str: String): CraneMachine = {
    val lines = str.trim.linesIterator.toVector
    val s"Button A: X+$xa, Y+$ya" = lines(0) : @unchecked
    val s"Button B: X+$xb, Y+$yb" = lines(1) : @unchecked
    val s"Prize: X=$xp, Y=$yp" = lines(2) : @unchecked
    CraneMachine(Vec2l(xa.toLong, ya.toLong), Vec2l(xb.toLong, yb.toLong), Vec2l(xp.toLong, yp.toLong))
  }
}

object Day13 extends Problem[List[CraneMachine], Long] {
  override def parse(str: String): List[CraneMachine] = str.split("\n\n").map(CraneMachine.parse).toList

  override def part1(input: List[CraneMachine]): Long = {
    input.flatMap(_.minimumTokensP2).sum
  }

  override def part2(input: List[CraneMachine]): Long = {
    input.flatMap(_.correctUnitError.minimumTokensP2).sum
  }

  lazy val input: String = Source.fromResource("day13.txt").mkString.trim
}
/*
@main def main(): Unit = {
  println(Day13.fullPart1)
  println(Day13.fullPart2)
}
 */