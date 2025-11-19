package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import algebras.given
import cats.syntax.all.*

object Day17 extends Problem:
  type Input = AABB2D[Int]
  type Output = Int

  def input = FileIO.getInput(2021, 17)

  val debugger = Debuginator(active = false)

  def parse(str: String): Input =
    str.trim match
      case s"target area: x=$mnx..$mxx, y=$mny..$mxy" =>
        AABB2D(mnx.toInt dimBy mxx.toInt, mny.toInt dimBy mxy.toInt)

  def willBeInRange(input: AABB2D[Int], initVelocity: Vec2[Int]): Boolean =
    AABB2D(0 dimBy input.xs.max, input.ys.min dimBy Int.MaxValue)
    var cur = Vec2(0, 0)
    var curV = initVelocity
    // Even if cur.x == xs.max, curV.x could == 0
    while cur.x <= input.xs.max && cur.y > input.ys.min do
      cur += curV
      curV = curV.copy(x = math.max(0, curV.x - 1), y = curV.y - 1)
      if input.contains(cur) then return true
    false

  def xEventuallyEnters(vx: Int, range: Dimension[Int]): Boolean =
    var curX = 0
    var curV = vx
    while curX < range.max && curV != 0 do
      curX += curV
      curV = math.max(0, curV - 1)
      if range.contains(curX) then return true
    false
  def yEventuallyEnters(vy: Int, range: Dimension[Int]): Boolean =
    var curY = 0
    var curV = vy
    while curY > range.min do
      curY += curV
      curV -= 1
      if range.contains(curY) then return true
    false
  def calculateYVelocityFor(y: Int): Int = -y - 1
  def part1(input: AABB2D[Int]): Int =
    // We want our Y value to go up, then come back down.
    // We are shooting from 0,0, and our input's Y range _seems_ to always be below 0,
    // so we can calculate what our gravity will be for a given y value, then do some math there
    // Because of parabolas and gravity and the like, we can guarentee that we will always have a position that
    // will be exactly 0, but our velocity will be negative

    // Given a velocity, we can see if it will end up being in the target area
    def willYBeInRange(vy: Int): Boolean = yEventuallyEnters(vy, input.ys)

    val ys =
      -(input.ys.min to 0)
        .find(it => willYBeInRange(calculateYVelocityFor(-it))).get

    IntSequences.triangleNumber(ys).toInt

  def part2(input: AABB2D[Int]): Int =

    // Get all Xs that are valid
    val validXs = (1 to input.xs.max)
      .filter(x => xEventuallyEnters(x, input.xs))
    // Get all negative Ys that are valid
    val validNegYs = (input.ys.min to 0)
      .filter(y => yEventuallyEnters(y, input.ys))
    // Get all positive Ys that are valid
    val validPosYs = (input.ys.min until 0).map(y => -y)
      .filter(vy => yEventuallyEnters(calculateYVelocityFor(vy), input.ys))
    debugger.debug(validPosYs)
    // Join negative and positive cases
    val validYs = validPosYs ++ validNegYs
    val validVs =
      // Choosing the cartesion product of the lists on purpose
      (validXs.toList, validYs.toList).tupled.toSet
        .filter(it => willBeInRange(input, Vec2(it._1, it._2)))
    debugger.debug(validVs)

    validVs.size
