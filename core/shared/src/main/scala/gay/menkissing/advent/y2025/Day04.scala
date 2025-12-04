package gay.menkissing.advent
package y2025

import gay.menkissing.common.Vec2
import gay.menkissing.common.Grid
import gay.menkissing.common.algebras.given
import scala.annotation.tailrec

object Day04 extends Problem:
  type Input = Set[Vec2[Int]]
  type Output = Int

  def input: String = FileIO.getInput(2025, 4)

  def parse(str: String): Set[Vec2[Int]] =
    Grid.fromString(str)(_ == '@').zipWithIndices.filter(_._1).map(_._2).toSet

  def accessible(universe: Set[Vec2[Int]])(p: Vec2[Int]): Boolean =
    p.allNeighbors.count(universe) < 4

  def part1(input: Set[Vec2[Int]]): Int = input.count(accessible(input))

  def part2(input: Set[Vec2[Int]]): Int =
    @tailrec
    def step(in: Set[Vec2[Int]], acc: Int): Int =
      val (accessibleIn, inaccessibleIn) = in.partition(accessible(in))
      if accessibleIn.isEmpty then acc
      else step(inaccessibleIn, acc + accessibleIn.size)
    step(input, 0)
