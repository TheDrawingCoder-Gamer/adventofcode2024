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

  def parse(str: String): Input =
    Grid.fromString(str)(_ == '@').zipWithIndices.filter(_._1).map(_._2).toSet

  def part1(input: Set[Vec2[Int]]): OutputP1 =
    input.count: p =>
      p.allNeighbors.count(input) < 4

  def part2(input: Set[Vec2[Int]]): OutputP2 =
    @tailrec
    def step(in: Set[Vec2[Int]], acc: Int): Int =
      val (accessibleIn, inaccessibleIn) =
        in.partition(_.allNeighbors.count(in) < 4)
      if accessibleIn.isEmpty then acc
      else step(inaccessibleIn, acc + accessibleIn.size)
    step(input, 0)
