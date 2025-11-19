package gay.menkissing.advent
package y2020

import gay.menkissing.common.*
import algebras.given
import Sys3D.*

import cats.implicits.*
import algebra.ring.Ring
import cats.*

object Day17 extends Problem:
  type Input = Set[Vec3[Int]]
  type Output = Int

  final case class Vec4[A](x: A, y: A, z: A, w: A) derives VecN

  def input = FileIO.getInput(2020, 17)
  def parse(str: String): Input =
    Grid.fromString(str)(_ == '#').zipWithIndices.flatMap:
      case (true, Vec2(x, y)) => Some(Vec3(x, 0, y))
      case _                  => None
    .toSet

  // thought process:
  // wouldn't it be hilarous if I had to do zero changes between part 1 and 2?
  def step[V[_]](state: Set[V[Int]])(using vn: VecN[V]): Set[V[Int]] =
    conwayStep[V[Int]](vn.allNeighbors, it => it == 2 || it == 3, _ == 3)(
      state
    )

  def show3dSlice(state: Set[Vec3[Int]]): String =
    if state.isEmpty then return "empty state"
    val minY = state.minBy(_.y).y
    val maxY = state.maxBy(_.y).y
    (minY to maxY).map: y =>
      val slice = state.filter(_.y == y).map(it => Vec2(it.x, it.z))
      if slice.isEmpty then s"slice y=$y (empty)"
      else
        val minX = slice.minBy(_.x).x
        val minZ = slice.minBy(_.y).y
        val rerooted = slice.map(_ - Vec2(minX, minZ))
        val grid =
          Grid.fromSparse(
            rerooted.maxBy(_.x).x + 1,
            rerooted.maxBy(_.y).y + 1,
            rerooted.map(_ -> '#').toMap
          )('.')
        show"slice y=$y;\n$grid"
    .mkString("\n\n")

  def part1(input: Set[Vec3[Int]]): Int = step[Vec3].repeated(6)(input).size

  def part2(input: Set[Vec3[Int]]): Int =
    val newInput = input.map(it => Vec4(it.x, it.y, it.z, 0))
    step[Vec4].repeated(6)(newInput).size
