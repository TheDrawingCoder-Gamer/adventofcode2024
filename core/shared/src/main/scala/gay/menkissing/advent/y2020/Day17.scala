package gay.menkissing.advent
package y2020

import gay.menkissing.common.*
import algebras.given
import Sys3D.*

import cats.implicits.*

object Day17 extends Problem:
  type Input = Set[Vec3[Int]]
  type Output = Int

  case class Vec4i(x: Int, y: Int, z: Int, w: Int):
    def allNeighbors: List[Vec4i] =
      for
        x <- List(this.x - 1, this.x, this.x + 1)
        y <- List(this.y - 1, this.y, this.y + 1)
        z <- List(this.z - 1, this.z, this.z + 1)
        w <- List(this.w - 1, this.w, this.w + 1)
        if x != this.x || y != this.y || z != this.z || w != this.w
      yield Vec4i(x, y, z, w)

  lazy val input = FileIO.getInput(2020, 17)
  def parse(str: String): Input =
    Grid.fromString(str)(_ == '#').zipWithIndices.flatMap:
      case (true, Vec2(x, y)) => Some(Vec3(x, 0, y))
      case _                  => None
    .toSet

  def step(state: Set[Vec3[Int]]): Set[Vec3[Int]] =
    val extendedSet = state.flatMap(it => it.allNeighbors.toSet + it)
    val withKilled =
      state.filter: p =>
        val r = p.allNeighbors.count(state.apply)
        r == 2 || r == 3
    val deadSet = extendedSet -- state
    val withBorn =
      deadSet.filter:
        _.allNeighbors.count(state.apply) == 3
    withKilled ++ withBorn

  def stepP2(state: Set[Vec4i]): Set[Vec4i] =
    val extendedSet = state.flatMap(it => it.allNeighbors.toSet + it)
    val withKilled =
      state.filter: p =>
        val r = p.allNeighbors.count(state.apply)
        r == 2 || r == 3
    val deadSet = extendedSet -- state
    val withBorn =
      deadSet.filter:
        _.allNeighbors.count(state.apply) == 3
    withKilled ++ withBorn

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

  def part1(input: Set[Vec3[Int]]): Int = step.repeated(6)(input).size

  def part2(input: Set[Vec3[Int]]): Int =
    val newInput = input.map(it => Vec4i(it.x, it.y, it.z, 0))
    stepP2.repeated(6)(newInput).size
