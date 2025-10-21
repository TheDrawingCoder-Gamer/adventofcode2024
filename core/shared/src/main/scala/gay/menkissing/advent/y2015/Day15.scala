package gay.menkissing.advent
package y2015

import gay.menkissing.common.*
import cats.*, cats.syntax.all.*
import alleycats.std.set.*

object Day15 extends HalfDay[List[Day15.Ingredient], Int]:
  case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int)

  def parse(str: String): List[Ingredient] =
    str.trim.linesIterator.map:
      case s"$_: capacity $capacity, durability $durability, flavor $flavor, texture $texture, calories $calories" =>
        Ingredient(capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
    .toList

  // 2 ingredients has 100 variations
  // 4 ingredients (our input) would have 100 variations for the initial ingredient (selecting x as the number),
  // then second ingredient would have 100 - x variations (selecting y, between x and 100), and third ingredient would
  // have 100 - y
  def part1(input: List[Ingredient]): Int = ???

  def part2(input: String): Int =
    input.zipWithIndex.foldLeft((0, -1)):
      case ((acc, -1), ('(', _)) =>
        (acc + 1, -1)
      case ((0, -1), (')', i)) =>
        (-1, i)
      case ((acc, -1), (')', _)) =>
        (acc - 1, -1)
      case ((a, b), (_, _)) => (-1, b)
    ._2 + 1

  lazy val input: String = FileIO.getInput(2015, 15)
