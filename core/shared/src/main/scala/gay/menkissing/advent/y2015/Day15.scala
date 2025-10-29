package gay.menkissing.advent
package y2015

import gay.menkissing.common.*
import cats.*, cats.syntax.all.*
import alleycats.std.set.*

object Day15 extends Problem[Vector[Day15.Ingredient], Int]:
  case class Ingredient(capacity: Int, durability: Int, flavor: Int, texture: Int, calories: Int):
    def *(n: Int): Ingredient =
      map(_ * n)
      
    def map(f: Int => Int): Ingredient =
      Ingredient(f(capacity), f(durability), f(flavor), f(texture), f(calories))
    
    def zip(that: Ingredient)(f: (Int, Int) => Int): Ingredient =
      Ingredient(
        f(this.capacity, that.capacity),
        f(this.durability, that.durability),
        f(this.flavor, that.flavor),
        f(this.texture, that.texture),
        f(this.calories, that.calories))

    def score: Int =
      math.max(0, capacity * durability * flavor * texture)
  
  def sumGroups(xs: Ingredient*): Ingredient =
    xs.reduce((l, r) => l.zip(r)(_ + _)).map(it => math.max(it, 0))

  def parse(str: String): Vector[Ingredient] =
    str.trim.linesIterator.map:
      case s"$_: capacity $capacity, durability $durability, flavor $flavor, texture $texture, calories $calories" =>
        Ingredient(capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
    .toVector

  // 2 ingredients has 100 variations
  // 4 ingredients (our input) would have 100 variations for the initial ingredient (selecting x as the number),
  // then second ingredient would have 100 - x variations (selecting y, between x and 100), and third ingredient would
  // have 100 - y
  // optimization problem:
  // maximize Product of Sum(n_x * capacity(x)), Sum(n_x * durability(x)), Sum(n_x * flavor(x)), Sum(n_x * texture(x))
  // subject to:
  // Sum(n_x) = 100

  def freakyConfigurations(input: Vector[Ingredient]): Seq[Ingredient] =
    for
      i <- 0 until 100
      j <- 0 until (100 - i)
      k <- 0 until (100 - i - j)
      l = 100 - i - j - k
    yield sumGroups(input(0) * i, input(1) * j, input(2) * k, input(3) * l)

  def part1(input: Vector[Ingredient]): Int =
    freakyConfigurations(input).map(_.score).max
  def part2(input: Vector[Ingredient]): Int =
    freakyConfigurations(input).withFilter(_.calories == 500).map(_.score).max

  lazy val input: String = FileIO.getInput(2015, 15)
