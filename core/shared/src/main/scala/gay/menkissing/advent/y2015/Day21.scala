package gay.menkissing.advent
package y2015

import gay.menkissing.common.*, ArityN.*
import cats.implicits.*

object Day21 extends Problem:
  type Input = Actor
  type Output = Int

  final case class Actor(hp: Int, damage: Int, armor: Int):
    def diesIn(dmg: Int): Int =
      val realDmg = math.max(1, dmg - armor)
      math.ceil(hp.toDouble / realDmg.toDouble).toInt

  def input = FileIO.getInput(2015, 21)

  def parse(str: String): Actor =
    val Array(
      s"Hit Points: $hp",
      s"Damage: $dmg",
      s"Armor: $armor"
    ) = str.linesIterator.toArray: @unchecked
    Actor(hp.toInt, dmg.toInt, armor.toInt)

  final case class StoreItem(cost: Int, damage: Int, armor: Int):
    def |+|(that: StoreItem): StoreItem =
      StoreItem(cost + that.cost, damage + that.damage, armor + that.armor)

  val weapons =
    List(
      StoreItem(8, 4, 0),
      StoreItem(10, 5, 0),
      StoreItem(25, 6, 0),
      StoreItem(40, 7, 0),
      StoreItem(74, 8, 0)
    )
  val armors =
    List(
      StoreItem(13, 0, 1),
      StoreItem(31, 0, 2),
      StoreItem(53, 0, 3),
      StoreItem(75, 0, 4),
      StoreItem(102, 0, 5)
    )

  val rings =
    List(
      StoreItem(25, 1, 0),
      StoreItem(50, 2, 0),
      StoreItem(100, 3, 0),
      StoreItem(20, 0, 1),
      StoreItem(40, 0, 2),
      StoreItem(80, 0, 3)
    )

  object Player:
    def apply(dmg: Int, armor: Int) = Actor(100, dmg, armor)

  // why does brute force keep working???
  // this is day 21 it should not be this trivial
  def configurations: List[(Int, Actor)] =
    for
      weapon <- weapons
      armor <- armors.prepended(StoreItem(0, 0, 0))
      rings <- (rings.combinationsN[2].map(_ |+| _) ++ rings).toList
        .prepended(StoreItem(0, 0, 0))
    yield
      val combined = weapon |+| armor |+| rings
      (combined.cost, Player(combined.damage, combined.armor))

  def part1(boss: Actor): Int =
    val values = configurations
    // >= - if we would die in the same amount of turns as the boss, we take our turn first so
    // we get to deal the last hit
    values.filter((_, a) => a.diesIn(boss.damage) >= boss.diesIn(a.damage))
      .minBy(_._1)._1

  def part2(boss: Actor): Int =
    val values = configurations
    values.filter((_, a) => a.diesIn(boss.damage) < boss.diesIn(a.damage))
      .maxBy(_._1)._1
