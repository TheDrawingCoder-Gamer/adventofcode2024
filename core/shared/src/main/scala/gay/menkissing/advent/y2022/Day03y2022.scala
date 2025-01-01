package gay.menkissing.advent
package y2022

object Day03y2022 extends Problem[List[Day03y2022.Rucksack], Int] {
  case class ItemType(underlying: Char) {
    def priority: Int = {
      if (underlying.isUpper) {
        underlying - 'A' + 27
      } else {
        underlying - 'a' + 1
      }
    }
  }

  case class Rucksack(leftCompartment: List[ItemType], rightCompartment: List[ItemType]) {
    def isValid: Boolean = {
      invalidItems.isEmpty
    }

    def invalidItems: List[ItemType] = {
      leftCompartment.intersect(rightCompartment)
    }

    def sharedItem: ItemType = {
      invalidItems.head
    }

    def items: Set[ItemType] = (leftCompartment ++ rightCompartment).toSet
  }

  object Rucksack {
    def parse(input: String): Rucksack = {
      val (l, r) = input.splitAt(input.length / 2)
      Rucksack(l.toList.map(ItemType.apply), r.toList.map(ItemType.apply))
    }
  }

  case class Group(elf1: Rucksack, elf2: Rucksack, elf3: Rucksack) {
    def badge: ItemType = {
      elf1.items.intersect(elf2.items.intersect(elf3.items)).head
    }
  }


  def parse(input: String) : List[Rucksack] = {
    input.linesIterator.map(Rucksack.parse).toList
  }
  def part1(input: List[Rucksack]): Int = {
    input.map(_.sharedItem.priority).sum
  }
  def part2(input: List[Rucksack]): Int = {
    val groups = input.grouped(3).map(it => Group(it.head, it(1), it(2)))
    groups.map(_.badge.priority).sum
  }

  lazy val input = FileIO.getInput(2022, 3)
}

