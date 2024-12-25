package gay.menkissing.advent

import gay.menkissing.common.Grid

object Day25 extends Problem[List[Day25.KeyOrLock], Long]:
  case class KeyOrLock(isKey: Boolean, pinHeights: Vector[Int]):
    def compatibleWith(that: KeyOrLock): Boolean =
      assert(this.isKey ^ that.isKey)
      this.pinHeights.zip(that.pinHeights).forall((l, r) => l + r <= 5)

  override def parse(str: String): List[KeyOrLock] =
    str.split("\n\n").map: str =>
      val goodGrid: Grid[Char] = Grid(str.linesIterator.map(_.toCharArray))
      val isKey = goodGrid(0, 0) != '#'
      val pinHeights = goodGrid.columns.map(_.count(_ == '#') - 1).toVector
      KeyOrLock(isKey, pinHeights)
    .toList

  override def part1(input: List[KeyOrLock]): Long =
    val (keys, locks) = input.partition(_.isKey)

    keys.flatMap: key =>
      locks.map: lock =>
        key.compatibleWith(lock)
    .count(identity)

  // No part 2 on christmas!
  override def part2(input: List[KeyOrLock]): Long = -1L

  override val input: String = FileIO.getContentsOf("day25.txt")


