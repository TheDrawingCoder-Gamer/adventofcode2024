package gay.menkissing.advent
package y2024

object Day25 extends HalfDay:
  type Input = List[KeyOrLock]
  type Output = Long

  final case class KeyOrLock(isKey: Boolean, pinHeights: Vector[Int]):
    def compatibleWith(that: KeyOrLock): Boolean =
      assert(this.isKey ^ that.isKey)
      this.pinHeights.zip(that.pinHeights).forall((l, r) => l + r <= 5)

  override def parse(str: String): List[KeyOrLock] =
    str.split("\n\n").map: str =>
      val goodGrid: Vector[Vector[Char]] =
        str.linesIterator.map(_.toVector).toVector
      val isKey = goodGrid(0)(0) != '#'
      val pinHeights = goodGrid.transpose.map(_.count(_ == '#') - 1)
      KeyOrLock(isKey, pinHeights)
    .toList

  override def part1(input: List[KeyOrLock]): Long =
    val (keys, locks) = input.partition(_.isKey)

    keys.flatMap: key =>
      locks.filter: lock =>
        key.compatibleWith(lock)
    .size

  override def input: String = FileIO.getInput(2024, 25)
