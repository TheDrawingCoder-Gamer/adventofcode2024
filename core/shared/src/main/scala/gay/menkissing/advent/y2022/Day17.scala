package gay.menkissing.advent
package y2022

import gay.menkissing.common.*

import collection.mutable

object Day17 extends Problem:
  type Input = List[Boolean]
  type Output = Long

  lazy val input = FileIO.getInput(2022, 17)

  val debugger = Debuginator(level = Debuginator.Level.Simple)

  def parse(str: String): List[Boolean] = str.trim.map(_ == '>').toList

  type RockSegment = Vector[Boolean]
  type Rock = Vector[RockSegment]

  object Rock:
    def parseSegment(str: String): RockSegment = str.map(_ == '#').toVector

  val rocks =
    List(
      Vector(
        "####"
      ),
      Vector(
        ".#.",
        "###",
        ".#."
      ),
      Vector(
        "..#",
        "..#",
        "###"
      ),
      Vector(
        "#",
        "#",
        "#",
        "#"
      ),
      Vector(
        "##",
        "##"
      )
    ).map(_.map(Rock.parseSegment).reverse)

  class Chamber(val levels: mutable.Buffer[Array[Boolean]] = mutable.Buffer()):
    def height = levels.length
    def cloned: Chamber = Chamber(levels.map(_.clone()).clone())

    def testIntersect(x: Int, y: Int, rock: Rock): Boolean =
      if y < 0 then true
      else if levels.length <= y then false
      else
        levels.drop(y).zip(rock).exists: (level, rockPart) =>
          val z = rockPart.prependedAll(Vector.fill(x)(false))
          z.length > 7 || z.zip(level).exists(_ && _)

    def dropRocks
      (rockStream: Iterator[Rock], wind: Iterator[Boolean]): this.type =
      rockStream.foreach: rock =>
        var x = 2
        var y = levels.length + 3
        def whileMethod(): Unit =
          while true do
            val newX = x + (if wind.next() then 1 else -1)
            if newX >= 0 && (newX + rock.head.length) <= 7 &&
              !testIntersect(newX, y, rock)
            then x = newX
            if y == 0 || testIntersect(x, y - 1, rock) then return
            else y = y - 1
        whileMethod()
        rock.zipWithIndex.foreach: (part, yp) =>
          val newY = y + yp
          if newY >= levels.length then
            debugger.assertEq(expected = 0, actual = newY - levels.length)
            levels.append(Array.ofDim[Boolean](7))
          part.zipWithIndex.foreach: (b, xp) =>
            if b then
              debugger.assert(!levels(newY)(x + xp))
              levels(newY)(x + xp) = b
      this

  def rockIterator: Iterator[Rock] = ForeverIterator(this.rocks.iterator)
  def part1(input: List[Boolean]): Long =
    val wind = ForeverIterator(input.iterator)

    // Lets just try the naive approach before I get too caught up
    // in optimizing
    // - iirc thats why I gave up last time

    val chamber = Chamber()

    chamber.dropRocks(rockIterator.take(2022), wind)

    debugger.verbose(
      chamber.levels.map(_.map(if _ then '#' else '.').mkString).reverse
        .mkString("\n")
    )

    chamber.levels.length

  def part2(input: List[Boolean]): Long =
    val wind = CountingIterator(ForeverIterator(input.iterator))
    // The floor is FREAKY, so we cant do any modulo magic right above the floor
    val chamber = Chamber()

    // So to avoid the forbidden floor, we are just gonna drop a shitton of rocks
    val baseRockCount = 10 * this.rocks.size
    chamber.dropRocks(rockIterator.take(baseRockCount), wind)
    val baseWind = wind.count
    val baseHeight = chamber.height

    // Then for each multiple of 5, check to see if we found any repitition
    case class BaseChamber(rockCount: Int, windCount: Long, height: Int)

    val (repeatBase, extRockCount) =
      unfoldedMap(
        (
          baseRockCount,
          Vector(BaseChamber(baseRockCount, baseWind, baseHeight))
        )
      ): (rockCount, extendedBaseChambers) =>
        chamber.dropRocks(rocks.iterator, wind.iterator)
        val extRockCount = rockCount + rocks.length
        val extChamberHeight = chamber.height
        val extWind = wind.count

        val foundRepeat =
          extendedBaseChambers.find: extended =>
            ((extWind - extended.windCount) % input.length == 0) && locally:
              val extDeltaHalfHeight = (extChamberHeight - extended.height) / 2
              val bottomPart =
                (extended.height - 20 until
                  extended.height + extDeltaHalfHeight - 20).map(chamber.levels)
              val topPart =
                (extended.height + extDeltaHalfHeight - 20 until
                  extChamberHeight - 20).map(chamber.levels)
              bottomPart.zip(topPart).forall((l, r) => l.sameElements(r))

        foundRepeat.map(it => (it, extRockCount)).toLeft:
          val newExtendedBaseChambers =
            extendedBaseChambers :+
              BaseChamber(extRockCount, extWind, extChamberHeight)
          (extRockCount, newExtendedBaseChambers)

    val beforeRepeatRockCount = repeatBase.rockCount
    val repeatSizeRocks = extRockCount - beforeRepeatRockCount

    wind.count - repeatBase.windCount

    val beforeRepeatChamberHeight = repeatBase.height
    val afterRepeatChamberHeight = chamber.height
    val repeatSizeHeight = afterRepeatChamberHeight - beforeRepeatChamberHeight

    val rockCount = 1_000_000_000_000L
    val repeatCount = (rockCount - beforeRepeatRockCount) / repeatSizeRocks
    val projectedChamberH = repeatCount * repeatSizeHeight

    val stillToDrop =
      rockCount - beforeRepeatRockCount - (repeatCount * repeatSizeRocks)
    assert(stillToDrop < Int.MaxValue)
    chamber.dropRocks(rockIterator.take(stillToDrop.toInt), wind)
    val afterChamberHDelta = chamber.height - afterRepeatChamberHeight

    assert(
      beforeRepeatRockCount + repeatCount * repeatSizeRocks + stillToDrop ==
        rockCount
    )

    beforeRepeatChamberHeight + projectedChamberH + afterChamberHDelta
