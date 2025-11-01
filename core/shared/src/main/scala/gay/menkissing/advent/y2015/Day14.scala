package gay.menkissing.advent
package y2015

import gay.menkissing.common.*
import scala.collection.mutable
import cats.Show

object Day14 extends Problem:
  type Input = List[Reindeer]
  type Output = Int
  def showOutput: Show[Int] = summon

  case class Reindeer(speed: Int, active: Int, rest: Int)
  case class ReindeerState
    (
      pos: Int,
      activeTime: Int,
      point: Int,
      reindeer: Reindeer
    ):
    def advance: ReindeerState =
      if activeTime > 0 then
        val newPos = pos + reindeer.speed
        val newTime = activeTime - 1
        ReindeerState(
          newPos,
          if newTime == 0 then -reindeer.rest else newTime,
          point,
          reindeer
        )
      else
        assert(activeTime != 0)
        val newTime = activeTime + 1
        ReindeerState(
          pos,
          if newTime == 0 then reindeer.active else newTime,
          point,
          reindeer
        )

  object ReindeerState:
    def apply(reindeer: Reindeer): ReindeerState =
      ReindeerState(0, reindeer.active, 0, reindeer)

    def advanced(self: ReindeerState): ReindeerState = self.advance

  def parse(str: String): List[Day14.Reindeer] =
    str.linesIterator.map:
      case s"$_ can fly $speed km/s for $active seconds, but then must rest for $rest seconds." =>
        Reindeer(speed.toInt, active.toInt, rest.toInt)
    .toList

  def updateMaxes(l: List[ReindeerState]): List[ReindeerState] =
    val max = l.maxBy(_.pos).pos
    l.map(it => if it.pos == max then it.copy(point = it.point + 1) else it)

  def part1(input: List[Reindeer]): Int =
    val states = input.map(ReindeerState.apply)
    states.map(ReindeerState.advanced.repeated(2503)).maxBy(_.pos).pos

  def part2(input: List[Reindeer]): Int =
    val states = input.map(ReindeerState.apply)
    val fun = (it: List[ReindeerState]) => updateMaxes(it.map(_.advance))
    fun.repeated(2503)(states).maxBy(_.point).point

  lazy val input: String = FileIO.getInput(2015, 14)
