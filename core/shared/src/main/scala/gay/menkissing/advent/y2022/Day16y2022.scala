package gay.menkissing.advent
package y2022

import gay.menkissing.advent.FileIO
import gay.menkissing.common.{astar, memo}

import scala.collection.mutable as mut
import scala.io.Source
import cats.*


object Day16y2022 extends Problem[Day16y2022.ValveMap, Int]:
  val fullTimeP2 = 26
  case class ValveRoom(room: String, flowRate: Int, connectsTo: Vector[String]) {
    def value(time: Int, fullTime: Int) = flowRate * (fullTime - time )
  }

  type ValveMap = Map[String, ValveRoom]

  val start = "AA"

  lazy val input = FileIO.getInput(2022, 16)

  def parse(input: String): ValveMap =
    input.trim.linesIterator.map { it =>
      it.trim match {
        case s"Valve $room has flow rate=$n; tunnel$_ lead$_ to valve$_ $rest" =>
          val goodRest = rest.split(",").map(_.trim).toVector
          ValveRoom(room, n.toInt, goodRest)

        case _ => assert(false)
      }
    }.map(it => (it.room, it)).toMap

  private val distanceMemo = mut.HashMap[String, Int]()

  extension (self: Map[String, ValveRoom])
    def startRoom: ValveRoom = self(start)

    def distance(start: String, end: String): Int =
      val s = Order.min(start, end)
      val e = Order.max(start, end)
      distanceMemo.memo(s"$s.$e"):
        graphAStar(s, e, self).get.size - 1

  case class State(on: List[String], time: Int, pressure: Int)
  case class Position(dest: ValveRoom, progress: Int)

  def importantRooms(rooms: ValveMap, on: List[String]) = rooms.filter((k, v) => v.flowRate != 0 && !on.contains(k)).values.toVector


  def graphAStar(start: String, goal: String, graph: ValveMap): Option[List[String]] = {
    astar(start, goal, _ => 0d, (_, _) => 1d, it => graph(it).connectsTo)
  }

  def runP1(data: Map[String, ValveRoom], rooms: Set[ValveRoom], curPos: ValveRoom, fullTime: Int, time: Int, pressure: Int): Int = {
    val res = rooms.flatMap { room =>
      // includes current, and nodes to get there. no minus 1 because of valve turning
      val timeTaken = data.distance(curPos.room, room.room) + 1
      val goodTime = time + timeTaken
      Option.when(goodTime <= fullTime) {
        runP1(data, rooms - room, room, fullTime, goodTime, pressure + room.value(goodTime, fullTime))
      }
    }.maxOption
    res.getOrElse(pressure)
  }


  def part1(input: ValveMap): Int =
    val important = importantRooms(input, List()).toSet
    runP1(input, important, input.startRoom, 30, 0, 0)

  def part2(input: ValveMap): Int =
    // An optimal solution likely has an even split
    val important = importantRooms(input, List())
    val importantSet = important.toSet
    val valvesA = important
      .combinations(importantSet.size / 2)
      .map(_.toSet)

    val allPaths =
      for va <- valvesA yield
        val vb = importantSet -- va
        val scoreA = runP1(input, va, input.startRoom, 26, 0, 0)
        val scoreB = runP1(input, vb, input.startRoom, 26, 0, 0)
        scoreA + scoreB
    allPaths.max



