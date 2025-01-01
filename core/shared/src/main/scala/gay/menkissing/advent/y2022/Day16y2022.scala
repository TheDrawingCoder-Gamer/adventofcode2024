package gay.menkissing.advent
package y2022

import gay.menkissing.advent.FileIO
import gay.menkissing.common.{astar, memo}

import scala.collection.mutable as mut
import scala.io.Source
import cats.*


object Day16y2022 extends HalfDay[Day16y2022.ValveMap, Int]:
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
  case class ImportantRooms(rooms: ValveMap) {
    val daImportantRooms = importantRooms(rooms, List())
    def edge(p1: ValveRoom, p2: ValveRoom, fullTime: Int): Eval[Int] = Eval.later {
      astar(p1, p2, _ => 0d, (_, _) => 1d, it => it.connectsTo.map(rooms.apply))
        .map(_.size)
        .getOrElse(fullTime)
    }.memoize
  }
  def importantRooms(rooms: ValveMap, on: List[String]) = rooms.filter((k, v) => v.flowRate != 0 && !on.contains(k)).values.toVector
  def pathsToImportant(rooms: ValveMap, curPos: ValveRoom, on: List[String]) = {
    importantRooms(rooms, on).map { it =>
      (it, graphAStar(curPos.room, it.room, rooms))
    }
  }
  def shortestTime(rooms: ValveMap, curPos: ValveRoom, on: List[String], fullTime: Int): Int = {
    val res = pathsToImportant(rooms, curPos, on).flatMap(_._2)
    if (res.isEmpty)
      fullTime
    else
      res.minBy(_.size).size - 1
  }
  import scala.collection.mutable as mut


  def graphAStar(start: String, goal: String, graph: ValveMap): Option[List[String]] = {
    astar(start, goal, _ => 0d, (_, _) => 1d, it => graph(it).connectsTo)
  }

  case class HappenedNow(pressureAdded: Int, turnedOn: Option[String])
  def runFirstBit(rooms: ValveMap, pos: Position, state: State): (List[Position], HappenedNow) = {
    if (pos.progress == 0) {
      val newPressure = pos.dest.value(state.time, fullTimeP2)
      val res = pathsToImportant(rooms, pos.dest, state.on).flatMap { (room, path) =>
        require(path.isDefined)
        // includes current, and nodes to get there. no minus 1 because of valve turning
        val timeTaken = path.get.size
        val goodTime = state.time + timeTaken
        if (goodTime > fullTimeP2)
          None
        else
          Some(Position(room, goodTime - state.time))
      }
      (res.toList, HappenedNow(pos.dest.value(state.time, fullTimeP2), Some(pos.dest.room)))
    } else {
      (List(pos.copy(progress = pos.progress - 1)), HappenedNow(0, None))
    }
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
  def run(rooms: ValveMap, pos1: Position, pos2: Position, state: State): Int = {
    val (p1s, HappenedNow(pressure1, turnedOn1)) = runFirstBit(rooms, pos1, state)
    val (p2s, HappenedNow(pressure2, turnedOn2)) = runFirstBit(rooms, pos2, state)
    val goodOn = state.on ++ List(turnedOn1, turnedOn2).flatten
    val goodPressure = state.pressure + pressure1 + pressure2
    val newState = state.copy(on = goodOn,pressure = goodPressure, time = state.time + 1)
    if (newState.time == fullTimeP2)
      return newState.pressure
    val res = for {
      p1 <- p1s
      p2 <- p2s
    } yield {
      run(rooms, p1, p2, newState)
    }
    if (res.isEmpty)
      newState.pressure
    else
      res.max
  }

  def part1(input: ValveMap): Int =
    val important = importantRooms(input, List()).toSet
    runP1(input, important, input.startRoom, 30, 0, 0)

  def part2(input: ValveMap): Int =
    run2(input, input.startRoom).maxBy(_._2)._2

  def resultOfPath(rooms: ValveMap, path: Vector[ValveRoom]) = {
    var pressure = 0
    var time = 0
    val res = path.sliding(2).map { case Vector(x, y) =>
      time = time +  graphAStar(x.room, y.room, rooms).get.size
      pressure = pressure + y.value(time - 1, fullTimeP2)
    }.toVector
    if (time > fullTimeP2)
      None
    else Some((path, pressure))
  }
  def run2(rooms: ValveMap, curPos: ValveRoom) = {
    val importantRooms = rooms.filter((k, v) => v.flowRate != 0).values.toVector
    importantRooms.permutations.flatMap(it => resultOfPath(rooms, it))
  }

  // resultOfPath(data, Vector(startRoom, data("BB"), data("CC"), data("DD"), data("EE"), data("HH"), data("JJ"))).get._2
  // run2(data, startRoom).maxBy(_._2)
