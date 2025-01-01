import cats.*
import cats.implicits.*
import cats.data.*
import scala.io.Source 
import scala.collection.mutable as mut 
import gay.menkissing.common.astar

case class ValveRoom(room: String, flowRate: Int, connectsTo: Vector[String]) {
  def value(time: Int, fullTime: Int) = flowRate * (fullTime - time )
}

type ValveMap = Map[String, ValveRoom]

val start = "AA"

val input = Source.fromResource("day16.txt").mkString 

val data = {
  input.trim.linesIterator.map { it => 
    it.trim match {
      case s"Valve $room has flow rate=$n; tunnel$_ lead$_ to valve$_ $rest" => 
        val goodRest = rest.split(",").map(_.trim).toVector 
        ValveRoom(room, n.toInt, goodRest)

      case _ => assert(false)
    }
  }.map(it => (it.room, it)).toMap
}

object DistanceCalculator {
  private val memo = mut.HashMap[String, Int]()
  def distance(start: String, end: String): Int = {
    
    val goodStart = Order.min(start, end)
    val goodEnd = Order.max(start, end)
    memo.getOrElseUpdate(s"$goodStart.$goodEnd", graphAstar(start, end, data).get.size - 1)
  }
}
val startRoom = data(start)
case class State(on: List[String], time: Int, pressure: Int)

def importantRooms(rooms: ValveMap, on: List[String]) = rooms.filter((k, v) => v.flowRate != 0 && !on.contains(k)).values.toVector
def pathsToImportant(rooms: ValveMap, curPos: ValveRoom, on: List[String]) = {
  importantRooms(rooms, on).map { it =>
    (it, graphAstar(curPos.room, it.room, rooms))
  }
}
import scala.collection.mutable as mut
def reconstructPath(cameFrom: Map[String, String], p: String): List[String] = {

  val totalPath = mut.ListBuffer[String](p)
  var current = p
  while (cameFrom.contains(current)) {
    current = cameFrom(current)
    totalPath.prepend(current)
  }
  totalPath.toList
}

def graphAstar(start: String, goal: String, graph: ValveMap) =
  astar(start, goal, _ => 0d, (_, _) => 1d, it => graph(it).connectsTo)



def run(rooms: Set[ValveRoom], curPos: ValveRoom, fullTime: Int, time: Int, pressure: Int): Int = {
  val res = rooms.flatMap { room =>
    // includes current, and nodes to get there. no minus 1 because of valve turning
    val timeTaken = DistanceCalculator.distance(curPos.room, room.room) + 1
    val goodTime = time + timeTaken
    Option.when(goodTime <= fullTime) {
      run(rooms - room, room, fullTime, goodTime, pressure + room.value(goodTime, fullTime))
    }
  }.maxOption 
  res.getOrElse(pressure)
}

val important = importantRooms(data, List()).toSet 
run(important, startRoom, 30, 0, 0)


important.subsets()
.map {  myOpenValves => 
  val elephantValves = important -- myOpenValves
  // lie and say all elephant valves are turned on for your walkthrough
  run(myOpenValves, startRoom, 26, 0, 0)
  + run(elephantValves, startRoom, 26, 0, 0)
}.max
