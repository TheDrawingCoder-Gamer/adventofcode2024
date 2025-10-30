package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import cats.implicits.*
import Vec2i.*

import scala.collection.mutable


// this weird type param list is so we don't have a `parse` method,
// just `parseP1` and `parseP2`
object Day23 extends ProblemUniqueInputs[Day23.State, Day23.State, Int]:
  enum Amphipod(val cost: Int, val room: Int):
    case A extends Amphipod(1, 3)
    case B extends Amphipod(10, 5)
    case C extends Amphipod(100, 7)
    case D extends Amphipod(1000, 9)
  object Amphipod:
    def tryFromChar(c: Char): Option[Amphipod] =
      c match
        case 'A' => Some(A)
        case 'B' => Some(B)
        case 'C' => Some(C)
        case 'D' => Some(D)
        case _ => None

  val hallwayPoses = List(
    Vec2i(1, 1),
    Vec2i(2, 1),
    Vec2i(4, 1),
    Vec2i(6, 1),
    Vec2i(8, 1),
    Vec2i(10, 1),
    Vec2i(11, 1)
  )

  case class State(amphipods: Map[Vec2i, Amphipod], roomSize: Int):
    def isGoal: Boolean =
      amphipods.forall((pos, amphipod) => pos.x == amphipod.room)

    def neighbors: Seq[(State, Int)] =
      for
        (start, amphipod) <- amphipods.toSeq
        stop <- validMovesFor(start, amphipod)
        path = getPath(start, stop)
        if path.forall(it => !amphipods.contains(it))
      yield
        val newAmphipods = amphipods - start + (stop -> amphipod)
        val newEnergy = path.size * amphipod.cost
        (copy(amphipods = newAmphipods), newEnergy)

    private def roomFree(amphipod: Amphipod): Boolean =
      (2 to roomSize + 1)
        .flatMap(y => amphipods.get(Vec2i(amphipod.room, y)))
        .forall(_ == amphipod)


    private def getPath(start: Vec2i, stop: Vec2i): Seq[Vec2i] =
      val hallway =
        if start.x < stop.x
        then (start.x + 1).to(stop.x).map(Vec2i(_, 1))
        else (start.x - 1).to(stop.x, step = -1).map(Vec2i(_, 1))
      val startRoom = (start.y - 1).to(1, step = -1).map(Vec2i(start.x, _))
      val stopRoom = 2.to(stop.y).map(Vec2i(stop.x, _))
      startRoom ++ hallway ++ stopRoom

    private def validMovesFor(from: Vec2i, kind: Amphipod): Seq[Vec2i] =
      // if amphipod is in its room 
      if from.x == kind.room then 
        // and it doesn't need to move to free space, it stays
        if roomFree(kind) then Seq.empty
        // and it needs to move to free space, it can go to the hallway
        else hallwayPoses
      // if amphipod is in the hallway it can go to its destination
      else if from.y == 1 then
        if roomFree(kind) then
          2.to(roomSize + 1).map(y => Vec2i(kind.room, y)).findLast(it => !amphipods.contains(it)).toSeq
        else Seq.empty
      // otherwise it can go to the hallway
      else hallwayPoses

  lazy val input = FileIO.getInput(2021, 23)


  // its amazing how much faster it is when you have a sensible ordering for your priority queue
  def search[A](start: State): Option[Int] = {

    val gscore = mutable.HashMap(start -> 0)

    val openSet = mutable.PriorityQueue((start, 0))(using Ordering.by[(State, Int), Int](_._2).reverse)
    while (openSet.nonEmpty) {
      val (current, curEnergy) = openSet.dequeue()

      if (current.isGoal) 
        return Some(curEnergy)
      for ((neighbor, nEnergy) <- current.neighbors) {
        val stinkyGScore = curEnergy + nEnergy
        if (stinkyGScore < gscore.getOrElse(neighbor, Int.MaxValue)) {
          gscore(neighbor) = stinkyGScore
          if (!openSet.exists(_ == neighbor)) 
            openSet.enqueue((neighbor, stinkyGScore))
        }
      }
    }

    None
  }

  def parseShared(input: String, roomSize: Int): State =
    val amphipods =
      for 
        (line, y) <- input.linesIterator.zipWithIndex
        (char, x) <- line.zipWithIndex
        amphipod <- Amphipod.tryFromChar(char)
      yield Vec2i(x, y) -> amphipod
    State(amphipods.toMap, roomSize)
  def parseP1(str: String): State =
    parseShared(str, 2)
  def parseP2(str: String): State =
    val lines = str.linesIterator
    val newInput = (lines.take(3) ++ Seq("  #D#C#B#A#", "  #D#B#A#C#") ++ lines.take(2)).mkString("\n")
    parseShared(newInput, 4)
  

  def part1(input: State): Int =
    search(input).get
  
  def part2(input: State): Int =
    search(input).get

