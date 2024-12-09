package gay.menkissing.common

import scala.collection.mutable as mut
import cats.PartialOrder

def findAllPathsGeneric[A](start: A, isGoal: A => Boolean, neighbors: A => IterableOnce[A]): List[List[A]] = {
  val paths = mut.ListBuffer.empty[List[A]]
   
  def dfs(src: A, daPath: List[A]): Unit = {
    if (isGoal(src)) {
      paths.prepend(daPath.reverse)
    } else {
      neighbors(src).iterator.foreach { adjNode =>
        dfs(adjNode, daPath.prepended(adjNode))
      }
    }
  }
  dfs(start, List(start))
  
  paths.toList
}

def findAllPaths[A](start: A, dest: A, neighbors: A => IterableOnce[A]): List[List[A]] = findAllPathsGeneric[A](start, _ == dest, neighbors)

def reconstructPath[A](cameFrom: Map[A, A], p: A): List[A] = {

  val totalPath = mut.ListBuffer[A](p)
  var current = p
  while (cameFrom.contains(current)) {
    current = cameFrom(current)
    totalPath.prepend(current)
  }
  totalPath.toList
}

/** 
 * A generalized astar
 * @tparam A the element type in the graph
 * @param start the start point
 * @param isGoal test for end point
 * @param h the heuristic function. Represents how long it is assumed to take to go from the point supplied to the end.
 * * For a derived astar to be admissible the heuristic function must be admissable. Put simply, h must NEVER return a result greater 
 * than the actual value. 
 * @param d The edge function. Returns how much it costs to take an edge. Is a double to represent edges that aren't existant (use
 * [[scala.Double.PositiveInfinity]] for this case). If the edge weight depends on direction, the first parameter represents the start node 
 * and the second represents the end node.
 * @param neighbors Returns all nodes that connect to the passed node.
 * @return An Option containing the shortest path taken to reach the end node. This path includes the start and end node.
 */
def astarGeneric[A](start: A, isGoal: A => Boolean, h: A => Double, d: (A, A) => Double, neighbors: A => IterableOnce[A]): Option[List[A]] = {
  val cameFrom = mut.HashMap[A, A]()

  val gscore = mut.HashMap(start -> 0d)

  val fscore = mut.HashMap(start -> h(start))

  val openSet = mut.PriorityQueue(start)(using Ordering.by(it => fscore.getOrElse(it, Double.PositiveInfinity)).reverse)
  while (openSet.nonEmpty) {
    val current = openSet.dequeue()

    if (isGoal(current)) 
      return Some(reconstructPath(cameFrom.toMap, current))
    for (neighbor <- neighbors(current)) {
      val stinkyGScore = gscore.getOrElse(current, Double.PositiveInfinity) + d(current, neighbor)
      if (stinkyGScore < gscore.getOrElse(neighbor, Double.PositiveInfinity)) {
        cameFrom(neighbor) = current 
        gscore(neighbor) = stinkyGScore
        fscore(neighbor) = stinkyGScore + h(neighbor)
        if (!openSet.exists(_ == neighbor)) 
          openSet.enqueue(neighbor)
      }
    }
  }

  None
}
/**
 * Simplified generialized AStar
 * Removes isGoal in favor of goal, which does `_ == goal`. 
 * See [[astarGeneric]]
 */
def astar[A](start: A, goal: A, h: A => Double, d: (A, A) => Double, neighbors: A => IterableOnce[A]): Option[List[A]] = astarGeneric(start, _ == goal, h, d, neighbors)

def topologicalSort[A](a: List[A])(using pord: PartialOrder[A]): Option[List[A]] = {
  var unsorted = a.toBuffer
  val sorted = mut.Buffer[A]()
  
  def hasIncomingNode(n: A): Boolean = {
    unsorted.exists(it => pord.lt(it, n))
  }
  def nodeEnters(inc: A, n: A): Boolean = {
    pord.lt(inc, n)
  }
  
  val startNodes: mut.Set[A] = a.filterNot(hasIncomingNode).iterator.to(mut.Set.iterableFactory)
  
  while (startNodes.nonEmpty) {
    val n = startNodes.head
    startNodes.remove(n)
    sorted.append(n)
    unsorted = unsorted.filterNot(_ == n)
    unsorted.filter(it => nodeEnters(n, it)).foreach { m =>
      if (!hasIncomingNode(m)) {
        startNodes.add(m)
      }
    }
  }
  
  if (unsorted.nonEmpty) {
    None
  } else {
    Some(sorted.toList)
  }
}

def debugTiming[A](func: => A): A = {
  val start = System.currentTimeMillis()
  val res = func
  val end = System.currentTimeMillis()
  println(s"Elapsed: ${end - start}")
  res
}