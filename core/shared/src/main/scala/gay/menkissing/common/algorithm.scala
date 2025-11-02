package gay.menkissing.common

import collection.mutable
import cats.*
import cats.syntax.all.*

/**
 * Simplified generialized AStar Removes isGoal in favor of goal, which does
 * `_ == goal`. See [[astarGeneric]]
 */
def astar[A]
  (
    start: A,
    goal: A,
    h: A => Double,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A]
  )
  (using Eq[A]): Option[List[A]] =
  astarGeneric(start, _ === goal, h, d, neighbors)

def astarScore[A]
  (
    start: A,
    goal: A,
    h: A => Double,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A]
  )
  (using Eq[A]): Option[Double] =
  astarByReturning(start, _ === goal, h, d, neighbors, (_, _, score) => score)

/**
 * A generalized astar
 * @tparam A
 *   the element type in the graph
 * @param start
 *   the start point
 * @param isGoal
 *   test for end point
 * @param h
 *   the heuristic function. Represents how long it is assumed to take to go
 *   from the point supplied to the end. For a derived astar to be admissible
 *   the heuristic function must be admissable. Put simply, h must NEVER return
 *   a result greater than the actual value.
 * @param d
 *   The edge function. Returns how much it costs to take an edge. Is a double
 *   to represent edges that aren't existant (use
 *   [[scala.Double.PositiveInfinity]] for this case). If the edge weight
 *   depends on direction, the first parameter represents the start node and the
 *   second represents the end node.
 * @param neighbors
 *   Returns all nodes that connect to the passed node.
 * @return
 *   An Option containing the shortest path taken to reach the end node. This
 *   path includes the start and end node.
 * @note
 *   If h = `_ => 0.0`, [[dijstraBy]] will be a more optimized version of this
 *   function.
 */
def astarGeneric[A]
  (
    start: A,
    isGoal: A => Boolean,
    h: A => Double,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A]
  )
  (using Eq[A]): Option[List[A]] =
  astarByReturning[A, List[A]](
    start,
    isGoal,
    h,
    d,
    neighbors,
    (map, cur, _) => SearchReturns.reconstructPath(map, cur)
  )

def astarByReturning[A, R]
  (
    start: A,
    isGoal: A => Boolean,
    h: A => Double,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A],
    returning: (mutable.Map[A, A], A, Double) => R
  )
  (using Eq[A]): Option[R] =
  val cameFrom = mutable.HashMap[A, A]()

  val gscore = mutable.HashMap(start -> 0d)

  // ts is broken in someway but im lowk too stupid to snow how
  val fscore = mutable.HashMap(start -> h(start))

  val openSet = MinBinaryHeap[A, Double]()
  openSet.insert(start, h(start))
  while openSet.nonEmpty do
    val (current, priority) = openSet.extract()

    // if this is the shortest path found
    if priority <= fscore(current) then
      if isGoal(current) then
        return Some(returning(cameFrom, current, gscore(current)))
      neighbors(current).foreach: neighbor =>
        val stinkyGScore = gscore(current) + d(current, neighbor)
        if gscore.get(neighbor).forall(stinkyGScore < _) then
          cameFrom(neighbor) = current
          gscore(neighbor) = stinkyGScore
          val daFScore = stinkyGScore + h(neighbor)
          fscore(neighbor) = daFScore
          // let the filter above handle the case where its already in the queue
          openSet.insert(neighbor, daFScore)

  None

def dijstraByReturning[A, R]
  (
    start: A,
    isGoal: A => Boolean,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A],
    returning: (mutable.Map[A, A], A, Double) => R
  )
  (using Eq[A]): Option[R] =
  val cameFrom = mutable.HashMap[A, A]()

  val gscore = mutable.HashMap(start -> 0d)

  val openSet = MinBinaryHeap[A, Double]()
  openSet.insert(start, 0d)

  while openSet.nonEmpty do
    val (current, priority) = openSet.extract()
    val curScore = gscore(current)

    if curScore == priority then
      if isGoal(current) then
        return Some(returning(cameFrom, current, curScore))
      neighbors(current).foreach: neighbor =>
        val stinkyGScore = curScore + d(current, neighbor)
        if gscore.get(neighbor).forall(stinkyGScore < _) then
          cameFrom(neighbor) = current
          gscore(neighbor) = stinkyGScore
          // insert is safe, we checked to see if this is the smallest
          openSet.insert(neighbor, stinkyGScore)

  None

/**
 * Dijstra's algorithm, but generic in its goal checker.
 * @tparam A
 *   type of node in the graph
 * @param start
 *   the start node
 * @param isGoal
 *   test for the goal node
 * @param d
 *   distance between two node. First node is source, second is destination
 * @param neighbors
 *   possible points to move to next after this node
 * @return
 *   An Option containing the shortest path taken to reach the end node. This
 *   path includes the start and end node.
 */
def dijstraBy[A]
  (
    start: A,
    isGoal: A => Boolean,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A]
  )
  (using Eq[A]): Option[List[A]] =
  dijstraByReturning[A, List[A]](
    start,
    isGoal,
    d,
    neighbors,
    (map, cur, _) => SearchReturns.reconstructPath(map, cur)
  )

def dijstra[A]
  (
    start: A,
    goal: A,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A]
  )
  (using eq: Eq[A]): Option[List[A]] =
  dijstraBy(start, _ === goal, d, neighbors)

def dijstraByScore[A]
  (
    start: A,
    isGoal: A => Boolean,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A]
  )
  (using Eq[A]): Option[Double] =
  dijstraByReturning[A, Double](start, isGoal, d, neighbors, (_, _, v) => v)

def dijstraScore[A]
  (
    start: A,
    goal: A,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A]
  )
  (using eq: Eq[A]): Option[Double] =
  dijstraByScore(start, _ === goal, d, neighbors)

def equals[A](l: A, r: A): Boolean = l.equals(r)

def dijstraAll[A]
  (
    start: A,
    isGoal: A => Boolean,
    d: (A, A) => Double,
    neighbors: A => IterableOnce[A],
    eqv: (A, A) => Boolean = equals
  ): List[List[A]] =
  val visited = mutable.Set.empty[A]

  val queue =
    mutable.PriorityQueue((0d, start, List(start)))(using
      Ordering.by[(Double, A, List[A]), Double](it => it._1).reverse
    )
  while !isGoal(queue.head._2) do
    val (score, current, path) = queue.dequeue()

    for neighbor <- neighbors(current).iterator.filterNot(visited) do
      val nscore = score + d(current, neighbor)
      val npath = path.prepended(neighbor)

      queue.addOne((nscore, neighbor, npath))
    visited.addOne(current)

  val (shortestScore, s, path) = queue.dequeue()
  val paths = mutable.ListBuffer[List[A]](path)
  while queue.head._1 == shortestScore do
    val next = queue.dequeue()
    if eqv(next._2, s) then paths.prepend(next._3)

  paths.toList

def findAllPathsNoRetraverse[A]
  (
    start: A,
    isGoal: A => Boolean,
    neighbors: A => IterableOnce[A]
  ): List[List[A]] =
  val paths = mutable.ListBuffer.empty[List[A]]

  def dfs(src: A, daPath: List[A]): Eval[List[List[A]]] =
    if isGoal(src) then Eval.now(List(daPath.reverse))
    else
      val ns = neighbors(src).iterator.toList
      ns match
        case Nil => Eval.now(List())
        case ls  =>
          ls.foldLeft(Eval.now(List[List[A]]())): (acc, adjNode) =>
            if !daPath.contains(adjNode) then
              acc.flatMap: c =>
                dfs(adjNode, daPath.prepended(adjNode)).map: cs =>
                  c ++ cs
            else Eval.now(List())

  dfs(start, List(start)).value

def findAllPathsGeneric[A]
  (
    start: A,
    isGoal: A => Boolean,
    neighbors: A => IterableOnce[A]
  ): List[List[A]] =
  val paths = mutable.ListBuffer.empty[List[A]]

  def dfs(src: A, daPath: List[A]): Unit =
    if isGoal(src) then paths.prepend(daPath.reverse)
    else
      neighbors(src).iterator.foreach: adjNode =>
        dfs(adjNode, daPath.prepended(adjNode))

  dfs(start, List(start))

  paths.toList

def findAllPaths[A]
  (
    start: A,
    dest: A,
    neighbors: A => IterableOnce[A]
  ): List[List[A]] = findAllPathsGeneric[A](start, _ == dest, neighbors)

object SearchReturns:
  def reconstructPath[A](cameFrom: mutable.Map[A, A], p: A): List[A] =

    val totalPath = mutable.ListBuffer[A](p)
    var current = p
    while cameFrom.contains(current) do
      current = cameFrom(current)
      totalPath.prepend(current)
    totalPath.toList

  def lengthFromPath[A](cameFrom: mutable.Map[A, A], p: A, score: Double): Int =
    var c = 1
    var current = p
    while cameFrom.contains(current) do
      current = cameFrom(current)
      c += 1
    c
