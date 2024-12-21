package gay.menkissing.common

import scala.collection.mutable as mut
import cats.PartialOrder
import cats.*
import cats.data.Kleisli
import cats.syntax.all.*

import scala.annotation.tailrec



def reconstructPaths[A](cameFrom: Map[A, List[A]], p: A, start: A): List[List[A]] = {
  def dfs(src: A, daPath: List[A]): Eval[List[List[A]]] = {
    if (src == start) {
      Eval.now(List(daPath))
    } else {
      cameFrom(src).foldLeft(Eval.now(List[List[A]]())) { (acc, adjNode) =>
        acc.flatMap { cc =>
          dfs(adjNode, daPath.prepended(adjNode)).map(_ ++ cc)
        }
      }
    }
  }

  dfs(p, List(p)).value
}

def equals[A](l: A, r: A): Boolean =
  l.equals(r)

def dijstraAll[A](start: A, isGoal: A => Boolean, d: (A, A) => Double, neighbors: A => IterableOnce[A], eqv: (A, A) => Boolean = equals): List[List[A]] = {
  val visited = mut.Set.empty[A]

  val queue = mut.PriorityQueue((0d, start, List(start)))(using Ordering.by[(Double, A, List[A]), Double](it => it._1).reverse)
  while (!isGoal(queue.head._2)) {
    val (score, current, path) = queue.dequeue()

    for (neighbor <- neighbors(current).iterator.filterNot(visited)) {
      val nscore = score + d(current, neighbor)
      val npath = path.prepended(neighbor)

      queue.addOne((nscore, neighbor, npath))
    }
    visited.addOne(current)
  }


  val (shortestScore, s, path) = queue.dequeue()
  val paths = mut.ListBuffer[List[A]](path)
  while (queue.head._1 == shortestScore) {
    val next = queue.dequeue()
    if (eqv(next._2, s)) paths.prepend(next._3)
  }

  paths.toList
}

def findAllPathsNoRetraverse[A](start: A, isGoal: A => Boolean, neighbors: A => IterableOnce[A]): List[List[A]] = {
  val paths = mut.ListBuffer.empty[List[A]]

  def dfs(src: A, daPath: List[A]): Eval[List[List[A]]] = {
    if (isGoal(src)) {
      Eval.now(List(daPath.reverse))
    } else {
      val ns = neighbors(src).iterator.toList
      ns match {
        case Nil => Eval.now(List())
        case ls =>
          ls.foldLeft(Eval.now(List[List[A]]())) { (acc, adjNode) =>
            if (!daPath.contains(adjNode)) {
              acc.flatMap { c =>
                dfs(adjNode, daPath.prepended(adjNode)).map { cs =>
                  c ++ cs
                }
              }
            } else {
              Eval.now(List())
            }
          }
      }
    }
  }

  dfs(start, List(start)).value
}

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

extension[A](f: A => A) {
  def repeated(n: Int): A => A = {
    MonoidK[Endo].algebra[A].combineN(f, n)
  }
}

extension[A, G[_]](f: A => G[A])(using Monad[G]) {
  def flatRepeated(n: Int): A => G[A] = {
    Kleisli.endoMonoidK[G].algebra[A].combineN(Kleisli[G, A, A](f), n).run
  }
}

// may not be unique
def prettyCharForNum(num: Int): Char = {
  if (num < 10) {
    ('0' + num).toChar
  } else if (num < 10 + 26) {
    ('a' + num - 10).toChar
  } else if (num < 10 + 26 + 26) {
    ('A' + num - 10 - 26).toChar
  } else '?'
}

def logBaseN(n: Double, base: Double): Double = math.log(n) / math.log(base)

def fix[A, B](f: (A => B, A) => B)(v: A): B =
  f(fix(f), v)

def oneOf(values: Boolean*): Boolean =
  @tailrec
  def go(values: List[Boolean], c: Int): Boolean =
    if c > 1 then
      false
    else
        values match
          case head :: next => go(next, c + (if head then 1 else 0))
          case Nil =>
            c == 1
  go(values.toList, 0)

extension[A, B] (map: mut.HashMap[A, B])
  def memo(in: A)(func: => B): B = map.getOrElseUpdate(in, func)

private def bfsImpl[A, B, C](a: A, z: C, append: (C, B) => C)(f: A => Either[Iterable[A], B]): C =
  var result = z
  val queue = mut.Queue(a)
  while queue.nonEmpty do
    f(queue.dequeue()) match
      case Right(r) => result = append(result, r)
      case Left(states) => queue.enqueueAll(states)
  result

def bfsFoldl[A, B](a: A)(f: A => Either[Iterable[A], B])(using M: Monoid[B]): B =
  bfsImpl(a, M.empty, M.combine)(f)