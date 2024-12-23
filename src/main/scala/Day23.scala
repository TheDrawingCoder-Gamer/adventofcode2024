import gay.menkissing.advent.ProblemAdv
import gay.menkissing.common.*

import scala.io.Source
import scala.collection.{AbstractIterator, mutable as mut}
import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.immutable.ParVector
import cats.syntax.all.*

import scala.annotation.tailrec

object Day23 extends ProblemAdv[Day23.LANConnections, Long, String]:
  override def parse(str: String): Day23.LANConnections =
    LANConnections:
      str.linesIterator.map:
        _.split('-') match
          case Array(x, y) => (x, y)
      .toList

  case class LANConnections(values: List[(String, String)]):
    val allComputers: List[String] = values.flatMap((x, y) => List(x, y)).distinct
    val computerMap: Map[String, Set[String]] = values.flatMap(it => List((it._1, it._2), (it._2, it._1))).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap

    val containsCache: mut.HashMap[Set[String], Boolean] = mut.HashMap[Set[String], Boolean]()

    def contains(l: String, r: String): Boolean =
      containsCache.getOrElseUpdate(
        Set(l, r),
        computerMap(l).contains(r)
      )

  final def maximalClique(graph: Map[String, Set[String]]): Set[String] =
    graph.foldLeft(Set[String]()):
      case (acc, (vert, neighbors)) =>
        if acc ⊆ neighbors then
          acc + vert
        else
          acc

  def maximumClique(graph: Map[String, Set[String]]): Set[String] =
    def maximalCliques(r: Set[String], p: Set[String], x: Set[String]): Set[Set[String]] =
      if p.isEmpty && x.isEmpty then
        Set(r)
      else
        p.foldLeft((Set[Set[String]](), p, x)):
          case ((res, p, x), v) =>
            (res ++ maximalCliques(r.incl(v), p.intersect(graph(v)), p.intersect(graph(v))), p - v, x.incl(v))
        ._1
    maximalCliques(Set(), graph.keySet, Set()).maxBy(_.size)


  override def part1(conns: LANConnections): Long =
    val freakySets = debugTiming {
      conns.allComputers.toSet.subsets(3).toVector.par.filter: l =>
        l.forall: it =>
           l.filter(_ != it).forall: r =>
               conns.contains(it, r)
      .toSet
    }
    freakySets.count: ls =>
      ls.exists(_.head == 't')

  override def part2(conns: LANConnections): String =
    maximumClique(conns.computerMap).toList.sorted.mkString(",")

  override val input: String = Source.fromResource("day23.txt").mkString


@main def main(): Unit =
  Day23.debugAndTimeP1()
  Day23.debugAndTimeP2()