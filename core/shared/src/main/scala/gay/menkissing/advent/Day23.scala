package gay.menkissing.advent

import gay.menkissing.advent.ProblemAdv
import gay.menkissing.common.*

import scala.annotation.tailrec
import scala.collection.{AbstractIterator, mutable as mut}
import scala.io.Source

object Day23 extends ProblemAdv[Day23.LANConnections, Long, String]:
  override def parse(str: String): Day23.LANConnections =
    LANConnections:
      str.linesIterator.map:
        _.split('-') match
          case Array(x, y) => (x, y)
      .toList

  case class LANConnections(values: List[(String, String)]):
    val allComputers: List[String] = values.flatMap((x, y) => List(x, y)).distinct
    val computerMap: Map[String, Set[String]] =
      values.flatMap(it => List((it._1, it._2), (it._2, it._1))).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap


  def maximumClique(graph: Map[String, Set[String]]): Set[String] =
    def maximalCliques(r: Set[String], p: Set[String], x: Set[String]): Set[Set[String]] =
      if p.isEmpty && x.isEmpty then
        Set(r)
      else
        val u = p.union(x).head
        p.diff(graph(u)).foldLeft((Set[Set[String]](), p, x)):
          case ((res, p, x), v) =>
            (res ++ maximalCliques(r.incl(v), p.intersect(graph(v)), p.intersect(graph(v))), p - v, x.incl(v))
        ._1
    maximalCliques(Set(), graph.keySet, Set()).maxBy(_.size)


  override def part1(conns: LANConnections): Long =
    conns.allComputers.toSet.subsets(3).filter: l =>
      l.exists(_.head == 't')
        && l.forall: it =>
            l.filter(_ != it).forall: r =>
              conns.computerMap(it).contains(r)
    .distinct.size

  override def part2(conns: LANConnections): String =
    maximumClique(conns.computerMap).toList.sorted.mkString(",")

  override lazy val input: String = FileIO.getInput(2024, 23)