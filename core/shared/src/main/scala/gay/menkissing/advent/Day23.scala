package gay.menkissing.advent

import gay.menkissing.advent.ProblemAdv
import gay.menkissing.common.*

import scala.annotation.tailrec
import scala.collection.{AbstractIterator, mutable as mut, immutable as immut}
import scala.io.Source

object Day23 extends ProblemAdv[Day23.LANConnections, Long, String]:
  override def parse(str: String): Day23.LANConnections =
    LANConnections:
      str.linesIterator.map:
        case s"$x-$y" =>
          (Computer(x), Computer(y))
      .toList

  object Computer:
    // We want this number to be as small as possible so the bitset
    // chooses the least amount of bits needed
    def apply(str: String): Int =
      val c1 = str(0) - 'a'
      val c2 = str(1) - 'a'
      // highest bit set is 5
      (c1 << 5) + c2

    def startsWithT(n: Int): Boolean =
      (n >> 5).toChar == 't'

    def unapply(n: Int): String =
      val c1 = (n >> 5).toChar
      val c2 = (n & 0b11111).toChar
      String.valueOf(Array(c1, c2))

  case class LANConnections(values: List[(Int, Int)]):
    val allComputers: Set[Int] = values.iterator.flatMap((x, y) => Iterator(x, y)).toSet
    val computerMap: Map[Int, Set[Int]] =
      values.flatMap(it => List((it._1, it._2), (it._2, it._1))).groupMap(_._1)(_._2).view.mapValues(_.toSet).toMap


  @specialized
  def maximumClique[A](graph: Map[A, Set[A]]): Set[A] =
    @specialized
    def maximalCliques(r: Set[A], p: Set[A], x: Set[A]): Set[Set[A]] =
      if p.isEmpty && x.isEmpty then
        Set(r)
      else
        val u = p.union(x).head
        p.diff(graph(u)).foldLeft((Set[Set[A]](), p, x)):
          case ((res, p, x), v) =>
            (res ++ maximalCliques(r.incl(v), p.intersect(graph(v)), p.intersect(graph(v))), p - v, x.incl(v))
        ._1
    maximalCliques(Set(), graph.keySet, Set()).maxBy(_.size)


  override def part1(conns: LANConnections): Long =
    val goodMap = conns.computerMap.map((x, y) => (x, y.to(immut.BitSet)))
    val res = for {
      (n1, n2s) <- goodMap.iterator
      if Computer.startsWithT(n1)
      n2 <- n2s.iterator
      n3 <- goodMap(n2).iterator
      if n3 != n2
      n4 <- goodMap(n3).iterator
      if n4 == n1
    } yield Set(n1, n2, n3)

    res.distinct.size

  override def part2(conns: LANConnections): String =
    maximumClique(conns.computerMap).map(Computer.unapply).toList.sorted.mkString(",")

  override lazy val input: String = FileIO.getInput(2024, 23)