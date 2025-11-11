package gay.menkissing.advent
package y2024


import scala.collection.immutable.BitSet

object Day23 extends ProblemAdv:
  type Input = LANConnections
  type OutputP1 = Long
  type OutputP2 = String

  override def parse(str: String): Day23.LANConnections =
    LANConnections:
      str.trim.linesIterator.map:
        case s"$x-$y" => (Computer(x), Computer(y))
      .toList

  object Computer:
    // We want this number to be as small as possible so the bitset
    // chooses the least amount of bits needed
    def apply(str: String): Int =
      val c1 = str(0) - 'a'
      val c2 = str(1) - 'a'
      // highest bit set is 5
      (c1 << 5) + c2

    def startsWithT(n: Int): Boolean = ((n >> 5) + 'a').toChar == 't'

    def unapply(n: Int): String =
      val c1 = ((n >> 5) + 'a').toChar
      val c2 = ((n & 0b11111) + 'a').toChar
      String.valueOf(Array(c1, c2))

  case class LANConnections(values: List[(Int, Int)]):
    val computerMap: Map[Int, BitSet] =
      values.flatMap(it => List((it._1, it._2), (it._2, it._1)))
        .groupMap(_._1)(_._2).view.mapValues(_.to(BitSet)).toMap

  def maximumClique(graph: Map[Int, BitSet]): BitSet =
    def maximalCliques(r: BitSet, p: BitSet, x: BitSet): Set[BitSet] =
      if p.isEmpty && x.isEmpty then Set(r)
      else
        val u = p.union(x).head
        p.diff(graph(u)).foldLeft((Set[BitSet](), p, x)):
          case ((res, p, x), v) =>
            (
              res ++ maximalCliques(
                r.incl(v),
                p.intersect(graph(v)),
                p.intersect(graph(v))
              ),
              p - v,
              x.incl(v)
            )
        ._1
    maximalCliques(BitSet(), graph.keySet.to(BitSet), BitSet()).maxBy(_.size)

  override def part1(conns: LANConnections): Long =
    val goodMap = conns.computerMap
    val res =
      for
        (n1, n2s) <- goodMap.iterator
        if Computer.startsWithT(n1)
        n2 <- n2s.iterator
        n3 <- goodMap(n2).iterator
        if n3 != n2
        n4 <- goodMap(n3).iterator
        if n4 == n1
      yield Set(n1, n2, n3)

    res.distinct.size

  override def part2(conns: LANConnections): String =
    maximumClique(conns.computerMap).map(Computer.unapply).toList.sorted
      .mkString(",")

  override lazy val input: String = FileIO.getInput(2024, 23)
