package gay.menkissing.advent
package y2021

import cats.data.Chain
import cats.syntax.all.*
import cats.*

object Day12 extends Problem:
  type Input = List[(Node, Node)]
  type Output = Long

  def input = FileIO.getInput(2021, 12)

  enum Node:
    case Start
    case Big(str: String)
    case Small(str: String)
    case End

  given Hash[Node] = Hash.fromUniversalHashCode

  object Node:
    def parse(str: String): Node =
      str match
        case "start"             => Node.Start
        case "end"               => Node.End
        case s if s.head.isUpper => Node.Big(s)
        case s                   => Node.Small(s)

  def parse(str: String): List[(Node, Node)] =
    str.linesIterator.map:
      case s"$l-$r" => (Node.parse(l), Node.parse(r))
    .toList

  def checkPath(smallMax: Int)(stack: List[Node], node: Node): Boolean =
    node match
      case Node.Start     => false
      case Node.End       => true
      case Node.Big(_)    => true
      case Node.Small(sm) =>
        lazy val smalls =
          stack.collect:
            case Node.Small(sm2) => sm2
        !stack.contains(node) ||
        smalls.groupBy(identity).values.forall(_.sizeIs <= smallMax)

  def followNodes(smallMax: Int)(nodes: List[(Node, Node)]): Chain[List[Node]] =
    val chainNodes = Chain.fromSeq(nodes)
    def go(stack: List[Node]): Chain[List[Node]] =
      val pos = stack.head
      val connections =
        chainNodes.mapFilter: (x, y) =>
          if x == pos then Some(y)
          else if y == pos then Some(x)
          else None
        .filter: y =>
          checkPath(smallMax - 1)(stack, y)

      pos match
        case Node.Start | Node.Big(_) | Node.Small(_) =>
          connections.flatMap(y => go(y :: stack)).hashDistinct
        case Node.End => Chain.one(stack)

    go(List(Node.Start))

  def part1(nodes: List[(Node, Node)]): Long = followNodes(1)(nodes).length

  def part2(nodes: List[(Node, Node)]): Long = followNodes(2)(nodes).length
