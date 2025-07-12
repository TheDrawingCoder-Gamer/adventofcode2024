package gay.menkissing.advent.y2024

import cats.*
import cats.implicits.*
import gay.menkissing.advent.{FileIO, Problem}
import gay.menkissing.common.*


object Day05 extends Problem[(List[(Int, Int)], List[List[Int]]), Int]:
  lazy val input = FileIO.getInput(2024, 5)

  override def parse(str: String): (List[(Int, Int)], List[List[Int]]) =

    val data = str.linesIterator.toList

    val (rawOrderingRules, badUpdates) = data.span(!_.isBlank)

    val orderingRules = rawOrderingRules.map { it =>
      val c = it.split('|').map(_.toInt)
      (c(0), c(1))
    }
    val updates = badUpdates.drop(1).map(_.split(',').map(_.toInt).toList)

    (orderingRules, updates)

  def testUpdate(orderingRules: List[(Int, Int)], update: List[Int]): Boolean = {
    var goodRules = orderingRules

    goodRules = goodRules.filter {
      case (l, r) => update.contains(l) && update.contains(r)
    }

    update.forall { page =>
      goodRules = goodRules.filterNot(_._1 == page)
      !goodRules.exists(_._2 == page)
    }
  }

  def getCentral(list: List[Int]): Int = {
    val l = list.length / 2
    list(l)
  }

  override def part1(input: (List[(Int, Int)], List[List[Int]])): Int =
    val (orderingRules, updates) = input
    val (validUpdates, invalidUpdates) = updates.partition(testUpdate(orderingRules, _))


    validUpdates.map(getCentral).sum

  override def part2(input: (List[(Int, Int)], List[List[Int]])): Int =
    val (orderingRules, updates) = input
    val (validUpdates, invalidUpdates) = updates.partition(testUpdate(orderingRules, _))
    given PartialOrder[Int] = (x, y) =>
      if (x == y) {
        0.0
      } else if (orderingRules.contains((x, y))) {
        -1.0
      } else if (orderingRules.contains(y, x)) {
        1.0
      } else Double.NaN
    
    invalidUpdates.map(it => topologicalSort(it).get).map(getCentral).sum

