import scala.io.Source
import cats.PartialOrder
import gay.menkissing.common.*

val data = Source.fromResource("day5.txt").getLines().toList

val (rawOrderingRules, badUpdates) = data.span(!_.isBlank)

val orderingRules = rawOrderingRules.map { it =>
  val c = it.split('|').map(_.toInt)
  (c(0), c(1))
}
val updates = badUpdates.drop(1).map(_.split(',').map(_.toInt).toList)

def testUpdate(update: List[Int]): Boolean = {
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

val (validUpdates, invalidUpdates) = updates.partition(testUpdate)

validUpdates.map(getCentral).sum

def ruleOrdering(rules: List[(Int, Int)]): PartialOrder[Int] = {
  new PartialOrder[Int] {
    override def partialCompare(x: Int, y: Int): Double = {
      if (x == y) {
        0.0
      } else if (rules.contains((x, y))) {
        -1.0
      } else if (rules.contains(y, x)) {
        1.0
      } else Double.NaN
    }
  }
}



val ordering = ruleOrdering(orderingRules)

ordering.partialCompare(97, 13)

debugTiming {
  invalidUpdates.map(it => topologicalSort(it)(using ordering).get).map(getCentral).sum
}
