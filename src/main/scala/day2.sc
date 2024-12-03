import scala.io.Source
import cats.implicits.*
import cats.syntax.all.*

val reports = Source.fromResource("day2.txt").getLines().map(_.split(' ').map(_.toInt).toList).toList

def testSafe(report: List[Int]): Boolean = {
  val inc = Integer.signum(report.tail.head - report.head)
  if (inc == 0) {
    false
  } else {
    report.sliding(2).forall {
      case List(x, y) => {
        val diff = y - x
        Math.abs(diff) >= 1 && Math.abs(diff) <= 3 && Integer.signum(diff) == inc
      }
      case _ => ???
    }
  }
}

def testSafeDampener(report: List[Int]): Boolean = {
  val inc = {
    val c = Integer.signum(report.tail.head - report.head)
    if (c == 0) {
      Integer.signum(report.tail.tail.head - report.tail.head)
    } else c
  }
  if (inc == 0) {
    false
  } else {
    val r = report.zipWithIndex.sliding(2).collectFirst(Function.unlift {
      case List((x, i), (y, _)) => {
        val diff = y - x
        Option.when(!(Math.abs(diff) >= 1 && Math.abs(diff) <= 3 && Integer.signum(diff) == inc))(i)
      }
      case _ => ???
    })
    r.forall { it =>
      val l = report.take(it)
      val r = report.drop(it + 1)
      testSafe(l ++ r) || {
        val l2 = report.take(it + 1)
        val r2 = report.drop(it + 2)
        testSafe(l2 ++ r2)
      }
    }
  }


}



reports.count(testSafe)
reports.count(testSafeDampener)