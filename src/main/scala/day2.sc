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
// a valid "final" configuration must:
// be all increasing or all decreasing
// two adjacent levels differ by at least one or at most three
// this implies...
// that all numbers are unique
// that the list is strictly monotonic
// that the largest number is at most 3 * length of the list + smallest number
// (assuming its at the end)

def testSafeDampener(report: List[Int]): Boolean = {
  // I HATE THAT THIS IS INEFFICIENT
  testSafe(report) || {
    report.indices.exists { idx =>
      val newReport = report.take(idx) ++ report.drop(idx + 1)
      testSafe(newReport)
    }
  }


}



reports.count(testSafe)
reports.count(testSafeDampener)