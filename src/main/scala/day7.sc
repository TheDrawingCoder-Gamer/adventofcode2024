import scala.io.Source
import scala.collection.parallel.CollectionConverters.*
import gay.menkissing.common.debugTiming




def combinationsOfGeneric(n: Int, availableChars: Seq[Char]): Seq[String] = {
  @annotation.tailrec
  def intlCombosOf(m: Int, curOnes: Seq[String]): Seq[String] = {
    m match {
      case 0 => curOnes
      case _ => {
        val newOnes = availableChars.map(c => curOnes.map(_ + c)).reduce(_ ++ _)
        intlCombosOf(m - 1, newOnes)
      }
    }
  }
  if (n == 0) {
    Seq()
  } else {
    intlCombosOf(n - 1, availableChars.map(_.toString))
  }
}

def concatLong(l: Long, r: Long): Long = {
  val rDigits = math.log10(r.toDouble).toInt + 1
  (l * math.pow(10, rDigits)).toLong + r
}

// possibly breaks if values contains a 0
def calculateTruth(values: List[Long], finalResult: Long, runningResult: Long, part2: Boolean = false): Boolean = {
  values match {
    case Nil => finalResult == runningResult
    case v :: next => {
      if (runningResult > finalResult)
        false
      else {
        calculateTruth(next, finalResult, runningResult + v, part2)
          || calculateTruth(next, finalResult, runningResult * v, part2)
          || (if (part2) calculateTruth(next, finalResult, concatLong(runningResult, v), part2) else false)
      }
    }
  }
}


def combinationsOfP1(n: Int): Seq[String] = combinationsOfGeneric(n, Seq('+', '*'))
def combinationsOfP2(n: Int): Seq[String] = combinationsOfGeneric(n, Seq('+', '*', '|'))

case class Equation(result: Long, inputs: Seq[Long]) {
  def canBeTrue: Boolean = {
    assert(inputs.sizeIs > 1)
    calculateTruth(inputs.toList, result, 0)
  }
  def canBeTrueP2: Boolean = {
    assert(inputs.sizeIs > 1)
    calculateTruth(inputs.toList, result, 0, true)
  }

  def applyCombo(combo: String): Long = {
    assert(inputs.sizeIs == combo.length + 1)
    inputs.tail.zipWithIndex.foldLeft(inputs.head) { case (l, (r, idx)) =>
      combo(idx) match {
        case '*' => l * r
        case '+' => l + r
        case '|' => (l.toString ++ r.toString).toLong
        case _ => ???
      }
    }
  }
}

val data = Source.fromResource("day7.txt").getLines.map {
  case s"$a: $nums" => {
    Equation(a.toLong, nums.trim.split(' ').map(_.toLong))
  }
  case _ => ???
}.toSeq

debugTiming {
  data.withFilter(_.canBeTrue).map(_.result).sum
}

debugTiming {
  data.par.withFilter(_.canBeTrueP2).map(_.result).sum
}