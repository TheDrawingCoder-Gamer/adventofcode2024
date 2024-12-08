import scala.io.Source
import scala.collection.parallel.CollectionConverters.*



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

def combinationsOfP1(n: Int): Seq[String] = combinationsOfGeneric(n, Seq('+', '*'))
def combinationsOfP2(n: Int): Seq[String] = combinationsOfGeneric(n, Seq('+', '*', '|'))

case class Equation(result: Long, inputs: Seq[Long]) {
  def canBeTrue: Boolean = {
    assert(inputs.sizeIs > 1)
    val combos = inputs.size - 1
    combinationsOfP1(combos).exists { str =>
      applyCombo(str) == result

    }
  }
  def canBeTrueP2: Boolean = {
    assert(inputs.sizeIs > 1)
    val combos = inputs.size - 1
    combinationsOfP2(combos).exists { str =>
      applyCombo(str) == result
    }
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

data.par.filter(_.canBeTrueP2).map(_.result).sum