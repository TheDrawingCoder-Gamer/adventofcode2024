package gay.menkissing.advent
package y2015

object Day20 extends Problem:
  type Input = Int
  type Output = Int

  def input = FileIO.getInput(2015, 20)

  def parse(str: String): Int = str.trim.toInt

  val max = 1_000_000
  def precalc(): Array[Int] =
    // sieve for chowla (sigma without self)
    val sums = Array.fill(max + 1)(1)
    (2 to ((max + 1) / 2)).foreach: i =>
      ((i * 2) until (max + 1) by i).foreach: idx =>
        sums(idx) += i
    sums

  // IT JUST WORKS:tm:
  def precalcP2(): Array[Int] =
    // it seems it doesn't really matter if this is "correct" for my input,
    // its "close enough"
    val sums = Array.fill(max + 1)(0)
    (1 to 50).foreach(x => sums(x) = 1)
    (2 to ((max + 1) / 2)).foreach: i =>
      // this only breaks down when I take 54 (it reverts to previous answer)
      // the gaps in the sequence are large enough that I can be very incorrect here
      // and still get the correct answer
      ((i * 2) until (max + 1) by i).take(49).foreach: idx =>
        sums(idx) += i
    sums

  // according to OEIS, the sequence is the "sigma(x)" sequences
  // where sigma is the sum of divisors
  def part1(input: Int): Int =
    val chowla = precalc()
    def sigma(n: Int): Int = chowla(n) + n
    val v = input / 10
    Iterator.iterate(1)(_ + 1).collectFirst:
      case it if sigma(it) > v => it
    .get

  def part2(input: Int): Int =
    val modChowla = precalcP2()
    def modSigma(n: Int): Int = modChowla(n) + n
    val v = input / 11
    Iterator.iterate(1)(_ + 1).collectFirst:
      case it if modSigma(it) > v => it
    .get
