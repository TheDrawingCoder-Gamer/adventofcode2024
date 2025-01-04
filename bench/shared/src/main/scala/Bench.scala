
import gay.menkissing.bench.*
import Blackhole.*

import scala.collection.mutable as mut


// Pow of 10 related to seconds
enum TimeUnit(val pow: Int, val display: String):
  case Seconds extends TimeUnit(0, "s")
  case Milliseconds extends TimeUnit(3, "ms")
  case Microseconds extends TimeUnit(6, "μs")
  case Nanoseconds extends TimeUnit(9, "ns")

  def convertTo(sourceValue: Double, to: TimeUnit): Double =
    TimeUnit.convert(this, to, sourceValue)

object TimeUnit:
  def convert(from: TimeUnit, to: TimeUnit, value: Double): Double =
    value * math.pow(10, to.pow - from.pow)

def nanoTimed[U](a: => U): Double =
  val start = System.nanoTime()
  a
  val end = System.nanoTime()
  (end - start).toDouble

case class Benchmark(name: String, body: Blackhole.Impl => Unit, unit: TimeUnit)

case class Result(name: String, mean: Double, error: Double, unit: TimeUnit)

trait Bench:
  val warmup: Int = 3
  val measurement: Int = 3
  val unit: TimeUnit = TimeUnit.Milliseconds

  private val benchmarks = mut.ListBuffer[Benchmark]()

  final def benchmark[U](name: String, benchUnit: TimeUnit = unit)(body: => U): Unit =
    benchmarks.append(Benchmark(name, hole => {
      hole.consumed(body)
    }, benchUnit))

  def main(badArgs: Array[String]): Unit =
    val args = Args.args(badArgs)
    val daBenches =
      if args.nonEmpty then
        val reg = args(0).r
        benchmarks.filter(it => reg.matches(it._1)).toVector
      else
        benchmarks.toVector

    val hole = Blackhole.obtainBlackhole()

    val benches = daBenches.map:
      case Benchmark(name, body, unit) =>
        println(s"benchmarking $name...")
        (1 to warmup).foreach: n =>
          val time = nanoTimed(body(hole))
          hole.teardown()
          println(s"warmup $n: ${TimeUnit.Nanoseconds.convertTo(time, unit)} ${unit.display}")
          Gc.gc()
        val times = mut.ListBuffer[Double]()
        (1 to measurement).foreach: n =>
          val time = nanoTimed(body(hole))
          hole.teardown()
          println(s"iteration $n: ${TimeUnit.Nanoseconds.convertTo(time, unit)} ${unit.display}")
          times.append(time)
          Gc.gc()
        val stats = ListStatistics(times.toVector)
        Result(name, stats.mean, stats.standardErrorOfTheMean, unit)

    println("results: ")
    benches.foreach:
      case Result(name, mean, error, unit) =>
        val goodMean = TimeUnit.Nanoseconds.convertTo(mean, unit)
        val goodError = TimeUnit.Nanoseconds.convertTo(error, unit)

        println(s"$name $goodMean [+/-] ${goodError} ${unit.display}")



