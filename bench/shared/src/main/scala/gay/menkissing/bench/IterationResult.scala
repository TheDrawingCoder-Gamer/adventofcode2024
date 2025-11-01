package gay.menkissing.bench

import cats.syntax.all.*

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import util.*

case class IterationFailure(name: String, why: String)

case class IterationResult(name: String, stats: ListStatistics, unit: TimeUnit):
  def mean: Double = stats.mean
  def error: Double = stats.meanErrorAt(0.999)

  def pretty: String =
    val goodMean = Duration(mean, TimeUnit.NANOSECONDS).toUnit(unit)
    val goodError = Duration(error, TimeUnit.NANOSECONDS).toUnit(unit)

    f"$name $goodMean%1.3f [+/-] ${goodError}%1.3f ${unit.display}"

  def fullResult: String =
    val goodMean = Duration(mean, TimeUnit.NANOSECONDS).toUnit(unit)
    val goodError = Duration(error, TimeUnit.NANOSECONDS).toUnit(unit)
    val (ciMin, ciMax) =
      stats.confidenceIntervalAt(0.999).getOrElse((Double.NaN, Double.NaN))
    val goodMin = Duration(stats.min, TimeUnit.NANOSECONDS).toUnit(unit)
    val goodMax = Duration(stats.max, TimeUnit.NANOSECONDS).toUnit(unit)
    val goodStdDev =
      Duration(stats.sampleStandardDeviation, TimeUnit.NANOSECONDS).toUnit(unit)
    val goodCiMin = Duration(ciMin, TimeUnit.NANOSECONDS).toUnit(unit)
    val goodCiMax = Duration(ciMax, TimeUnit.NANOSECONDS).toUnit(unit)
    f"""
       |results for $name:
       |  $goodMean%1.3f +-(99.9%%) $goodError%1.3f ${unit.display}/op [Average]
       |  sample size = ${stats.n}
       |  (min, avg, max) = ($goodMin%1.3f, $goodMean%1.3f, $goodMax%1.3f), stdev = $goodStdDev%1.3f
       |  CI (99.9%%): [$goodCiMin%1.3f, $goodCiMax%1.3f] (assumes normal distribution)
       |""".stripMargin

  def hocon: String =
    val goodMean = Duration(mean, TimeUnit.NANOSECONDS).toUnit(unit)
    val goodError = Duration(error, TimeUnit.NANOSECONDS).toUnit(unit)

    f"""[$goodMean%1.3f, $goodError%1.3f,${unit.serialized}]""".stripMargin

  def fullHocon: String =
    f"""
       |$name = {
       | mean = $mean
       | stderr = $error
       | unit = ns
       | asUnit = ${unit.serialized}
       |}
       |""".stripMargin

  def sbtDsl: String =
    s"""
       |"$name" -> BenchmarkTiming(
       |  mean = ${mean}d,
       |  error = ${error}d,
       |  unit = Some(TimeUnit.NANOSECONDS),
       |  convertTo = Some(TimeUnit.${unit.name()})
       |)
       |""".stripMargin

/*
object IterationResult:
  def parse(str: String): IterationResult =
    str match
      case s"$name;$mean;$stdev;$error;$unit;$ci1;$ci2" =>
        IterationResult(name, mean.toDouble, stdev.toDouble, error.toDouble, TimeUnit.parse(unit), (ci1.toDoubleOption, ci2.toDoubleOption).tupled)
      case _ =>
        println(str)
        throw new RuntimeException("Not a valid IterationResult")
 */
