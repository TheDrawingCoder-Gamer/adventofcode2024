package gay.menkissing.bench


import cats.syntax.all.*

case class IterationResult(name: String, stats: ListStatistics, unit: TimeUnit):
  def mean: Double = stats.mean
  def error: Double = stats.meanErrorAt(0.999)

  def pretty: String =
    val goodMean = TimeUnit.Nanoseconds.convertTo(mean, unit)
    val goodError = TimeUnit.Nanoseconds.convertTo(error, unit)

    f"$name $goodMean%1.3f [+/-] ${goodError}%1.3f ${unit.display}"

  def fullResult: String =
    val goodMean = TimeUnit.Nanoseconds.convertTo(mean, unit)
    val goodError = TimeUnit.Nanoseconds.convertTo(error, unit)
    val (ciMin, ciMax) = stats.confidenceIntervalAt(0.999).getOrElse((Double.NaN, Double.NaN))
    val goodMin = TimeUnit.Nanoseconds.convertTo(stats.min, unit)
    val goodMax = TimeUnit.Nanoseconds.convertTo(stats.max, unit)
    val goodStdDev = TimeUnit.Nanoseconds.convertTo(stats.standardDeviation, unit)
    val goodCiMin = TimeUnit.Nanoseconds.convertTo(ciMin, unit)
    val goodCiMax = TimeUnit.Nanoseconds.convertTo(ciMax, unit)
    f"""
       |results for $name:
       |  $goodMean%1.3f +-(99.9%%) $goodError%1.3f ${unit.display}/op [Average]
       |  (min, avg, max) = ($goodMin%1.3f, $goodMean%1.3f, $goodMax%1.3f), stdev = $goodStdDev%1.3f
       |  CI (99.9%%): [$goodCiMin%1.3f, $goodCiMax%1.3f] (assumes normal distribution)
       |""".stripMargin

  def hocon: String =
    val goodMean = TimeUnit.Nanoseconds.convertTo(mean, unit)
    val goodError = TimeUnit.Nanoseconds.convertTo(error, unit)

    f"""[$goodMean%1.3f, $goodError%1.3f,${unit.serialized}]""".stripMargin

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