package gay.menkissing.bench

class ListStatistics(val samples: Vector[Double]):
  def max: Double =
    samples.maxOption.getOrElse(Double.NaN)
  def min: Double =
    samples.minOption.getOrElse(Double.NaN)

  def n: Long = samples.size

  def sum: Double =
    if samples.nonEmpty then samples.sum else Double.NaN

  def mean: Double =
    if n != 0 then
      sum / n.toDouble
    else
      Double.NaN

  def standardDeviation: Double =
    math.sqrt(variance)

  def variance: Double =
    val m = mean
    samples.map(x => math.pow(x - m, 2)).sum / n

  def unbiasedVariance: Double =
    val m = mean
    samples.map(x => math.pow(x - m, 2)).sum / (n - 1)

  def sampleStandardDeviation: Double =
    math.sqrt(unbiasedVariance)

  def standardErrorOfTheMean: Double = {
    val mean = this.mean
    val n = samples.length.toDouble
    Math.sqrt(samples.map(xi => Math.pow(xi - mean, 2)).sum / (n * (n - 1)))
  }

  def meanErrorAt(confidence: Double): Double =
    if n <= 2 then
      Double.NaN
    else
      val dist = util.TDistribution(n - 1)
      val a = dist.inverseCumulativeProbability(1 - (1 - confidence) / 2)
      a * sampleStandardDeviation / math.sqrt(n)

  def confidenceIntervalAt(confidence: Double): Option[(Double, Double)] =
    Option.when(n > 2):
      val dist = util.TDistribution(n - 1)
      val a = dist.inverseCumulativeProbability(1 - (1 - confidence) / 2)
      val mean = this.mean
      (
        mean - a * sampleStandardDeviation / math.sqrt(n),
        mean + a * sampleStandardDeviation / math.sqrt(n)
      )



