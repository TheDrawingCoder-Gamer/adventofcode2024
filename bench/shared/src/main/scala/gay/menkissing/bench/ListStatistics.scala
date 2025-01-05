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
    val m = mean
    val variance = samples.map(x => math.pow(x - m, 2)).sum / n
    math.sqrt(variance)

  def standardErrorOfTheMean: Double = {
    val mean = this.mean
    val n = samples.length.toDouble
    Math.sqrt(samples.map(xi => Math.pow(xi - mean, 2)).sum / (n * (n - 1)))
  }





