package gay.menkissing.bench.util

case class TDistribution(degreesOfFreedom: Double):
  val factor: Double =
    val n = degreesOfFreedom
    val nPlus1Over2 = (n + 1) / 2
    Gamma.logGamma(nPlus1Over2) - 0.5 * (math.log(math.Pi) + math.log(n)) -
      Gamma.logGamma(n / 2)

  def inverseCumulativeProbability(p: Double): Double =
    if p < 0.0 || p > 1.0 then throw new Exception(s"$p out of range 0 to 1")

    var lowerBound = Double.NegativeInfinity
    if p == 0.0 then return lowerBound

    var upperBound = Double.PositiveInfinity
    if p == 1.0 then return upperBound

    val mu = this.mean
    val sig = math.sqrt(this.variance)
    val chebyshevApplies =
      !(mu.isInfinite || mu.isNaN || sig.isInfinite || sig.isNaN)

    // always true here
    if lowerBound == Double.NegativeInfinity then
      if chebyshevApplies then lowerBound = mu - sig * math.sqrt((1.0 - p) / p)
      else
        lowerBound = -1.0
        while cumulativeProbability(lowerBound) >= p do lowerBound *= 2.0

    if upperBound == Double.PositiveInfinity then
      if chebyshevApplies then upperBound = mu + sig * math.sqrt(p / (1.0 - p))
      else
        upperBound = 1.0
        while cumulativeProbability(upperBound) < p do upperBound *= 2.0

    val func = (x: Double) => cumulativeProbability(x) - p

    val x = brent(func, lowerBound, upperBound, absoluteAccuracy = 1e-9)

    // skip connection test, we are connected

    x

  def cumulativeProbability(x: Double): Double =
    if x == 0 then 0.5
    else
      val t =
        Beta.regularizedBeta(
          degreesOfFreedom / (degreesOfFreedom + (x * x)),
          0.5 * degreesOfFreedom,
          0.5
        )
      if x < 0.0 then 0.5 * t
      else 1.0 - 0.5 * t

  def mean: Double = if degreesOfFreedom > 1 then 0 else Double.NaN

  def variance: Double =
    if degreesOfFreedom > 2 then degreesOfFreedom / (degreesOfFreedom - 2)
    else if degreesOfFreedom > 1 && degreesOfFreedom <= 2 then
      Double.PositiveInfinity
    else Double.NaN
