package gay.menkissing.bench.util

def fuzzyEquals(l: Double, r: Double, by: Double): Boolean = {
  math.abs(r - l) <= by
}

def calculateErrorOfMeanAt(n: Long, stdev: Double, confidence: Double): Double = {
  if n <= 2 then
    Double.NaN
  else
    val dist = TDistribution(n - 1)
    val a = dist.inverseCumulativeProbability(1 - (1 - confidence) / 2)
    a * stdev / math.sqrt(n)
}