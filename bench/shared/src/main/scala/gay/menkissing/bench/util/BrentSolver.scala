package gay.menkissing.bench.util

object BrentSolver:
  val absoluteAccuracy: Double = 1e-6
  val relativeAccuracy: Double = 1e-14
  val functionValueAccuracy: Double = 1e-15

def brent
  (
    func: Double => Double,
    min: Double,
    max: Double,
    start: Option[Double] = None,
    maxEval: Int = Int.MaxValue,
    absoluteAccuracy: Double = BrentSolver.absoluteAccuracy
  ): Double =
  val init = start.getOrElse(min + 0.5 * (max - min))
  var evals = 0
  def computeObjectiveValue(p: Double): Double =
    evals += 1
    if evals > maxEval then throw new Exception(s"Exceeded max evals $evals")
    func(p)

  def brentIntl(lo: Double, hi: Double, fLo: Double, fHi: Double): Double =
    var a = lo
    var fa = fLo
    var b = hi
    var fb = fHi
    var c = a
    var fc = fa
    var d = b - a
    var e = d

    val t = absoluteAccuracy
    val eps = BrentSolver.relativeAccuracy

    while true do
      if math.abs(fc) < math.abs(fb) then
        a = b
        b = c
        c = a
        fa = fb
        fb = fc
        fc = fa

      val tol = 2 * eps * math.abs(b) + t
      val m = 0.5 * (c - b)

      if math.abs(m) <= tol || fb == 0 then return b
      if math.abs(e) < tol || math.abs(fa) <= math.abs(fb) then
        d = m
        e = d
      else
        var s = fb / fa
        var p = 0.0
        var q = 0.0
        if a == c then
          p = 2 * m * s
          q = 1 - s
        else
          q = fa / fc
          val r = fb / fc
          p = s * (2 * m * q * (q - r) - (b - a) * (r - 1))
          q = (q - 1) * (r - 1) * (s - 1)
        if p > 0 then q = -q
        else p = -p
        s = e
        e = d
        if p >= 1.5 * m * q - math.abs(tol * q) || p >= math.abs(0.5 * s * q)
        then
          d = m
          e = d
        else d = p / q
      a = b
      fa = fb

      if math.abs(d) > tol then b += d
      else if m > 0 then b += tol
      else b -= tol

      fb = computeObjectiveValue(b)
      if (fb > 0 && fc > 0) || (fb <= 0 && fc <= 0) then
        c = a
        fc = fa
        d = b - a
        e = d

    0
  val yInit = computeObjectiveValue(init)
  if math.abs(yInit) <= BrentSolver.functionValueAccuracy then return init

  val yMin = computeObjectiveValue(min)
  if math.abs(yMin) <= BrentSolver.functionValueAccuracy then return min

  if yInit * yMin < 0 then return brentIntl(min, init, yMin, yInit)

  val yMax = computeObjectiveValue(max)
  if math.abs(yMax) <= BrentSolver.functionValueAccuracy then return max

  if yInit * yMax < 0 then return brentIntl(init, max, yInit, yMax)

  throw new Exception(s"No bracketing $min $max $yMin $yMax")
