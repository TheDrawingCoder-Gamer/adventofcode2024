package gay.menkissing.bench.util

/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * <p> This is a utility class that provides computation methods related to the
 * Beta family of functions. </p> <p> Implementation of
 * {@link # logBeta ( double, double)} is based on the algorithms described in
 * <ul> <li><a href="http://dx.doi.org/10.1145/22721.23109">Didonato and Morris
 * (1986)</a>, <em>Computation of the Incomplete Gamma Function Ratios and their
 * Inverse</em>, TOMS 12(4), 377-393,</li> <li><a
 * href="http://dx.doi.org/10.1145/131766.131776">Didonato and Morris
 * (1992)</a>, <em>Algorithm 708: Significant Digit Computation of the
 * Incomplete Beta Function Ratios</em>, TOMS 18(3), 360-373,</li> </ul> and
 * implemented in the <a
 * href="http://www.dtic.mil/docs/citations/ADA476840">NSWC Library of
 * Mathematical Functions</a>, available <a
 * href="http://www.ualberta.ca/CNS/RESEARCH/Software/NumericalNSWC/site.html">here</a>.
 * This library is "approved for public release", and the <a
 * href="http://www.dtic.mil/dtic/pdf/announcements/CopyrightGuidance.pdf">Copyright
 * guidance</a> indicates that unless otherwise stated in the code, all FORTRAN
 * functions in this library are license free. Since no such notice appears in
 * the code these functions can safely be ported to Commons-Math. </p>
 */
object Beta:
  /** Maximum allowed numerical error. */
  private val DEFAULT_EPSILON = 1e-14

  /** The constant value of ½log 2π. */
  private val HALF_LOG_TWO_PI = .9189385332046727

  /**
   * <p> The coefficients of the series expansion of the Δ function. This
   * function is defined as follows </p> <center>Δ(x) = log Γ(x) - (x - 0.5) log
   * a + a - 0.5 log 2π,</center> <p> see equation (23) in Didonato and Morris
   * (1992). The series expansion, which applies for x ≥ 10, reads </p> <pre> 14
   * ====1  \                2 n
 Δ(x) = ---  >    d  (10 / x)
 x  /      n====
   * n = 0 <pre>
   */
  private val DELTA =
    Array(.833333333333333333333333333333e-01,
      -.277777777777777777777777752282e-04, .793650793650793650791732130419e-07,
      -.595238095238095232389839236182e-09, .841750841750832853294451671990e-11,
      -.191752691751854612334149171243e-12, .641025640510325475730918472625e-14,
      -.295506514125338232839867823991e-15, .179643716359402238723287696452e-16,
      -.139228964661627791231203060395e-17, .133802855014020915603275339093e-18,
      -.154246009867966094273710216533e-19, .197701992980957427278370133333e-20,
      -.234065664793997056856992426667e-21, .171348014966398575409015466667e-22)

  /**
   * Returns the <a
   * href="http://mathworld.wolfram.com/RegularizedBetaFunction.html">
   * regularized beta function</a> I(x, a, b).
   *
   * @param x
   *   Value.
   * @param a
   *   Parameter {@code a}.
   * @param b
   *   Parameter {@code b}.
   * @return
   *   the regularized beta function I(x, a, b).
   * @throws org.apache.commons.math3.exception.MaxCountExceededException
   *   if the algorithm fails to converge.
   */
  def regularizedBeta(x: Double, a: Double, b: Double): Double =
    regularizedBeta(x, a, b, DEFAULT_EPSILON, Integer.MAX_VALUE)

  /**
   * Returns the <a
   * href="http://mathworld.wolfram.com/RegularizedBetaFunction.html">
   * regularized beta function</a> I(x, a, b).
   *
   * @param x
   *   Value.
   * @param a
   *   Parameter {@code a}.
   * @param b
   *   Parameter {@code b}.
   * @param epsilon
   *   When the absolute value of the nth item in the series is less than
   *   epsilon the approximation ceases to calculate further elements in the
   *   series.
   * @return
   *   the regularized beta function I(x, a, b)
   * @throws org.apache.commons.math3.exception.MaxCountExceededException
   *   if the algorithm fails to converge.
   */
  def regularizedBeta
    (x: Double, a: Double, b: Double, epsilon: Double): Double =
    regularizedBeta(x, a, b, epsilon, Integer.MAX_VALUE)

  /**
   * Returns the regularized beta function I(x, a, b).
   *
   * @param x
   *   the value.
   * @param a
   *   Parameter {@code a}.
   * @param b
   *   Parameter {@code b}.
   * @param maxIterations
   *   Maximum number of "iterations" to complete.
   * @return
   *   the regularized beta function I(x, a, b)
   * @throws org.apache.commons.math3.exception.MaxCountExceededException
   *   if the algorithm fails to converge.
   */
  def regularizedBeta
    (x: Double, a: Double, b: Double, maxIterations: Int): Double =
    regularizedBeta(x, a, b, DEFAULT_EPSILON, maxIterations)

  /**
   * Returns the regularized beta function I(x, a, b).
   *
   * The implementation of this method is based on: <ul> <li> <a
   * href="http://mathworld.wolfram.com/RegularizedBetaFunction.html">
   * Regularized Beta Function</a>.</li> <li> <a
   * href="http://functions.wolfram.com/06.21.10.0001.01"> Regularized Beta
   * Function</a>.</li> </ul>
   *
   * @param x
   *   the value.
   * @param a
   *   Parameter {@code a}.
   * @param b
   *   Parameter {@code b}.
   * @param epsilon
   *   When the absolute value of the nth item in the series is less than
   *   epsilon the approximation ceases to calculate further elements in the
   *   series.
   * @param maxIterations
   *   Maximum number of "iterations" to complete.
   * @return
   *   the regularized beta function I(x, a, b)
   * @throws org.apache.commons.math3.exception.MaxCountExceededException
   *   if the algorithm fails to converge.
   */
  def regularizedBeta
    (
      x: Double,
      a: Double,
      b: Double,
      epsilon: Double,
      maxIterations: Int
    ): Double =
    var ret = .0
    if x.isNaN || a.isNaN || b.isNaN || x < 0 || x > 1 || a <= 0 || b <= 0 then
      ret = Double.NaN
    else if x >
        (a + 1) /
        (2 + b + a) &&
        1 - x <=
        (b + 1) /
        (2 + b + a)
    then ret = 1 - regularizedBeta(1 - x, b, a, epsilon, maxIterations)
    else
      val fraction =
        new ContinuedFraction():
          /** {@inheritDoc } */
          override def getB(n: Int, x: Double): Double =
            var ret = .0
            var m = .0
            if n % 2 == 0 then // even
              m = n / 2.0
              ret = (m * (b - m) * x) / ((a + (2 * m) - 1) * (a + (2 * m)))
            else
              m = (n - 1.0) / 2.0
              ret =
                -((a + m) * (a + b + m) * x) /
                  ((a + (2 * m)) * (a + (2 * m) + 1.0))
            ret

          /** {@inheritDoc } */
          override def getA(n: Int, x: Double) = 1.0
      ret =
        math.exp(
          (a * math.log(x)) + (b * math.log1p(-x)) - math.log(a) - logBeta(a, b)
        ) * 1.0 / fraction.evaluate(x, epsilon, maxIterations)
    ret

  /**
   * Returns the natural logarithm of the beta function B(a, b).
   *
   * The implementation of this method is based on: <ul> <li><a
   * href="http://mathworld.wolfram.com/BetaFunction.html"> Beta Function</a>,
   * equation (1).</li> </ul>
   *
   * @param a
   *   Parameter {@code a}.
   * @param b
   *   Parameter {@code b}.
   * @param epsilon
   *   This parameter is ignored.
   * @param maxIterations
   *   This parameter is ignored.
   * @return
   *   log(B(a, b)).
   * @deprecated
   *   as of version 3.1, this method is deprecated as the computation of the
   *   beta function is no longer iterative; it will be removed in version 4.0.
   *   Current implementation of this method internally calls
   *   {@link # logBeta ( double, double)}.
   */
  @deprecated def logBeta
    (a: Double, b: Double, epsilon: Double, maxIterations: Int): Double =
    logBeta(a, b)

  /**
   * Returns the value of log Γ(a + b) for 1 ≤ a, b ≤ 2. Based on the <em>NSWC
   * Library of Mathematics Subroutines</em> double precision implementation,
   * {@code DGSMLN}. In {@code BetaTest.testLogGammaSum()}, this private method
   * is accessed through reflection.
   *
   * @param a
   *   First argument.
   * @param b
   *   Second argument.
   * @return
   *   the value of {@code log(Gamma(a + b))}.
   * @throws OutOfRangeException
   *   if {@code a} or {@code b} is lower than {@code 1.0} or greater than
   *   {@code 2.0}.
   */
  private def logGammaSum(a: Double, b: Double) =
    if (a < 1.0) || (a > 2.0) then
      throw new Exception(s"$a is out of range 1.0, 2.0")
    if (b < 1.0) || (b > 2.0) then
      throw new Exception(s"$b is out of range 1.0, 2.0")
    val x = (a - 1.0) + (b - 1.0)
    if x <= 0.5 then Gamma.logGamma1p(1.0 + x)
    else if x <= 1.5 then Gamma.logGamma1p(x) + math.log1p(x)
    else Gamma.logGamma1p(x - 1.0) + math.log(x * (1.0 + x))

  /**
   * Returns the value of log[Γ(b) / Γ(a + b)] for a ≥ 0 and b ≥ 10. Based on
   * the <em>NSWC Library of Mathematics Subroutines</em> double precision
   * implementation, {@code DLGDIV}. In
   * {@code BetaTest.testLogGammaMinusLogGammaSum()}, this private method is
   * accessed through reflection.
   *
   * @param a
   *   First argument.
   * @param b
   *   Second argument.
   * @return
   *   the value of {@code log(Gamma(b) / Gamma(a + b))}.
   * @throws NumberIsTooSmallException
   *   if {@code a < 0.0} or {@code b < 10.0}.
   */
  private def logGammaMinusLogGammaSum(a: Double, b: Double) =
    if a < 0.0 then throw new Exception(s"$a is too small (smaller than 0.0)")
    if b < 10.0 then throw new Exception(s"$b is too small (smaller than 10.0)")
    /*
     * d = a + b - 0.5
     */
    var d = .0
    var w = .0
    if a <= b then
      d = b + (a - 0.5)
      w = deltaMinusDeltaSum(a, b)
    else
      d = a + (b - 0.5)
      w = deltaMinusDeltaSum(b, a)
    val u = d * math.log1p(a / b)
    val v = a * (math.log(b) - 1.0)
    if u <= v then (w - u) - v
    else
      (w - v) - u

  /**
   * Returns the value of Δ(b) - Δ(a + b), with 0 ≤ a ≤ b and b ≥ 10. Based on
   * equations (26), (27) and (28) in Didonato and Morris (1992).
   *
   * @param a
   *   First argument.
   * @param b
   *   Second argument.
   * @return
   *   the value of {@code Delta(b) - Delta(a + b)}
   * @throws OutOfRangeException
   *   if {@code a < 0} or {@code a > b}
   * @throws NumberIsTooSmallException
   *   if {@code b < 10}
   */
  private def deltaMinusDeltaSum(a: Double, b: Double) =
    if (a < 0) || (a > b) then
      throw new Exception(s"$a is out of range 0 to $b")
    if b < 10 then throw new Exception(s"$b is too small (less than 10.0)")
    val h = a / b
    val p = h / (1.0 + h)
    val q = 1.0 / (1.0 + h)
    val q2 = q * q
    /*
     * s[i] = 1 + q + ... - q**(2 * i)
     */
    val s = new Array[Double](DELTA.length)
    s(0) = 1.0
    for i <- 1 until s.length do s(i) = 1.0 + (q + q2 * s(i - 1))
    /*
     * w = Delta(b) - Delta(a + b)
     */
    val sqrtT = 10.0 / b
    val t = sqrtT * sqrtT
    var w = DELTA(DELTA.length - 1) * s(s.length - 1)
    for i <- DELTA.length - 2 to 0 by -1 do w = t * w + DELTA(i) * s(i)
    w * p / b

  /**
   * Returns the value of Δ(p) + Δ(q) - Δ(p + q), with p, q ≥ 10. Based on the
   * <em>NSWC Library of Mathematics Subroutines</em> double precision
   * implementation, {@code DBCORR}. In
   * {@code BetaTest.testSumDeltaMinusDeltaSum()}, this private method is
   * accessed through reflection.
   *
   * @param p
   *   First argument.
   * @param q
   *   Second argument.
   * @return
   *   the value of {@code Delta(p) + Delta(q) - Delta(p + q)}.
   * @throws NumberIsTooSmallException
   *   if {@code p < 10.0} or {@code q < 10.0}.
   */
  private def sumDeltaMinusDeltaSum(p: Double, q: Double) =
    if p < 10.0 then throw new Exception(s"$p is too small (< 10)")
    if q < 10.0 then throw new Exception(s"$q is too small (< 10)")
    val a = math.min(p, q)
    val b = math.max(p, q)
    val sqrtT = 10.0 / a
    val t = sqrtT * sqrtT
    var z = DELTA(DELTA.length - 1)
    for i <- DELTA.length - 2 to 0 by -1 do z = t * z + DELTA(i)
    z / a + deltaMinusDeltaSum(a, b)

  /**
   * Returns the value of log B(p, q) for 0 ≤ x ≤ 1 and p, q > 0. Based on the
   * <em>NSWC Library of Mathematics Subroutines</em> implementation,
   * {@code DBETLN}.
   *
   * @param p
   *   First argument.
   * @param q
   *   Second argument.
   * @return
   *   the value of {@code log(Beta(p, q))}, {@code NaN} if {@code p <= 0} or
   *   {@code q <= 0}.
   */
  def logBeta(p: Double, q: Double): Double =
    if p.isNaN || q.isNaN || (p <= 0.0) || (q <= 0.0) then return Double.NaN
    val a = math.min(p, q)
    val b = math.max(p, q)
    if a >= 10.0 then
      val w = sumDeltaMinusDeltaSum(a, b)
      val h = a / b
      val c = h / (1.0 + h)
      val u = -(a - 0.5) * math.log(c)
      val v = b * math.log1p(h)
      if u <= v then (((-0.5 * math.log(b) + HALF_LOG_TWO_PI) + w) - u) - v
      else
        (((-0.5 * math.log(b) + HALF_LOG_TWO_PI) + w) - v) - u
    else if a > 2.0 then
      if b > 1000.0 then
        val n = math.floor(a - 1.0).toInt
        var prod = 1.0
        var ared = a
        for i <- 0 until n do
          ared -= 1.0
          prod *= ared / (1.0 + ared / b)
        (math.log(prod) - n * math.log(b)) +
          (Gamma.logGamma(ared) + logGammaMinusLogGammaSum(ared, b))
      else
        var prod1 = 1.0
        var ared = a
        while ared > 2.0 do
          ared -= 1.0
          val h = ared / b
          prod1 *= h / (1.0 + h)
        if b < 10.0 then
          var prod2 = 1.0
          var bred = b
          while bred > 2.0 do
            bred -= 1.0
            prod2 *= bred / (ared + bred)
          math.log(prod1) + math.log(prod2) +
            (Gamma.logGamma(ared) +
              (Gamma.logGamma(bred) - logGammaSum(ared, bred)))
        else
          math.log(prod1) + Gamma.logGamma(ared) +
            logGammaMinusLogGammaSum(ared, b)
    else if a >= 1.0 then
      if b > 2.0 then
        if b < 10.0 then
          var prod = 1.0
          var bred = b
          while bred > 2.0 do
            bred -= 1.0
            prod *= bred / (a + bred)
          math.log(prod) +
            (Gamma.logGamma(a) + (Gamma.logGamma(bred) - logGammaSum(a, bred)))
        else Gamma.logGamma(a) + logGammaMinusLogGammaSum(a, b)
      else Gamma.logGamma(a) + Gamma.logGamma(b) - logGammaSum(a, b)
    else if b >= 10.0 then Gamma.logGamma(a) + logGammaMinusLogGammaSum(a, b)
    else
      // The following command is the original NSWC implementation.
      // return Gamma.logGamma(a) +
      // (Gamma.logGamma(b) - Gamma.logGamma(a + b));
      // The following command turns out to be more accurate.
      math.log(Gamma.gamma(a) * Gamma.gamma(b) / Gamma.gamma(a + b))

class Beta private

/**
 * Default constructor. Prohibit instantiation.
 */
{}
