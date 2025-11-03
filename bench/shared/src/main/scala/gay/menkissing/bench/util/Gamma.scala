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
 * &Gamma; (Gamma) family of functions. </p> <p> Implementation of
 * {@link # invGamma1pm1 ( double )} and {@link # logGamma1p ( double )} is
 * based on the algorithms described in <ul> <li><a
 * href="http://dx.doi.org/10.1145/22721.23109">Didonato and Morris (1986)</a>,
 * <em>Computation of the Incomplete Gamma Function Ratios and their
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
object Gamma:
  /**
   * <a
   * href="http://en.wikipedia.org/wiki/Euler-Mascheroni_constant">Euler-Mascheroni
   * constant</a>
   *
   * @since 2.0
   */
  val GAMMA = 0.577215664901532860606512090082

  /**
   * The value of the {@code g} constant in the Lanczos approximation, see
   * {@link # lanczos ( double )}.
   *
   * @since 3.1
   */
  val LANCZOS_G: Double = 607.0 / 128.0

  /** Maximum allowed numerical error. */
  private val DEFAULT_EPSILON = 10e-15

  /** Lanczos coefficients */
  private val LANCZOS =
    Array(0.99999999999999709182, 57.156235665862923517, -59.597960355475491248,
      14.136097974741747174, -0.49191381609762019978, .33994649984811888699e-4,
      .46523628927048575665e-4, -.98374475304879564677e-4,
      .15808870322491248884e-3, -.21026444172410488319e-3,
      .21743961811521264320e-3, -.16431810653676389022e-3,
      .84418223983852743293e-4, -.26190838401581408670e-4,
      .36899182659531622704e-5)

  /** Avoid repeated computation of log of 2 PI in logGamma */
  private val HALF_LOG_2_PI = 0.5 * math.log(2.0 * math.Pi)

  /** The constant value of &radic;(2&pi;). */
  private val SQRT_TWO_PI = 2.506628274631000502

  /** The constant {@code A0} defined in {@code DGAM1}. */
  /*
   * Constants for the computation of double invGamma1pm1(double).
   * Copied from DGAM1 in the NSWC library.
   */
  private val INV_GAMMA1P_M1_A0 = .611609510448141581788e-08

  /** The constant {@code A1} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_A1 = .624730830116465516210e-08

  /** The constant {@code B1} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_B1 = .203610414066806987300e+00

  /** The constant {@code B2} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_B2 = .266205348428949217746e-01

  /** The constant {@code B3} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_B3 = .493944979382446875238e-03

  /** The constant {@code B4} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_B4 = -.851419432440314906588e-05

  /** The constant {@code B5} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_B5 = -.643045481779353022248e-05

  /** The constant {@code B6} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_B6 = .992641840672773722196e-06

  /** The constant {@code B7} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_B7 = -.607761895722825260739e-07

  /** The constant {@code B8} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_B8 = .195755836614639731882e-09

  /** The constant {@code P0} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_P0 = .6116095104481415817861e-08

  /** The constant {@code P1} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_P1 = .6871674113067198736152e-08

  /** The constant {@code P2} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_P2 = .6820161668496170657918e-09

  /** The constant {@code P3} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_P3 = .4686843322948848031080e-10

  /** The constant {@code P4} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_P4 = .1572833027710446286995e-11

  /** The constant {@code P5} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_P5 = -.1249441572276366213222e-12

  /** The constant {@code P6} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_P6 = .4343529937408594255178e-14

  /** The constant {@code Q1} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_Q1 = .3056961078365221025009e+00

  /** The constant {@code Q2} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_Q2 = .5464213086042296536016e-01

  /** The constant {@code Q3} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_Q3 = .4956830093825887312020e-02

  /** The constant {@code Q4} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_Q4 = .2692369466186361192876e-03

  /** The constant {@code C} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C = -.422784335098467139393487909917598e+00

  /** The constant {@code C0} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C0 = .577215664901532860606512090082402e+00

  /** The constant {@code C1} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C1 = -.655878071520253881077019515145390e+00

  /** The constant {@code C2} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C2 = -.420026350340952355290039348754298e-01

  /** The constant {@code C3} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C3 = .166538611382291489501700795102105e+00

  /** The constant {@code C4} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C4 = -.421977345555443367482083012891874e-01

  /** The constant {@code C5} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C5 = -.962197152787697356211492167234820e-02

  /** The constant {@code C6} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C6 = .721894324666309954239501034044657e-02

  /** The constant {@code C7} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C7 = -.116516759185906511211397108401839e-02

  /** The constant {@code C8} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C8 = -.215241674114950972815729963053648e-03

  /** The constant {@code C9} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C9 = .128050282388116186153198626328164e-03

  /** The constant {@code C10} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C10 = -.201348547807882386556893914210218e-04

  /** The constant {@code C11} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C11 = -.125049348214267065734535947383309e-05

  /** The constant {@code C12} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C12 = .113302723198169588237412962033074e-05

  /** The constant {@code C13} defined in {@code DGAM1}. */
  private val INV_GAMMA1P_M1_C13 = -.205633841697760710345015413002057e-06

  /**
   * <p> Returns the value of log&nbsp;&Gamma;(x) for x&nbsp;&gt;&nbsp;0. </p>
   * <p> For x &le; 8, the implementation is based on the double precision
   * implementation in the <em>NSWC Library of Mathematics Subroutines</em>,
   * {@code DGAMLN}. For x &gt; 8, the implementation is based on </p> <ul>
   * <li><a href="http://mathworld.wolfram.com/GammaFunction.html">Gamma
   * Function</a>, equation (28).</li> <li><a
   * href="http://mathworld.wolfram.com/LanczosApproximation.html"> Lanczos
   * Approximation</a>, equations (1) through (5).</li> <li><a
   * href="http://my.fit.edu/~gabdo/gamma.txt">Paul Godfrey, A note on the
   * computation of the convergent Lanczos complex Gamma approximation</a></li>
   * </ul>
   *
   * @param x
   *   Argument.
   * @return
   *   the value of {@code log(Gamma(x))}, {@code Double.NaN} if
   *   {@code x <= 0.0}.
   */
  def logGamma(x: Double): Double =
    var ret = .0
    if x.isNaN || (x <= 0.0) then ret = Double.NaN
    else if x < 0.5 then return logGamma1p(x) - math.log(x)
    else if x <= 2.5 then return logGamma1p((x - 0.5) - 0.5)
    else if x <= 8.0 then
      val n = math.floor(x - 1.5).toInt
      var prod = 1.0
      for i <- 1 to n do prod *= x - i
      return logGamma1p(x - (n + 1)) + math.log(prod)
    else
      val sum = lanczos(x)
      val tmp = x + LANCZOS_G + .5
      ret = ((x + .5) * math.log(tmp)) - tmp + HALF_LOG_2_PI + math.log(sum / x)
    ret

  /**
   * <p> Returns the Lanczos approximation used to compute the gamma function.
   * The Lanczos approximation is related to the Gamma function by the following
   * equation <center> {@code gamma(x) = sqrt(2 * pi) / x * (x + g + 0.5) ^ (x +
   * 0.5) * exp(-x - g - 0.5) * lanczos(x)}, </center> where {@code g} is the
   * Lanczos constant. </p>
   *
   * @param x
   *   Argument.
   * @return
   *   The Lanczos approximation.
   * @see
   *   <a href="http://mathworld.wolfram.com/LanczosApproximation.html">Lanczos
   *   Approximation</a> equations (1) through (5), and Paul Godfrey's <a
   *   href="http://my.fit.edu/~gabdo/gamma.txt">Note on the computation of the
   *   convergent Lanczos complex Gamma approximation</a>
   * @since 3.1
   */
  def lanczos(x: Double): Double =
    var sum = 0.0
    for i <- LANCZOS.length - 1 until 0 by -1 do sum += LANCZOS(i) / (x + i)
    sum + LANCZOS(0)

  /**
   * Returns the value of 1 / &Gamma;(1 + x) - 1 for -0&#46;5 &le; x &le;
   * 1&#46;5. This implementation is based on the double precision
   * implementation in the <em>NSWC Library of Mathematics Subroutines</em>,
   * {@code DGAM1}.
   *
   * @param x
   *   Argument.
   * @return
   *   The value of {@code 1.0 / Gamma(1.0 + x) - 1.0}.
   * @throws NumberIsTooSmallException
   *   if {@code x < -0.5}
   * @throws NumberIsTooLargeException
   *   if {@code x > 1.5}
   * @since 3.1
   */
  def invGamma1pm1(x: Double): Double =
    if x < -0.5 then throw new Exception(s"$x is smaller than -0.5")
    if x > 1.5 then throw new Exception(s"$x is greater than 1.5")
    var ret = .0
    val t =
      if x <= 0.5 then x
      else (x - 0.5) - 0.5
    if t < 0.0 then
      val a = INV_GAMMA1P_M1_A0 + t * INV_GAMMA1P_M1_A1
      var b = INV_GAMMA1P_M1_B8
      b = INV_GAMMA1P_M1_B7 + t * b
      b = INV_GAMMA1P_M1_B6 + t * b
      b = INV_GAMMA1P_M1_B5 + t * b
      b = INV_GAMMA1P_M1_B4 + t * b
      b = INV_GAMMA1P_M1_B3 + t * b
      b = INV_GAMMA1P_M1_B2 + t * b
      b = INV_GAMMA1P_M1_B1 + t * b
      b = 1.0 + t * b
      var c = INV_GAMMA1P_M1_C13 + t * (a / b)
      c = INV_GAMMA1P_M1_C12 + t * c
      c = INV_GAMMA1P_M1_C11 + t * c
      c = INV_GAMMA1P_M1_C10 + t * c
      c = INV_GAMMA1P_M1_C9 + t * c
      c = INV_GAMMA1P_M1_C8 + t * c
      c = INV_GAMMA1P_M1_C7 + t * c
      c = INV_GAMMA1P_M1_C6 + t * c
      c = INV_GAMMA1P_M1_C5 + t * c
      c = INV_GAMMA1P_M1_C4 + t * c
      c = INV_GAMMA1P_M1_C3 + t * c
      c = INV_GAMMA1P_M1_C2 + t * c
      c = INV_GAMMA1P_M1_C1 + t * c
      c = INV_GAMMA1P_M1_C + t * c
      if x > 0.5 then ret = t * c / x
      else ret = x * ((c + 0.5) + 0.5)
    else
      var p = INV_GAMMA1P_M1_P6
      p = INV_GAMMA1P_M1_P5 + t * p
      p = INV_GAMMA1P_M1_P4 + t * p
      p = INV_GAMMA1P_M1_P3 + t * p
      p = INV_GAMMA1P_M1_P2 + t * p
      p = INV_GAMMA1P_M1_P1 + t * p
      p = INV_GAMMA1P_M1_P0 + t * p
      var q = INV_GAMMA1P_M1_Q4
      q = INV_GAMMA1P_M1_Q3 + t * q
      q = INV_GAMMA1P_M1_Q2 + t * q
      q = INV_GAMMA1P_M1_Q1 + t * q
      q = 1.0 + t * q
      var c = INV_GAMMA1P_M1_C13 + (p / q) * t
      c = INV_GAMMA1P_M1_C12 + t * c
      c = INV_GAMMA1P_M1_C11 + t * c
      c = INV_GAMMA1P_M1_C10 + t * c
      c = INV_GAMMA1P_M1_C9 + t * c
      c = INV_GAMMA1P_M1_C8 + t * c
      c = INV_GAMMA1P_M1_C7 + t * c
      c = INV_GAMMA1P_M1_C6 + t * c
      c = INV_GAMMA1P_M1_C5 + t * c
      c = INV_GAMMA1P_M1_C4 + t * c
      c = INV_GAMMA1P_M1_C3 + t * c
      c = INV_GAMMA1P_M1_C2 + t * c
      c = INV_GAMMA1P_M1_C1 + t * c
      c = INV_GAMMA1P_M1_C0 + t * c
      if x > 0.5 then ret = (t / x) * ((c - 0.5) - 0.5)
      else ret = x * c
    ret

  /**
   * Returns the value of log &Gamma;(1 + x) for -0&#46;5 &le; x &le; 1&#46;5.
   * This implementation is based on the double precision implementation in the
   * <em>NSWC Library of Mathematics Subroutines</em>, {@code DGMLN1}.
   *
   * @param x
   *   Argument.
   * @return
   *   The value of {@code log(Gamma(1 + x))}.
   * @throws NumberIsTooSmallException
   *   if {@code x < -0.5}.
   * @throws NumberIsTooLargeException
   *   if {@code x > 1.5}.
   * @since 3.1
   */

  def logGamma1p(x: Double): Double =
    if x < -0.5 then
      throw new Exception(s"Number $x is too small (smaller than -0.5)")
    if x > 1.5 then
      throw new Exception(s"Number $x is too large (larger than 1.5)")
    -math.log1p(invGamma1pm1(x))

  /**
   * Returns the value of Γ(x). Based on the <em>NSWC Library of Mathematics
   * Subroutines</em> double precision implementation, {@code DGAMMA}.
   *
   * @param x
   *   Argument.
   * @return
   *   the value of {@code Gamma(x)}.
   * @since 3.1
   */
  def gamma(x: Double): Double =
    if (x == math.rint(x)) && (x <= 0.0) then return Double.NaN
    var ret = .0
    val absX = math.abs(x)
    if absX <= 20.0 then
      if x >= 1.0 then
        /*
         * From the recurrence relation
         * Gamma(x) = (x - 1) * ... * (x - n) * Gamma(x - n),
         * then
         * Gamma(t) = 1 / [1 + invGamma1pm1(t - 1)],
         * where t = x - n. This means that t must satisfy
         * -0.5 <= t - 1 <= 1.5.
         */
        var prod = 1.0
        var t = x
        while t > 2.5 do
          t -= 1.0
          prod *= t
        ret = prod / (1.0 + invGamma1pm1(t - 1.0))
      else
        /*
         * From the recurrence relation
         * Gamma(x) = Gamma(x + n + 1) / [x * (x + 1) * ... * (x + n)]
         * then
         * Gamma(x + n + 1) = 1 / [1 + invGamma1pm1(x + n)],
         * which requires -0.5 <= x + n <= 1.5.
         */
        var prod = x
        var t = x
        while t < -0.5 do
          t += 1.0
          prod *= t
        ret = 1.0 / (prod * (1.0 + invGamma1pm1(t)))
    else
      val y = absX + LANCZOS_G + 0.5
      val gammaAbs =
        SQRT_TWO_PI / absX * math.pow(y, absX + 0.5) * math.exp(-y) *
          lanczos(absX)
      if x > 0.0 then ret = gammaAbs
      else
        /*
         * From the reflection formula
         * Gamma(x) * Gamma(1 - x) * sin(pi * x) = pi,
         * and the recurrence relation
         * Gamma(1 - x) = -x * Gamma(-x),
         * it is found
         * Gamma(x) = -pi / [x * sin(pi * x) * Gamma(-x)].
         */
        ret = -math.Pi / (x * math.sin(math.Pi * x) * gammaAbs)
    ret
