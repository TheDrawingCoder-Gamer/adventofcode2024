package gay.menkissing.bench.util

/**
 * Provides a generic means to evaluate continued fractions. Subclasses simply
 * provided the a and b coefficients to evaluate the continued fraction.
 *
 * <p> References: <ul> <li><a
 * href="http://mathworld.wolfram.com/ContinuedFraction.html"> Continued
 * Fraction</a></li> </ul> </p>
 */
object ContinuedFraction:
  /** Maximum allowed numerical error. */
  val DEFAULT_EPSILON = 10e-9

abstract class ContinuedFraction protected

/**
 * Default constructor.
 */
:
  /**
   * Access the n-th a coefficient of the continued fraction. Since a can be a
   * function of the evaluation point, x, that is passed in as well.
   *
   * @param n
   *   the coefficient index to retrieve.
   * @param x
   *   the evaluation point.
   * @return
   *   the n-th a coefficient.
   */
  def getA(n: Int, x: Double): Double

  /**
   * Access the n-th b coefficient of the continued fraction. Since b can be a
   * function of the evaluation point, x, that is passed in as well.
   *
   * @param n
   *   the coefficient index to retrieve.
   * @param x
   *   the evaluation point.
   * @return
   *   the n-th b coefficient.
   */
  def getB(n: Int, x: Double): Double

  /**
   * Evaluates the continued fraction at the value x. <p> The implementation of
   * this method is based on the modified Lentz algorithm as described on page
   * 18 ff. in: <ul> <li>
   *   I. J. Thompson, A. R. Barnett. "Coulomb and Bessel Functions of Complex
   *      Arguments and Order."
   * <a target="_blank"
   * href="http://www.fresco.org.uk/papers/Thompson-JCP64p490.pdf">
   * http://www.fresco.org.uk/papers/Thompson-JCP64p490.pdf</a> </li> </ul>
   * <b>Note:</b> the implementation uses the terms a<sub>i</sub> and
   * b<sub>i</sub> as defined in <a
   * href="http://mathworld.wolfram.com/ContinuedFraction.html">Continued
   * Fraction @ MathWorld</a>. </p>
   *
   * @param x
   *   the evaluation point.
   * @param epsilon
   *   maximum error allowed.
   * @param maxIterations
   *   maximum number of convergents
   * @return
   *   the value of the continued fraction evaluated at x.
   * @throws ConvergenceException
   *   if the algorithm fails to converge.
   * @throws MaxCountExceededException
   *   if maximal number of iterations is reached
   */
  def evaluate
    (
      x: Double,
      epsilon: Double = ContinuedFraction.DEFAULT_EPSILON,
      maxIterations: Int = Int.MaxValue
    ): Double =
    val small = 1e-50
    var hPrev = getA(0, x)
    // use the value of small as epsilon criteria for zero checks
    if fuzzyEquals(hPrev, 0.0, small) then hPrev = small
    var n = 1
    var dPrev = 0.0
    var cPrev = hPrev
    var hN = hPrev

    def whileLoop(): Unit =
      while n < maxIterations do
        val a = getA(n, x)
        val b = getB(n, x)
        var dN = a + b * dPrev
        if fuzzyEquals(dN, 0.0, small) then dN = small
        var cN = a + b / cPrev
        if fuzzyEquals(cN, 0.0, small) then cN = small
        dN = 1 / dN
        val deltaN = cN * dN
        hN = hPrev * deltaN
        if hN.isInfinite then
          throw new Exception(
            s"Continued Fraction Diverges to +/- infinity for value $x"
          )
        if hN.isNaN then
          throw new Exception(
            s"Continued Fraction diverged to NaN for value $x"
          )
        if math.abs(deltaN - 1.0) < epsilon then return
        dPrev = dN
        cPrev = cN
        hPrev = hN
        n += 1

    whileLoop()
    if n >= maxIterations then
      throw new Exception(
        s"Continued fraction convergents failed to converge (in less than $maxIterations) for value $x"
      )
    hN
