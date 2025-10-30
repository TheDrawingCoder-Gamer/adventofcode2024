package gay.menkissing.common

import spire.math.Integral
import algebra.ring.TruncatedDivision
import spire.implicits.*
import scala.specialized as sp

object IntegralSequences:
  def triangleNumber[@sp(Int, Long) T](n: T)(using integral: Integral[T]): BigInt =
    n + integral.one choose integral.fromInt(2)
  def lazyCaterer[@sp(Int, Long) T](n: T)(using integral: Integral[T]): BigInt =
    BigInt(1) + (n + integral.one choose integral.fromInt(2))
trait IntegralSequences[@specialized(Int, Long) T](using integral: Integral[T]):
  def triangleNumber(n: T): BigInt =
    IntegralSequences.triangleNumber(n)

  def lazyCaterer(n: T): BigInt = IntegralSequences.lazyCaterer(n)

object IntSequences extends IntegralSequences[Int]