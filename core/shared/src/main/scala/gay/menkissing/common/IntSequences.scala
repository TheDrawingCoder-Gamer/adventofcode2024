package gay.menkissing.common

import algebra.ring.*
import algebra.instances.all.*
import cats.implicits.*

object IntegralSequences:
  def triangleNumber[T](n: T)(using ring: Ring[T], as: AsNumber[T]): BigInt =
    n + ring.one choose ring.fromInt(2)
  def lazyCaterer[T](n: T)(using ring: Ring[T], as: AsNumber[T]): BigInt =
    BigInt(1) + (n + ring.one choose ring.fromInt(2))
trait IntegralSequences[T](using ring: Ring[T], as: AsNumber[T]):
  def triangleNumber(n: T): BigInt = IntegralSequences.triangleNumber(n)

  def lazyCaterer(n: T): BigInt = IntegralSequences.lazyCaterer(n)

object IntSequences extends IntegralSequences[Int]
