package gay.menkissing.common

import algebra.ring.*
import algebra.instances.*
import cats.*
import algebras.*

trait IsIntegral[A] extends IsReal[A]:
  extension (self: A)
    def ceil: A = self
    def floor: A = self
    def round: A = self
    def isWhole: Boolean = true
    def toBigInt: BigInt

trait IsReal[A] extends Order[A], Signed[A]:
  extension (self: A)
    def ceil: A
    def floor: A
    def round: A
    def isWhole: Boolean
    def toDouble: Double

trait SIntegral[A] extends IsReal[A], EuclideanRing[A], AsNumber[A]:
  def compare(x: A, y: A): Int = order.compare(x, y)

object SIntegral:
  class IntSIntegral
      extends LooseIntAlgebra,
        AsNumber.IntAsNumber,
        SIntegral[Int],
        IsIntegral[Int]

  class LongSIntegral
      extends LooseLongAlgebra,
        AsNumber.LongAsNumber,
        SIntegral[Long],
        IsIntegral[Long]

  given IntSIntegral()

  given LongSIntegral()

  class BigIntSIntegral
      extends BigIntTruncatedDivison,
        AsNumber.BigIntAsNumber,
        SIntegral[BigInt],
        IsIntegral[BigInt]

  given BigIntSIntegral()
