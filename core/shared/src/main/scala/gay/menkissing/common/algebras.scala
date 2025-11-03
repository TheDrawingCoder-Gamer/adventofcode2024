package gay.menkissing.common

import algebra.instances.*
import algebra.ring.*
import cats.*
import scala.math.Integral.Implicits.infixIntegralOps

object algebras:
  // int algebra that pretends that overflow doesn't exist
  // so its unsound when overflow does happen, but thats kind of the deal
  // with most operations on int
  class LooseIntAlgebra extends IntAlgebra, TruncatedDivision.forCommutativeRing[Int], EuclideanRing[Int]:
    final def emod(a: Int, b: Int): Int =
      val mod = a % b
      if mod.signum >= 0 then mod
      else if b > 0 then mod + b
      else mod - b
    final def equot(a: Int, b: Int): Int =
      val (qt, rt) = a /% b
      if rt.signum >= 0 then qt
      else if b > 0 then qt - 1
      else qt + 1
    final override def equotmod(a: Int, b: Int): (Int, Int) =
      val (qt, rt) = a /% b
      if rt.signum >= 0 then (qt, rt)
      else if b > 0 then (qt - 1, rt + b)
      else (qt + 1, rt - b)
    final def euclideanFunction(a: Int): BigInt = BigInt(a).abs

    final def tquot(x: Int, y: Int): Int = x / y
    final def tmod(x: Int, y: Int): Int = x % y

    override def order: Order[Int] = cats.kernel.instances.int.catsKernelStdOrderForInt
  
  given LooseIntAlgebra()

  class LooseLongAlgebra extends LongAlgebra, TruncatedDivision.forCommutativeRing[Long], EuclideanRing[Long]:
    final def emod(a: Long, b: Long): Long =
      val mod = a % b
      if mod.signum >= 0 then mod
      else if b > 0 then mod + b
      else mod - b
    final def equot(a: Long, b: Long): Long =
      val (qt, rt) = a /% b
      if rt.signum >= 0 then qt
      else if b > 0 then qt - 1
      else qt + 1
    final override def equotmod(a: Long, b: Long): (Long, Long) =
      val (qt, rt) = a /% b
      if rt.signum >= 0 then (qt, rt)
      else if b > 0 then (qt - 1, rt + b)
      else (qt + 1, rt - b)
    final def euclideanFunction(a: Long): BigInt = BigInt(a).abs

    final def tquot(x: Long, y: Long): Long = x / y
    final def tmod(x: Long, y: Long): Long = x % y

    override def order: Order[Long] = cats.kernel.instances.long.catsKernelStdOrderForLong

  given LooseLongAlgebra()

