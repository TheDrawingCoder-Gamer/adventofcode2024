package gay.menkissing.common

import scala.deriving.*
import annotation.nowarn

trait AxisN[A]:
  def fromIndex(n: Int): A
  extension (self: A) def index: Int

object AxisN:
  @nowarn
  inline def derived[T](using m: Mirror.SumOf[T]): AxisN[T] =
    val arr = Macros.summonSingletons[T].toVector
    new AxisN[T]:
      def fromIndex(n: Int): T = arr(n)
      extension (self: T) def index: Int = m.ordinal(self)
