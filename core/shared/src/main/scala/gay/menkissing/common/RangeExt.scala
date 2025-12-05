package gay.menkissing.common

import cats.collections.Range
import algebra.ring.Ring
import cats.Order

extension [A](range: Range[A])
  def size(using ring: Ring[A]): A =
    ring.minus(range.end, range.start) + ring.fromInt(1)

extension [A](a: A)
  infix def safeToIncl(b: A)(using ord: Order[A]): Range[A] =
    Range(ord.min(a, b), ord.max(a, b))
