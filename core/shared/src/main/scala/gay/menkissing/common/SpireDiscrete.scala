package gay.menkissing.common

import spire.math.Integral
import cats.collections.Discrete
import spire.syntax.integral.*

given spireIntegralDiscrete[@specialized(Specializable.Integral) I](using I: Integral[I]): Discrete[I] =
  new Discrete[I]:
    override def pred(x: I): I = x - I.one
    override def succ(x: I): I = x + I.one
