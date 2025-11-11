package gay.menkissing.common

import algebra.ring.*

extension [A](self: A)(using asg: AdditiveSemigroup[A])
  infix def +(that: A): A = asg.plus(self, that)
extension [A](self: A)(using ag: AdditiveGroup[A])
  def unary_- : A = ag.negate(self)
  infix def -(that: A): A = ag.minus(self, that)

extension [A](self: A)(using msg: MultiplicativeSemigroup[A])
  infix def *(that: A): A = msg.times(self, that)
extension [A](self: A)(using mg: MultiplicativeGroup[A])
  infix def /(that: A): A = mg.div(self, that)
