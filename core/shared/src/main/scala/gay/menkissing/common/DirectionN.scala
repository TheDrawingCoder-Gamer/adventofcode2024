package gay.menkissing.common

import spire.algebra.*
import spire.implicits.*

case class DirectionN(axis: Int, direction: AxisDirection)

trait IsDirectionN[A]:
  type Axis
  given axisNAxis: AxisN[Axis]
  type Vec[_]
  given vecNVec: VecN[Vec]

  def fromDirectionN(d: DirectionN): A
  extension (self: A)
    def toDirectionN: DirectionN =
      DirectionN(self.axisId, self.axisDirection)

    def reverse: A =
      val d = self.toDirectionN
      fromDirectionN(d.copy(direction = !d.direction))

    final def digitalDir[@specialized(Specializable.Bits32AndUp) F](using ring: Ring[F]): Vec[F] =
      if self.axisDirection == AxisDirection.Positive then
        vecNVec.axis(self.axisId)
      else
        -vecNVec.axis(self.axisId)
      

    def axisId: Int
    def axis: Axis =
      axisNAxis.fromIndex(self.axisId)
    def axisDirection: AxisDirection