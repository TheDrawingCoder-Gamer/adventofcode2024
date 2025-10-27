package gay.menkissing.common

import scala.annotation.targetName
import scala.math.Ordering.Implicits.infixOrderingOps
import cats.implicits.*
import spire.implicits.LongAlgebra

type Vec2l = Vec2[Long]

object Vec2l:
  def apply(x: Long, y: Long): Vec2l = new Vec2(x, y)

