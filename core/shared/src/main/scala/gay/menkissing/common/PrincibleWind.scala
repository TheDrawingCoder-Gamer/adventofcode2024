package gay.menkissing.common

import spire.algebra.*
import spire.math.*
import spire.implicits.IntAlgebra

enum PrincibleWind2D {
  case UpLeft, Up, UpRight, Right, DownRight, Down, DownLeft, Left

  def genericDigitalDir[@specialized(Specializable.Bits32AndUp) A](using integral: Integral[A]): Vec2[A] =
    lazy val neg1 = integral.fromInt(-1)
    inline def pos1 = integral.one
    inline def zero = integral.zero
    this match
      case UpLeft => Vec2(neg1, neg1)
      case Up => Vec2(zero, neg1)
      case UpRight => Vec2(pos1, neg1)
      case Right => Vec2(pos1, zero)
      case DownRight => Vec2(pos1, pos1)
      case Down => Vec2(zero, pos1)
      case DownLeft => Vec2(neg1, pos1)
      case Left => Vec2(neg1, zero)
    

  def digitalDir: Vec2[Int] = genericDigitalDir[Int]
}