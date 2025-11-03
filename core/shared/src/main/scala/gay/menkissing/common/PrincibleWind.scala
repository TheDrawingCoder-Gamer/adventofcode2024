package gay.menkissing.common

import cats.implicits.*
import cats.*
import algebra.ring.*
import algebra.instances.all.*

enum PrincibleWind2D:
  case UpLeft, Up, UpRight, Right, DownRight, Down, DownLeft, Left

  def genericDigitalDir[A](using ring: Ring[A]): Vec2[A] =
    lazy val neg1 = ring.fromInt(-1)
    inline def pos1 = ring.one
    inline def zero = ring.zero
    this match
      case UpLeft    => Vec2(neg1, neg1)
      case Up        => Vec2(zero, neg1)
      case UpRight   => Vec2(pos1, neg1)
      case Right     => Vec2(pos1, zero)
      case DownRight => Vec2(pos1, pos1)
      case Down      => Vec2(zero, pos1)
      case DownLeft  => Vec2(neg1, pos1)
      case Left      => Vec2(neg1, zero)

  def digitalDir: Vec2[Int] = genericDigitalDir[Int]
