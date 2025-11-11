package gay.menkissing.common

import cats.*

final case class AABB2D[A](xs: Dimension[A], ys: Dimension[A]):
  def contains(p: Vec2[A])(using Order[A]): Boolean =
    xs.contains(p.x) && ys.contains(p.y)

  def start: Vec2[A] = Vec2(xs.min, ys.min)
  def stop: Vec2[A] = Vec2(xs.max, ys.max)
