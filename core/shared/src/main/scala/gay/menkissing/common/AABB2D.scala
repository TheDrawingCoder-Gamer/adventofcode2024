package gay.menkissing.common

import cats.*
import cats.collections.Range

final case class AABB2D[A](xs: Range[A], ys: Range[A]):
  def contains(p: Vec2[A])(using Order[A]): Boolean =
    xs.contains(p.x) && ys.contains(p.y)

  def start: Vec2[A] = Vec2(xs.start, ys.start)
  def stop: Vec2[A] = Vec2(xs.end, ys.end)
