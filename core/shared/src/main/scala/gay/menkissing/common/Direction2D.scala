package gay.menkissing.common

import cats.*
import algebras.given

enum Direction2D:
  case Up, Down, Left, Right

  // Grid based axis direction
  // Like scratch for you NERDS
  def clockwise =
    this match
      case Direction2D.Up    => Direction2D.Right
      case Direction2D.Down  => Direction2D.Left
      case Direction2D.Left  => Direction2D.Up
      case Direction2D.Right => Direction2D.Down
  def counterClockwise =
    this match
      case Direction2D.Up    => Direction2D.Left
      case Direction2D.Down  => Direction2D.Right
      case Direction2D.Left  => Direction2D.Down
      case Direction2D.Right => Direction2D.Up

  def rotate(n: Int): Direction2D =
    if n < 0 then Iterator.iterate(this)(_.counterClockwise).drop(-n).next()
    else if n == 0 then this
    else Iterator.iterate(this)(_.clockwise).drop(n).next()

  def reverse: Direction2D = clockwise.clockwise

  def flipHorizontal: Direction2D =
    this match
      case Direction2D.Up    => Direction2D.Up
      case Direction2D.Down  => Direction2D.Down
      case Direction2D.Left  => Direction2D.Right
      case Direction2D.Right => Direction2D.Left

object Direction2D:
  given eqDirection2D: Eq[Direction2D] = Eq.fromUniversalEquals
  given showDirection2D: Show[Direction2D] =
    it =>
      it match
        case Down  => "Down"
        case Left  => "Left"
        case Right => "Right"
        case Up    => "Up"

  given isDirectionN2D: IsDirectionN[Direction2D] with
    type Axis = Axis2D
    given axisNAxis: AxisN[Axis2D] = summon[AxisN[Axis2D]]

    type Vec[A] = Vec2[A]
    given vecNVec: VecN[Vec] = summon
    def fromDirectionN(d: DirectionN): Direction2D =
      (d.axis, d.direction).runtimeChecked match
        case (0, AxisDirection.Negative) => Direction2D.Left
        case (0, AxisDirection.Positive) => Direction2D.Right
        case (1, AxisDirection.Negative) => Direction2D.Up
        case (1, AxisDirection.Positive) => Direction2D.Down

    extension (self: Direction2D)
      def axisId: Int =
        self match
          case Direction2D.Left | Direction2D.Right => 0
          case _                                    => 1
      def axisDirection: AxisDirection =
        self match
          case Direction2D.Left | Direction2D.Up => AxisDirection.Negative
          case _                                 => AxisDirection.Positive
