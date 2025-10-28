package gay.menkissing.common

import cats.*

enum Direction2D {
  case Up, Down, Left, Right

  def digitalDir: Vec2i =
    this match
      case Direction2D.Up => Vec2i(0,-1)
      case Direction2D.Down => Vec2i(0,1)
      case Direction2D.Left => Vec2i(-1,0)
      case Direction2D.Right => Vec2i(1,0) 
  
  def axis = 
    this match
      case Direction2D.Up => Axis2D.Y 
      case Direction2D.Down => Axis2D.Y 
      case Direction2D.Left => Axis2D.X 
      case Direction2D.Right => Axis2D.X
  // Grid based axis direction
  // Like scratch for you NERDS
  def axisDirection = 
    this match
      case Direction2D.Up => Axis2D.Direction.Negative
      case Direction2D.Down => Axis2D.Direction.Positive
      case Direction2D.Left => Axis2D.Direction.Negative
      case Direction2D.Right => Axis2D.Direction.Positive
  def clockwise = 
    this match
      case Direction2D.Up => Direction2D.Right 
      case Direction2D.Down => Direction2D.Left 
      case Direction2D.Left => Direction2D.Up 
      case Direction2D.Right => Direction2D.Down 
  def counterClockwise = 
    this match
      case Direction2D.Up => Direction2D.Left
      case Direction2D.Down => Direction2D.Right
      case Direction2D.Left => Direction2D.Down
      case Direction2D.Right => Direction2D.Up

  def rotate(n: Int): Direction2D =
    if n < 0 then
      Iterator.iterate(this)(_.counterClockwise).drop(-n).next()
    else if n == 0 then
      this
    else
      Iterator.iterate(this)(_.clockwise).drop(n).next()
      
  def reverse: Direction2D = clockwise.clockwise
  
  def flipHorizontal: Direction2D =
    this match
      case Direction2D.Up => Direction2D.Up
      case Direction2D.Down => Direction2D.Down
      case Direction2D.Left => Direction2D.Right
      case Direction2D.Right => Direction2D.Left
}

object Direction2D:
  given eqDirection2D: Eq[Direction2D] = Eq.fromUniversalEquals
  given showDirection2D: Show[Direction2D] = it => 
    it match
      case Down => "Down"
      case Left => "Left"
      case Right => "Right"
      case Up => "Up"