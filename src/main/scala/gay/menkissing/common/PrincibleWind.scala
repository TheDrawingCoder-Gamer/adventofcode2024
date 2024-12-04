package gay.menkissing.common

enum PrincibleWind2D {
  case UpLeft, Up, UpRight, Right, DownRight, Down, DownLeft, Left

  def digitalDir: Vec2i = this match {
    case UpLeft => Vec2i(-1, -1)
    case Up => Vec2i(0, -1)
    case UpRight => Vec2i(1, -1)
    case Right => Vec2i(1, 0)
    case DownRight => Vec2i(1, 1)
    case Down => Vec2i(0, 1)
    case DownLeft => Vec2i(-1, 1)
    case Left => Vec2i(-1, 0)
  }
}