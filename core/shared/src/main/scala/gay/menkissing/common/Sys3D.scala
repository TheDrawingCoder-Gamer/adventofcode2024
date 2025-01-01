package gay.menkissing.common

import scala.annotation.tailrec

object Sys3D {
  object Vec3i {
    def of(str: String): Vec3i = {
      val parts = str.split(',')
      Vec3i(parts(0).toInt, parts(1).toInt, parts(2).toInt)
    }
  }

  case class Vec3i(x: Int, y: Int, z: Int) {
    def offset(o: Vec3i): Vec3i = {
      val x = this.x + o.x
      val y = this.y + o.y
      val z = this.z + o.z
      Vec3i(x, y, z)
    }

    def +(o: Vec3i): Vec3i = offset(o)

    def -(o: Vec3i): Vec3i = offset(-o)

    def unary_- : Vec3i = {
      val x = -this.x
      val y = -this.y
      val z = -this.z
      Vec3i(x, y, z)
    }

    // facing default North
    def face(dir: Direction3D): Vec3i = {
      dir match
        case Direction3D.North => this
        case Direction3D.East => rotate(Axis3D.Y, 1)
        case Direction3D.South => rotate(Axis3D.Y, 2)
        case Direction3D.West => rotate(Axis3D.Y, 3)
        case Direction3D.Up => rotate(Axis3D.X, 1)
        case Direction3D.Down => rotate(Axis3D.X, -1)
    }

    def orient(rotation: Rotation): Vec3i = {
      rotation match
        case Rotation.Rot0 => this
        case Rotation.Rot90 => rotate(Axis3D.Z, 1)
        case Rotation.Rot180 => rotate(Axis3D.Z, 2)
        case Rotation.Rot270 => rotate(Axis3D.Z, 3)
    }

    @tailrec
    final def rotate(axis: Axis3D, times: Int): Vec3i = {
      val gTimes = {
        val temp = times % 4
        if (temp < 0)
          (4 + temp) % 4
        else
          temp

      }
      if (gTimes == 0)
        this
      else
        axis match {
          case Axis3D.X =>
            // y to z, z to -y
            Vec3i(x, -z, y).rotate(axis, times - 1)
          case Axis3D.Y =>
            // z to x, x to -z
            Vec3i(z, y, -x).rotate(axis, times - 1)
          case Axis3D.Z =>
            // y to x, x to -y
            Vec3i(y, -x, z).rotate(axis, times - 1)
        }
    }

    def manhattanDistance(o: Vec3i): Int = {
      (this.x - o.x).abs + (this.y - o.y).abs + (this.z - o.z).abs
    }

  }

  // it's like minecraft all over again :frown:
  enum Direction3D {
    case North, East, South, West, Up, Down
    lazy val axis: Axis3D = {
      this match {
        case North => Axis3D.Z
        case South => Axis3D.Z
        case East => Axis3D.X
        case West => Axis3D.X
        case Up => Axis3D.Y
        case Down => Axis3D.Y
      }
    }
    lazy val axisDirection: AxisDirection = {
      this match {
        case North => AxisDirection.Positive
        case East => AxisDirection.Positive
        case Up => AxisDirection.Positive
        case South => AxisDirection.Negative
        case West => AxisDirection.Negative
        case Down => AxisDirection.Negative
      }
    }

    def next(axis: Axis3D): Direction3D = {
      if (this.axis == axis)
        this
      else {
        axis match {
          case Axis3D.X =>
            this match {
              case North => Up
              case Up => South
              case South => Down
              case Down => North
              case _ => this
            }
          case Axis3D.Y =>
            this match {
              case North => East
              case East => South
              case South => West
              case West => East
              case _ => this
            }
          case Axis3D.Z =>
            this match {
              case Up => East
              case East => Down
              case Down => West
              case West => Up
              case _ => this
            }
        }
      }
    }

    @tailrec
    final def rotate(axis: Axis3D, clockwise: Boolean, times: Int): Direction3D = {
      val times2 = times % 4
      // magic
      val gTimes = if (times2 < 0) 4 + times2 else times2
      val ggTimes = (if (clockwise) gTimes else 4 - gTimes) % 4
      if (ggTimes == 0) this
      else this.next(axis).rotate(axis, true, ggTimes - 1)
    }
  }

  object Direction3D {
    val horizontal: Seq[Direction3D] = Seq(North, East, South, West)
    val zplane: Seq[Direction3D] = Seq(Up, East, Down, West)
  }

  enum Axis3D {
    case X, Y, Z
  }

  enum AxisDirection {
    case Positive, Negative
  }

  enum Rotation {
    case Rot0, Rot90, Rot180, Rot270
  }

  case class Orientation3D(facing: Direction3D, rot: Rotation)

  object Orientation3D {
    val orientations: Seq[Orientation3D] = {
      (for {
        f <- Direction3D.values
        r <- Rotation.values
      } yield Orientation3D(f, r)).toSeq
    }
  }
}
