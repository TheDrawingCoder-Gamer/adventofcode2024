package gay.menkissing.common

import scala.annotation.tailrec
import spire.implicits.IntAlgebra
import spire.algebra.*
import cats.*
import cats.syntax.all.*
import spire.implicits.{partialOrderOps => _, *}
import spire.math.ConvertableFrom
import spire.math.ConvertableTo

object Sys3D:
  object Vec3i:
    def of(str: String): Vec3[Int] =
      val parts = str.split(',')
      Vec3(parts(0).toInt, parts(1).toInt, parts(2).toInt)

  object Vec3:

    given vecNVec3: VecN[Vec3] with
      def dimensions: Int = 3


      def axis[A](i: Int)(using ring: Ring[A]): Vec3[A] =
        i match
          case 0 => Vec3(ring.one, ring.zero, ring.zero)
          case 1 => Vec3(ring.zero, ring.one, ring.zero)
          case 2 => Vec3(ring.zero, ring.zero, ring.one)
          case _ => whatTheScallop.!

      extension [A](self: Vec3[A]) 
        def axes: List[A] =  List(self.x, self.y, self.z)
        def zip(that: Vec3[A])(f: (A, A) => A): Vec3[A] =
          Vec3(f(self.x, that.x), f(self.y, that.y), f(self.z, that.z))
        def map(f: A => A): Vec3[A] =
          Vec3(f(self.x), f(self.y), f(self.z))

        def coord(i: Int): A =
          i match
            case 0 => self.x
            case 1 => self.y
            case 2 => self.z
            case _ => whatTheScallop.!
        
        def withCoord(i: Int, v: A): Vec3[A] =
          i match
            case 0 => self.copy(x = v)
            case 1 => self.copy(y = v)
            case 2 => self.copy(z = v)
  case class Vec3[A](x: A, y: A, z: A):


    def offset(dir: Direction3D, n: A)(using add: AdditiveGroup[A]): Vec3[A] =
      val v = 
        dir.axisDirection match
          case AxisDirection.Positive => n
          case AxisDirection.Negative => add.negate(n)
      dir.axis match
        case Axis3D.X => Vec3(x + v, y, z)
        case Axis3D.Y => Vec3(x, y + v, z)
        case Axis3D.Z => Vec3(x, y, z + v)
      
    def offset(dir: Direction3D)(using add: Ring[A]): Vec3[A] =
      offset(dir, add.one)



    // facing default North
    def face(dir: Direction3D)(using add: AdditiveGroup[A]): Vec3[A] =
      dir match
        case Direction3D.North => this
        case Direction3D.East => rotate(Axis3D.Y, 1)
        case Direction3D.South => rotate(Axis3D.Y, 2)
        case Direction3D.West => rotate(Axis3D.Y, 3)
        case Direction3D.Up => rotate(Axis3D.X, 1)
        case Direction3D.Down => rotate(Axis3D.X, -1)

    def orient(rotation: Rotation)(using add: AdditiveGroup[A]): Vec3[A] =
      rotation match
        case Rotation.Rot0 => this
        case Rotation.Rot90 => rotate(Axis3D.Z, 1)
        case Rotation.Rot180 => rotate(Axis3D.Z, 2)
        case Rotation.Rot270 => rotate(Axis3D.Z, 3)

    @tailrec
    final def rotate(axis: Axis3D, times: Int)(using add: AdditiveGroup[A]): Vec3[A] =
      val gTimes =
        val temp = times % 4
        if temp < 0 then
          (4 + temp) % 4
        else
          temp
      if gTimes == 0 then
        this
      else
        axis match
          case Axis3D.X =>
            // y to z, z to -y
            Vec3(x, -z, y).rotate(axis, times - 1)
          case Axis3D.Y =>
            // z to x, x to -z
            Vec3(z, y, -x).rotate(axis, times - 1)
          case Axis3D.Z =>
            // y to x, x to -y
            Vec3(y, -x, z).rotate(axis, times - 1)

  // it's like minecraft all over again :frown:
  enum Direction3D:
    case North, East, South, West, Up, Down


    def next(axis: Axis3D): Direction3D =
      if this.axis == axis then
        this
      else
        axis match
          case Axis3D.X =>
            this match
              case North => Up
              case Up => South
              case South => Down
              case Down => North
              case _ => this
          case Axis3D.Y =>
            this match
              case North => East
              case East => South
              case South => West
              case West => East
              case _ => this
          case Axis3D.Z =>
            this match
              case Up => East
              case East => Down
              case Down => West
              case West => Up
              case _ => this
      

    @tailrec
    final def rotate(axis: Axis3D, clockwise: Boolean, times: Int): Direction3D =
      val times2 = times % 4
      // magic
      val gTimes = if (times2 < 0) 4 + times2 else times2
      val ggTimes = (if (clockwise) gTimes else 4 - gTimes) % 4
      if ggTimes == 0 then this
      else this.next(axis).rotate(axis, true, ggTimes - 1)

  object Direction3D:
    val horizontal: Seq[Direction3D] = Seq(North, East, South, West)
    val zplane: Seq[Direction3D] = Seq(Up, East, Down, West)

    given isDirectionN3D: IsDirectionN[Direction3D] with
      type Axis = Axis3D
      given axisNAxis: AxisN[Axis3D] = summon

      type Vec = Vec3

      given vecNVec: VecN[Vec3] = summon
      def fromDirectionN(d: DirectionN): Direction3D =
        (d.axis, d.direction) match
          case (0, AxisDirection.Negative) => Direction3D.West
          case (0, AxisDirection.Positive) => Direction3D.East
          case (1, AxisDirection.Negative) => Direction3D.Down
          case (1, AxisDirection.Positive) => Direction3D.Up
          case (2, AxisDirection.Negative) => Direction3D.South
          case (2, AxisDirection.Positive) => Direction3D.North
          case _ => whatTheScallop.!
      extension (self: Direction3D)
        def axisId: Int =
          self match
            case Direction3D.West | Direction3D.East => 0
            case Direction3D.Down | Direction3D.Up   => 1
            case Direction3D.South | Direction3D.North => 2
        def axisDirection: AxisDirection =
          self match
            case Direction3D.West | Direction3D.Down | Direction3D.South => AxisDirection.Negative
            case _ => AxisDirection.Positive

  enum Axis3D derives AxisN:
    case X, Y, Z


  enum Rotation:
    case Rot0, Rot90, Rot180, Rot270

  case class Orientation3D(facing: Direction3D, rot: Rotation)

  object Orientation3D:
    val orientations: Seq[Orientation3D] =
      for
        f <- Direction3D.values.toSeq
        r <- Rotation.values
      yield Orientation3D(f, r)
    

  case class AABB3D[A](xs: Dimension[A], ys: Dimension[A], zs: Dimension[A]):
    infix def intersect(that: AABB3D[A])(using ord: Order[A]): Option[AABB3D[A]] =
      for
        xs <- this.xs intersect that.xs
        ys <- this.ys intersect that.ys
        zs <- this.zs intersect that.zs
      yield AABB3D(xs, ys, zs)

    def volume(using rng: Rng[A], cv: ConvertableFrom[A], ct: ConvertableTo[A]): BigInt = 
      cv.toBigInt(xs.length) * cv.toBigInt(ys.length) * cv.toBigInt(zs.length)

    def contains(v: Vec3[A])(using ord: Order[A]): Boolean =
      v.x >= xs.min
      && v.x <= xs.max
      && v.y >= ys.min
      && v.y <= ys.max
      && v.z >= zs.min
      && v.z <= zs.max

    def grow(n: A)(using add: AdditiveGroup[A], ord: Order[A]): AABB3D[A] =
      AABB3D(xs.min - n dimBy xs.max + n, ys.min - n dimBy ys.max + n, zs.min - n dimBy ys.max + n)

    def fitsIn(that: AABB3D[A])(using order: Order[A]): Boolean =
      xs.fitsIn(that.xs) && ys.fitsIn(that.ys) && zs.fitsIn(that.zs)
    
    def start: Vec3[A] =
      Vec3(xs.min, ys.min, zs.min)
    def stop: Vec3[A] =
      Vec3(xs.max, ys.max, zs.max)
    
  object AABB3D:
    given showAABB3D[A](using show: Show[A]): Show[AABB3D[A]] = box =>
      show"x=${box.xs},y=${box.ys},z=${box.zs}"

    def apply[A](start: Vec3[A], stop: Vec3[A])(using order: Order[A]): AABB3D[A] =
      new AABB3D(start.x dimBy stop.x, start.y dimBy stop.y, start.z dimBy stop.z)

    def containingAll[A](vs: Iterable[Vec3[A]])(using order: Order[A]): AABB3D[A] =
      val min = vs.reduce(_ min _)
      val max = vs.reduce(_ max _)
      AABB3D(min, max)


