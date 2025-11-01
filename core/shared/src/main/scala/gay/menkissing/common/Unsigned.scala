package gay.menkissing.common

object Unsigned:
  opaque type UInt = Int

  object UInt:
    inline def fromDouble(d: Double): UInt =
      // convert to long so that if d > Int.MaxValue result isn't Int.MaxValue
      d.toLong.toInt

  extension (d: Double) inline def toUInt: UInt = UInt.fromDouble(d)

  extension (self: UInt)
    inline def +(that: UInt): UInt = self + that
    inline def -(that: UInt): UInt = self - that
    inline def *(that: UInt): UInt = self * that
    inline def >>(that: UInt): UInt = self >>> that
    inline def <<(that: UInt): UInt = self << that
    inline def |(that: UInt): UInt = self | that
    inline def &(that: UInt): UInt = self & that
    inline def ^(that: UInt): UInt = self ^ that
