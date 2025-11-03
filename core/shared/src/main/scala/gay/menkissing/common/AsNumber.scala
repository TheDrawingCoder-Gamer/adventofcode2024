package gay.menkissing.common

trait AsNumber[A]:
  extension (self: A)
    def toBigInt: BigInt
    def toBigDecimal: BigDecimal
    def toByte: Byte
    def toDouble: Double
    def toFloat: Float
    def toInt: Int
    def toLong: Long
    def toShort: Short

object AsNumber:
  trait IntAsNumber extends AsNumber[Int]:
    extension (self: Int)
      def toBigInt = BigInt(self)
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self.toDouble
      def toFloat = self.toFloat
      def toInt = self
      def toLong = self.toLong
      def toShort = self.toShort
  given IntAsNumber()
  trait LongAsNumber extends AsNumber[Long]:
    extension (self: Long)
      def toBigInt = BigInt(self)
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self.toDouble
      def toFloat = self.toFloat
      def toInt = self.toInt
      def toLong = self
      def toShort = self.toShort
  given LongAsNumber()
  trait DoubleAsNumber extends AsNumber[Double]:
    extension (self: Double)
      def toBigInt = BigInt(self.toLong)
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self
      def toFloat = self.toFloat
      def toInt = self.toInt
      def toLong = self.toLong
      def toShort = self.toShort
  given DoubleAsNumber()
  trait FloatAsNumber extends AsNumber[Float]:
    extension (self: Float)
      def toBigInt = BigInt(self.toLong)
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self.toDouble
      def toFloat = self
      def toInt = self.toInt
      def toLong = self.toLong
      def toShort = self.toShort
  given FloatAsNumber()

  trait BigIntAsNumber extends AsNumber[BigInt]:
    extension (self: BigInt)
      def toBigInt = self
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self.toDouble
      def toFloat = self.toFloat
      def toInt = self.toInt
      def toLong = self.toLong
      def toShort = self.toShort
  given BigIntAsNumber()
  trait BigDecimalAsNumber extends AsNumber[BigDecimal]:
    extension (self: BigDecimal)
      def toBigInt = self.toBigInt
      def toBigDecimal = self
      def toByte = self.toByte
      def toDouble = self.toDouble
      def toFloat = self.toFloat
      def toInt = self.toInt
      def toLong = self.toLong
      def toShort = self.toShort
  given BigDecimalAsNumber()
