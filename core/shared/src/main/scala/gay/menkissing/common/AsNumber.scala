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
  given AsNumber[Int] with
    extension (self: Int)
      def toBigInt = BigInt(self)
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self.toDouble
      def toFloat = self.toFloat
      def toInt = self
      def toLong = self.toLong
      def toShort = self.toShort
  given AsNumber[Long] with
    extension (self: Long)
      def toBigInt = BigInt(self)
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self.toDouble
      def toFloat = self.toFloat
      def toInt = self.toInt
      def toLong = self
      def toShort = self.toShort
  given AsNumber[Double] with
    extension (self: Double)
      def toBigInt = BigInt(self.toLong)
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self
      def toFloat = self.toFloat
      def toInt = self.toInt
      def toLong = self.toLong
      def toShort = self.toShort
  given AsNumber[Float] with
    extension (self: Float)
      def toBigInt = BigInt(self.toLong)
      def toBigDecimal = BigDecimal(self)
      def toByte = self.toByte
      def toDouble = self.toDouble
      def toFloat = self
      def toInt = self.toInt
      def toLong = self.toLong
      def toShort = self.toShort
