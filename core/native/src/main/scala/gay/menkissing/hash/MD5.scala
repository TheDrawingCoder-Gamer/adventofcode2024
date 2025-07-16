package gay.menkissing.hash

import scala.scalanative.unsafe.*

@link("crypto")
@extern
object openssl:
  def MD5(in: Ptr[CUnsignedChar], n: CUnsignedLong, result: Ptr[CUnsignedChar]): Ptr[CUnsignedChar] = extern

object MD5:
  def calc(in: Array[Byte]): Array[Byte] =
    // UByte == Byte on bit level... : )
    val input = in.at(0).asInstanceOf[Ptr[CUnsignedChar]]
    val output = Array.fill[Byte](16)(0.toByte)
    openssl.MD5(input, in.length.toCSSize.toUSize, output.at(0).asInstanceOf[Ptr[CUnsignedChar]])
    output



