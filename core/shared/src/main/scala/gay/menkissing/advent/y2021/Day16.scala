package gay.menkissing.advent
package y2021

import gay.menkissing.common.*
import cats.data.Chain
import cats.implicits.*

object Day16 extends Problem:
  type Input = Array[Byte]
  type Output = Long

  case class Packet(version: Int, kind: PacketKind):
    def deepChildren: Chain[Packet] =
      kind match
        case PacketKind.Operator(_, ls) =>
          Chain.fromSeq(ls).flatMap(it => it.deepChildren.prepend(it))
        case _ => Chain.empty

  enum PacketKind:
    case Literal(v: Long)
    case Operator(kind: Int, subpackets: List[Packet])

    def resolve: Long =
      this match
        case Literal(v)         => v
        case Operator(kind, ps) =>
          val vs = ps.map(_.kind.resolve)
          kind match
            case 0 => vs.sum
            case 1 => vs.product
            case 2 => vs.min
            case 3 => vs.max
            case 5 => if vs(0) > vs(1) then 1L else 0L
            case 6 => if vs(0) < vs(1) then 1L else 0L
            case 7 => if vs(0) == vs(1) then 1L else 0L
            case _ => whatTheScallop.!

  lazy val input = FileIO.getInput(2021, 16)

  def parseNibble(c: Char): Byte = parseHex(c.toString).toByte
  def parseHex(s: String): Int = Integer.parseInt(s, 16)

  inline val debugMode = false

  val debugger = Debuginator(debugMode)

  def padLeft(str: String, to: Int, c: Char): String =
    if str.length < to then (c.toString * (to - str.length)) + str
    else str
  class BitAccess(val bits: Array[Byte]):

    def get(n: Long, nbits: Int): Int =
      require(nbits <= 32)
      val minByte = (n >> 3).toInt
      val maxBit = n + nbits - 1
      val maxByte = (maxBit >> 3).toInt
      val nBytes = maxByte - minByte + 1
      val fullBits = nBytes * 8
      var rawint = 0
      for b <- minByte to maxByte do rawint = ((rawint << 8) | bits(b).toUInt)
      debugger.debug(s"n: $n, nbits: $nbits, min, max: $minByte, $maxByte")
      debugger.debug(s"segment: ${padLeft(rawint.toHexString, 32 / 4, '0')}")
      // println(bytes.map(_.toUInt.toBinaryString).mkString(","))
      // 0xAAABBB00
      val padRight = fullBits - (n & 7).toInt - nbits
      debugger.debug(s"padright: $padRight")

      val r = (rawint >>> padRight) & (0xffffffff >>> (32 - nbits))
      debugger.debug(s"res: $r")
      r

  def parse(in: String): Input =
    if in.trim.length % 2 != 0 then parse(in.trim + "0")
    else in.trim.grouped(2).map(it => parseHex(it).toByte).toArray

  def parseWithAccess(access: BitAccess, startBit: Int): (Int, Packet) =
    var curBit = startBit
    def consume(n: Int): Int =
      val r = access.get(curBit, n)
      curBit += n
      r
    val version = consume(3)
    val kind = consume(3)

    kind match
      case 4 =>
        // god is good...
        var bits = 0L
        debugger.debug("literal")
        def parseSegment(): Boolean =
          val continueFlag = consume(1)
          val subBits = consume(4)
          bits = (bits << 4) + subBits
          continueFlag != 0
        while parseSegment() do ()
        debugger.debug(s"literal bits: $bits")

        (curBit, Packet(version, PacketKind.Literal(bits)))
      case op =>
        // god is bad...
        val lengthType = consume(1)
        val packets =
          lengthType match
            case 0 =>
              val subPacketLength = consume(15)
              val packets = List.newBuilder[Packet]
              val target = curBit + subPacketLength
              debugger.debug("subpack " + subPacketLength)
              while curBit < target do
                val res = parseWithAccess(access, curBit)
                curBit = res._1
                packets.addOne(res._2)
              packets.result()
            case 1 =>
              val nSubPackets = consume(11)
              debugger.assert(nSubPackets != 0)
              debugger.debug("nsub " + nSubPackets)
              val packets = List.newBuilder[Packet]
              for _ <- 0 until nSubPackets do
                val res = parseWithAccess(access, curBit)
                curBit = res._1
                packets.addOne(res._2)
              packets.result()
            case _ => whatTheScallop.!
        debugger.assert(packets.nonEmpty)
        if op >= 5 then debugger.assert(packets.length == 2)
        (curBit, Packet(version, PacketKind.Operator(op, packets)))

  def loadPackets(in: Input): Packet =
    val access = BitAccess(in)
    parseWithAccess(access, 0)._2

  def part1(in: Input): Output =
    val packet = loadPackets(in)
    debugger.debug(packet)
    packet.deepChildren.map(_.version).foldLeft(0)(_ + _) + packet.version

  def part2(input: Array[Byte]): Output =
    val packet = loadPackets(input)
    debugger.debug:
      packet.deepChildren.filter:
        case Packet(_, PacketKind.Operator(k, vs)) => k >= 5 && vs.length != 2
        case _                                     => false
    packet.kind.resolve
