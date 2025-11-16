package gay.menkissing.advent
package y2021

import gay.menkissing.common.*

object Day24 extends Problem:
  type Input = Vector[Instruction]
  type Output = BigInt

  lazy val input = FileIO.getInput(2021, 24)

  enum Variable:
    case X, Y, Z, W
  object Variable:
    def parse(str: String): Variable =
      str match
        case "w" => W
        case "x" => X
        case "y" => Y
        case "z" => Z

  type Operand = Either[Int, Variable]

  object Operand:
    def parse(str: String): Operand =
      str.toIntOption.toLeft(Variable.parse(str))

  enum Instruction:
    case Inp(variable: Variable)
    // a Mul A 0; Add A N; is like assigning a value directly
    // (if it was evil)
    case Add(base: Variable, value: Operand)
    case Mul(base: Variable, value: Operand)
    case Div(base: Variable, value: Operand)
    case Mod(base: Variable, value: Operand)
    case Eql(base: Variable, value: Operand)

  object Instruction:
    def parse(line: String): Instruction =
      line match
        case s"inp $v"    => Inp(Variable.parse(v))
        case s"add $x $y" => Add(Variable.parse(x), Operand.parse(y))
        case s"mul $x $y" => Mul(Variable.parse(x), Operand.parse(y))
        case s"div $x $y" => Div(Variable.parse(x), Operand.parse(y))
        case s"mod $x $y" => Mod(Variable.parse(x), Operand.parse(y))
        case s"eql $x $y" => Eql(Variable.parse(x), Operand.parse(y))

  final case class FuncSegment(n: Int, m: Int):
    override def toString(): String = s"segment N=$n,M=$m"
    // each iteration:
    // if n >= 0 then either we stay in the same "ballpark" or we times by 26
    // if n < 0 then either we stay in the same "ballpark" or we divide by 26
    // Given a z value, incrementing w will only increase the result by 1
    // In my input, there is no positive N that is < 9, so it's impossible to trigger
    // the condition on those.
    // However, its still very possible for those with N < 0 to trigger the condition and make it so we reduce z
    // If I had to guess, the goal is to perfectly get every single one where N < 0
    // This is especially because it seems that it's perfectly balanced - 7 N < 0, 7 N > 0
    def eval(w: Int, z: Int): (Int, Int) =
      var x = z % 26
      // if n < 0 then we will not get 26* larger this step
      val iz = if n < 0 then z / 26 else z
      x += n
      // if x == w then we don't edit z at all, asides from diving it if n < 0
      if x != w then (26 * iz + w + m, x)
      else (iz, x)

  def parse(str: String): Vector[Instruction] =
    str.linesIterator.map(Instruction.parse).toVector

  def splitInstructions
    (
      input: Vector[Instruction]
    ): Vector[Vector[Instruction]] =
    val builder = Vector.newBuilder[Vector[Instruction]]
    var cur = input
    while cur.nonEmpty do
      val (valueT, rest) = cur.tail.span(!_.isInstanceOf[Instruction.Inp])
      builder += cur.head +: valueT
      cur = rest
    builder.result()

  def extractSegment(segment: Vector[Instruction]): FuncSegment =
    val n = segment(5).asInstanceOf[Instruction.Add].value.leftOrDie
    val m = segment(15).asInstanceOf[Instruction.Add].value.leftOrDie
    FuncSegment(n, m)

  def evaluateAll
    (
      segments: Vector[FuncSegment],
      input: IndexedSeq[Int]
    ): (Int, Int) =
    input.zip(segments).foldLeft((0, 0)):
      case ((z0, _), (w, segment)) => segment.eval(w, z0)

  def find(segments: Vector[FuncSegment], range: Range): BigInt =
    val (_, indices) =
      segments.zipWithIndex
        .foldLeft((Seq.empty[Int], IndexedSeq.empty[Option[Int]])):
          case ((stack, indices), (segment, i)) =>
            if segment.n >= 0 then (i +: stack, indices :+ None)
            else (stack.tail, indices :+ stack.headOption)
    BigInt:
      segments.indices.foldLeft(IndexedSeq.empty[Int]): (filled, k) =>
        val newFilled = filled :+ range.head
        indices(k) match
          // if this one is IMPORTANT, then try to find the optimial digit
          case Some(i) =>
            (for
              v1 <- range.view
              fillTest = newFilled.updated(i, v1)
              (_, v2) = evaluateAll(segments, fillTest)
              segment = segments(k)
              if segment.n >= 0 || range.contains(v2)
            yield fillTest.updated(k, v2)).head
          case _ => newFilled
      .mkString("")

  // i was lowkey very close to solving this on my own
  // i got the "emulate instructions using a function" part, but I think i got a little lost on correctly
  // assemblign the path
  def part1(input: Vector[Instruction]): BigInt =
    val segments = splitInstructions(input)
    assert(segments.length == 14)
    val frkySegments = segments.map(extractSegment)

    /*
    def evaluateAll(str: String): Long =
      frkySegments.zip(str).foldLeft(0L):
        case (z, (segment, c)) =>
        segment.eval(c.asDigit, z)
     */
    // in our input, x is ALWAYS reset to z modulo 26 at the start of each segment
    // shape of segment:
    // inp w
    // mul x 0
    // add x z
    // mod x 26
    // div z 1 or 26 // seems to be 26 if N < 0 and 1 otherwise, smells like truncated division and remainder
    // add x N
    // eql x w // always false when N >= 10?
    // eql x 0 // always true when N >= 10
    // mul y 0
    // add y 25
    // mul y x
    // add y 1
    // mul z y
    // mul y 0
    // add y w
    // add y M
    // mul y x
    // add z y
    // The value at the end in the `x` register is if ((z % 26) + N) != w
    // then, the `y` register will contain
    // Our only two inputs that aren't reset before usage are w and z.
    // when N < 0, we are basically "losing" a level, so it could possibly be represented as a stack
    find(frkySegments, (1 to 9).reverse)

  def part2(input: Vector[Instruction]): BigInt =
    val segments = splitInstructions(input).map(extractSegment)
    find(segments, 1 to 9)
