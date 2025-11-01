package gay.menkissing.advent
package y2024

object Day24 extends ProblemAdv:
  type Input = Machine
  type OutputP1 = Long
  type OutputP2 = String

  def showOutputP1 = summon
  def showOutputP2 = summon

  case class Machine
    (
      starts: Map[String, Long],
      eqs: Map[String, BooleanEquation]
    ):
    def swap(l: String, r: String): Machine =
      val (eq1, eq2) = (eqs(l), eqs(r))
      copy(eqs = eqs.updated(l, eq2).updated(r, eq1))

    def broken: Boolean = binary("x") + binary("y") != binary("z")

    def solve(key: String): Long =
      eqs.get(key) match
        case Some(BooleanEquation(l, r, op)) => op.combine(solve(l), solve(r))
        case None                            => starts(key)

    def binary(prefix: String): Long =
      keys(prefix).foldRight(0L):
        case (key, acc) => (acc << 1) + solve(key)

    def keys(prefix: String): Vector[String] = (starts.keySet ++ eqs.keySet)
      .filter(_.startsWith(prefix)).toVector.sorted

    def circuit(key: String, loop: Set[String] = Set.empty): Circuit =
      if loop(key) then InvalidCircuit
      else
        eqs.get(key) match
          case Some(BooleanEquation(l, r, op)) =>
            op.circuit(circuit(l, loop + key), circuit(r, loop + key))
          case None => CircuitKey(key)

    def inputsTo(key: String): Set[String] =
      eqs.get(key) match
        case Some(BooleanEquation(l, r, _)) =>
          Set(key) ++ inputsTo(l) ++ inputsTo(r)
        case None => Set.empty

  extension (self: Circuit | String)
    def asCircuit: Circuit =
      self match
        case it: Circuit => it
        case k: String   => CircuitKey(k)

  enum Op:
    case And, Xor, Or

    def combine(l: Long, r: Long): Long =
      this match
        case Op.And => l & r
        case Op.Or  => l | r
        case Op.Xor => l ^ r

    def circuit(l: Circuit, r: Circuit): Circuit = CircuitOp(this, l, r)

    def apply(l: Circuit | String, r: Circuit | String): Circuit =
      CircuitOp(this, l.asCircuit, r.asCircuit)

  object Op:
    def parse(str: String): Op =
      str match
        case "XOR" => Op.Xor
        case "OR"  => Op.Or
        case "AND" => Op.And

  sealed trait Circuit

  case object InvalidCircuit extends Circuit

  case class CircuitKey(x: String) extends Circuit

  case class CircuitOp(op: Op, l: Circuit, r: Circuit) extends Circuit:
    override def equals(obj: Any): Boolean =
      obj match
        case CircuitOp(tOp, tL, tR) =>
          (tOp == op) && ((tL == l && tR == r) || (tL == r && tR == l))
        case _ => false

  case class BooleanEquation(l: String, r: String, op: Op)

  def keyOfInt(prefix: String, i: Int) =
    if i < 10 then s"${prefix}0$i" else s"$prefix$i"

  def add2bit(i: Int): Circuit =
    val (x, y) = (keyOfInt("x", i), keyOfInt("y", i))
    if i == 0 then Op.Xor(x, y)
    else Op.Xor(Op.Xor(x, y), overflow2bit(i - 1))

  def overflow2bit(i: Int): Circuit =
    val (x, y) = (keyOfInt("x", i), keyOfInt("y", i))
    if i == 0 then Op.And(x, y)
    else Op.Or(Op.And(Op.Xor(x, y), overflow2bit(i - 1)), Op.And(x, y))

  override def parse(str: String): Machine =
    val Array(starts, eqs) = str.split("\n\n")

    val goodStarts =
      starts.linesIterator.map:
        case s"$x: $y" => x -> y.toLong

    val goodEqs =
      eqs.linesIterator.map:
        case s"$l $op $r -> $res" => res -> BooleanEquation(l, r, Op.parse(op))

    Machine(goodStarts.toMap, goodEqs.toMap)

  override def part1(input: Machine): Long = input.binary("z")

  def solve(machine: Machine): Iterator[Vector[String]] =
    Iterator.unfold(machine): machine2 =>
      Option.when(machine2.broken)(fix(machine2).next())

  def fix(machine: Machine): Iterator[(Vector[String], Machine)] =
    for
      key <- machine.keys("z").iterator
      index = key.tail.toInt
      circuit = add2bit(index)
      if machine.circuit(key) != circuit
      key0 <- machine.inputsTo(key)
      key1 <- machine.eqs.keySet
      machine2 = machine.swap(key0, key1)
      if machine2.circuit(key) == circuit
    yield Vector(key0, key1) -> machine2

  override def part2(input: Machine): String =
    solve(input).flatten.toVector.sorted.mkString(",")

  override lazy val input: String = FileIO.getInput(2024, 24)
