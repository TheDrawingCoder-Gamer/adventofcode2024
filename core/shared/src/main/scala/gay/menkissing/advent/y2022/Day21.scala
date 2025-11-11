package gay.menkissing.advent
package y2022

object Day21 extends Problem:
  type Input = Context
  type Output = Long

  val testInput =
    """|root: pppw + sjmn
       |dbpl: 5
       |cczh: sllz + lgvd
       |zczc: 2
       |ptdq: humn - dvpt
       |dvpt: 3
       |lfqf: 4
       |humn: 5
       |ljgn: 2
       |sjmn: drzm * dbpl
       |sllz: 4
       |pppw: cczh / lfqf
       |lgvd: ljgn * ptdq
       |drzm: hmdt - zczc
       |hmdt: 32""".stripMargin
  enum Operation:
    case Multiply, Divide, Add, Subtract
    def eval(l: Long, r: Long): Long =
      this match
        case Operation.Multiply => l * r
        case Operation.Divide   => l / r
        case Operation.Add      => l + r
        case Operation.Subtract => l - r
    def invL(t: Long, r: Long): Long =
      this match
        case Operation.Multiply => t / r
        case Operation.Divide   => t * r
        case Operation.Add      => t - r
        case Operation.Subtract => t + r
    def invR(t: Long, l: Long): Long =
      this match
        case Operation.Multiply => t / l
        case Operation.Divide   => l / t
        case Operation.Add      => t - l
        case Operation.Subtract => l - t

  enum Expression:
    case Number(n: Long) extends Expression
    case Op(op: Operation, l: String, r: String) extends Expression
    case Unknown

  def buildExpr(ctx: Context): BuiltExpr =
    def buildExprHelper(expr: Expression): BuiltExpr =
      expr match
        case Expression.Number(n)    => BuiltExpr.Number(n)
        case Expression.Op(op, l, r) =>
          val e =
            BuiltExpr.Op(
              buildExprHelper(ctx.monkes(l)),
              op,
              buildExprHelper(ctx.monkes(r))
            )
          if e.canEval then BuiltExpr.Number(e.eval)
          else e
        case Expression.Unknown => BuiltExpr.Human
    buildExprHelper(ctx.monkes("root"))

  enum BuiltExpr:
    case Number(n: Long) extends BuiltExpr
    case Op(l: BuiltExpr, op: Operation, r: BuiltExpr) extends BuiltExpr
    case Human

    def canEval: Boolean =
      this match
        case BuiltExpr.Number(_)    => true
        case BuiltExpr.Op(l, op, r) => l.canEval && r.canEval
        case BuiltExpr.Human        => false
    def eval: Long =
      this match
        case BuiltExpr.Number(n)    => n
        case BuiltExpr.Op(l, op, r) => op.eval(l.eval, r.eval)
        case BuiltExpr.Human        => assert(false)
    def solve(res: Long): Long =
      this match
        case BuiltExpr.Op(Number(l), op, r) => r.solve(op.invR(res, l))
        case BuiltExpr.Op(l, op, Number(r)) => l.solve(op.invL(res, r))
        case BuiltExpr.Human                => res
        case _ => throw new Exception(s"unhandled $this")

  final case class Context(monkes: Map[String, Expression])
  def evaluate(ctx: Context, expr: Expression): Long =
    expr match
      case Expression.Number(n)    => n
      case Expression.Op(op, l, r) =>
        val ll = evaluate(ctx, ctx.monkes(l))
        val rr = evaluate(ctx, ctx.monkes(r))
        op match
          case Operation.Multiply => ll * rr
          case Operation.Divide   => ll / rr
          case Operation.Add      => ll + rr
          case Operation.Subtract => ll - rr
      case Expression.Unknown => assert(false)

  lazy val input = FileIO.getInput(2022, 21)
  def parseMonke(input: String): (String, Expression) =
    input match
      case s"$monke: $l $op $r" =>
        val daOp =
          op match
            case "+" => Operation.Add
            case "-" => Operation.Subtract
            case "*" => Operation.Multiply
            case "/" => Operation.Divide
            case _   => ???
        monke -> Expression.Op(daOp, l, r)
      case s"$monke: $n" => monke -> Expression.Number(n.toLong)
      case _             => ???
  def parse(input: String): Context =
    Context(input.linesIterator.map(parseMonke).toMap)
  def part1(ctx: Context): Long =
    val root = ctx.monkes("root")
    evaluate(ctx, root)
  def part2(badCtx: Context): Long =
    import BuiltExpr.*
    val ctx =
      badCtx.copy(monkes = badCtx.monkes.updated("humn", Expression.Unknown))

    val Op(l, _, r) = buildExpr(ctx): @unchecked
    assert(l.canEval ^ r.canEval, "one side must be unevaluatable")

    val (expr, value) = if l.canEval then (r, l.eval) else (l, r.eval)
    expr.solve(value)
