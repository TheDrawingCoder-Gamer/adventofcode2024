package gay.menkissing.advent
package y2020

import parsley.*
import parsley.combinator.*
import parsley.token.descriptions.LexicalDesc
import token.Lexer

object Day18 extends Problem:
  type Input = String
  type Output = BigInt

  override def parse(str: String): String = str

  val lexicalDesc = LexicalDesc.plain

  val lexer = new Lexer(lexicalDesc)

  // yes WE are parsing an ast for this problem!!!
  enum Expr:
    case Number(n: BigInt)
    case Atom(x: Expr)
    case Add(l: Expr, r: Expr)
    case Mult(l: Expr, r: Expr)

    def calc: BigInt =
      this match
        case Number(n)  => n
        case Atom(x)    => x.calc
        case Add(l, r)  => l.calc + r.calc
        case Mult(l, r) => l.calc * r.calc

  def primary(expr: => Parsley[Expr]): Parsley[Expr] =
    choice(
      lexer.lexeme.integer.decimal.map(Expr.Number.apply),
      lexer.lexeme.parens(expr).map(Expr.Atom.apply)
    )

  lazy val expr: Parsley[Expr] =
    import parsley.expr.*
    parsley.expr.precedence(primary(expr))(
      Ops[Expr](InfixL)(
        lexer.lexeme.symbol('*').map(_ => Expr.Mult.apply),
        lexer.lexeme.symbol('+').map(_ => Expr.Add.apply)
      )
    )

  // we (YES, WE) are taking advantage of the fact I overengineered part 1

  lazy val exprP2: Parsley[Expr] =
    import parsley.expr.*
    parsley.expr.precedence(primary(exprP2))(
      Ops[Expr](InfixL)(lexer.lexeme.symbol('+').map(_ => Expr.Add.apply)),
      Ops[Expr](InfixL)(lexer.lexeme.symbol('*').map(_ => Expr.Mult.apply))
    )

  override def part1(input: String): BigInt =
    input.linesIterator.map: line =>
      expr.parse(line).get.calc
    .sum

  def part2(input: String): BigInt =
    input.linesIterator.map: line =>
      exprP2.parse(line).get.calc
    .sum

  def input: String = FileIO.getInput(2020, 18)
