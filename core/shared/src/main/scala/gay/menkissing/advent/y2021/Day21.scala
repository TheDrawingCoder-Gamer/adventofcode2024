package gay.menkissing.advent
package y2021

import cats.data.*
import cats.implicits.*
import cats.*

import gay.menkissing.common.*

import monocle.*
import monocle.macros.*

object Day21 extends Problem:
  type Input = ProblemState
  type Output = Long

  final case class Player(score: Int, space: Int):
    def move(n: Int): Player =
      val daSpace = (space + n) % 10
      val good = if daSpace == 0 then 10 else daSpace
      copy(score = score + good, space = good)

  final case class ProblemState(dice: Int, nrolls: Int, p1: Player, p2: Player):
    def nextDiceDeterministic: ProblemState =
      copy(dice = (dice + 1) % 100, nrolls = nrolls + 1)

  val rollState: State[ProblemState, Int] =
    State: x =>
      (x.nextDiceDeterministic, x.dice + 1)

  val takeTurn: State[ProblemState, Int] =
    for
      x <- rollState
      y <- rollState
      z <- rollState
    yield x + y + z

  def playerTurn
    (lens: Lens[ProblemState, Player]): State[ProblemState, Boolean] =
    for
      score <- takeTurn
      _ <- State.modify[ProblemState](lens.modify(_.move(score)))
      res <- State.inspect[ProblemState, Int](lens.get.andThen(_.score))
    yield res >= 1000

  val player1Turn = playerTurn(GenLens[ProblemState](_.p1))
  val player2Turn = playerTurn(GenLens[ProblemState](_.p2))

  val fullTurn =
    for
      r1 <- player1Turn
      r2 <-
        if r1 then State.pure(Some(1))
        else player2Turn.map(it => Option.when(it)(2))
    yield r2

  lazy val input = FileIO.getInput(2021, 21)

  override def parse(str: String): ProblemState =
    val List(player1, player2) =
      str.linesIterator.map:
        case s"Player $_ starting position: $n" => Player(0, n.toInt)
      .toList
    ProblemState(0, 0, player1, player2)

  override def part1(input: ProblemState): Long =
    val (state, winner) = fullTurn.untilDefinedM.run(input).value

    winner match
      case 1 => state.p2.score * state.nrolls
      case 2 => state.p1.score * state.nrolls
      case _ => 0

  final case class Players(p1: Player, p2: Player)
  final case class P2State(p1: Player, p2: Player, universeCount: Long):
    inline def updatePlayer[N <: Int](w: Player, uc: Long): P2State =
      inline compiletime.constValue[N] match
        case 1 => copy(p1 = w, universeCount = uc)
        case 2 => copy(p2 = w, universeCount = uc)
        case _ => compiletime.error("what the scallop!")
    // awesome sauce!!!
    inline def getPlayer[N <: Int]: Player =
      inline compiletime.constValue[N] match
        case 1 => p1
        case 2 => p2
        case _ => compiletime.error("what the scallop!")
  object P2State:
    def fromProblemState(p: ProblemState): P2State = P2State(p.p1, p.p2, 1L)

  val dieCombos: List[(Int, Long)] =

    val possibleRolls =
      for
        x <- 1 to 3
        y <- 1 to 3
        z <- 1 to 3
      yield x + y + z
    possibleRolls.groupMapReduce(identity)(_ => 1L)(_ + _).toList

  inline def playerTurnP2[I <: Int]: P2State => List[(P2State, Option[Long])] =
    state =>
      dieCombos.map: (it, v) =>
        val newPlayer = state.getPlayer[I].move(it)
        (
          state.updatePlayer[I](newPlayer, state.universeCount * v),
          Option.when(newPlayer.score >= 21)(state.universeCount * v)
        )

  val player1TurnP2 = playerTurnP2[1]
  val player2TurnP2 = playerTurnP2[2]

  def fullTurnP2(state: P2State): (Long, Long, Chain[P2State]) =
    // get all possible results of a player 1 turn...
    val p1 = player1TurnP2(state)
    val (p1Win, p1Cont) =
      p1.partitionEither:
        case (_, Some(v)) => Left(v)
        case (s, _)       => Right(s)
    // For all turns where player one didn't win, then do a player 2 turn...
    val (p2Win, p2Cont) =
      p1Cont.flatMap(player2TurnP2.apply).partitionEither:
        case (_, Some(v)) => Left(v)
        case (s, _)       => Right(s)
    // return a list of problem states that have no winner
    // and count the ones where a player won.
    val p1WinCount = p1Win.sum
    val p2WinCount = p2Win.sum
    (p1WinCount, p2WinCount, Chain.fromSeq(p2Cont))

  override def part2(input: ProblemState): Long =
    val p2State = P2State.fromProblemState(input)
    // given Semigroup[Long] = _ + _
    // TODO: where is it getting its Long semigroup instance???
    val (l, r) =
      unfolded((0L, 0L, Chain(p2State))):
        case (p1Sum, p2Sum, states) =>
          Option.when(states.nonEmpty):
            val r =
              states.foldLeft((p1Sum, p2Sum, Chain[P2State]())):
                case (fs, state) => fs |+| fullTurnP2(state)
            ((r._1, r._2), r)
    l.max(r)
