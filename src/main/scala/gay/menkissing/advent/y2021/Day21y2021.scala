package gay.menkissing.advent
package y2021

import cats.*
import cats.implicits.*
import cats.data.{State, Reader}

object Day21y2021 extends Problem[Day21y2021.ProblemState, BigInt]:
  case class Player(score: Int, space: Int):
    def move(n: Int): Player =
      val daSpace = (space + n) % 10
      val good = if daSpace == 0 then 10 else daSpace
      copy(score = score + good, space = good)

  case class ProblemState(dice: Int, nrolls: Int, p1: Player, p2: Player):
    def nextDiceDeterministic: ProblemState =
      copy(dice = (dice + 1) % 100, nrolls = nrolls + 1)

  val rollState: State[ProblemState, Int] = State { x =>
    (x.nextDiceDeterministic, x.dice + 1)
  }

  val takeTurn: State[ProblemState, Int] =
    for
      x <- rollState
      y <- rollState
      z <- rollState
    yield x + y + z

  def playerTurn(accessor: ProblemState => Player, setter: (ProblemState, Player) => ProblemState): State[ProblemState, Boolean] =
    for
      score <- takeTurn
      _ <- State.modify[ProblemState](state => setter(state, accessor(state).move(score)))
      res <- State.inspect[ProblemState, Int](accessor.andThen(_.score))
    yield res >= 1000

  val player1Turn = playerTurn(_.p1, (s, p) => s.copy(p1 = p))
  val player2Turn = playerTurn(_.p2, (s, p) => s.copy(p2 = p))

  val fullTurn =
    for
      r1 <- player1Turn
      r2 <-
        if r1 then
          State.pure(Some(1))
        else
          player2Turn.map(it => Option.when(it)(2))
    yield r2

  lazy val input = FileIO.getInput(2021, 21)

  override def parse(str: String): ProblemState =
    val List(player1, player2) = input.linesIterator.map { it =>
      Player(0, it.dropWhile(_ != ':').drop(1).trim.toInt)
    }.toList
    ProblemState(0, 0, player1, player2)

  override def part1(input: ProblemState): BigInt =
    val (state, winner) = fullTurn.untilDefinedM.run(input).value
    BigInt:
      winner match
        case 1 => state.p2.score * state.nrolls
        case 2 => state.p1.score * state.nrolls
        case _ => 0


  case class P2State(pState: ProblemState, universeCount: BigInt)

  val dieCombos: List[(Int, BigInt)] = {
    val possibleRolls =
      (for {
        x <- 1 to 3
        y <- 1 to 3
        z <- 1 to 3
      } yield x + y + z).toList
    possibleRolls.groupMapReduce(identity)(_ => BigInt(1))(_ + _).toList
  }

  def playerTurnP2(accessor: ProblemState => Player, setter: (ProblemState, Player) => ProblemState): Reader[P2State, List[(P2State, Option[BigInt])]] =
    Reader: state =>
      dieCombos.map: (it, v) =>
        val newPlayer = accessor(state.pState).move(it)
        (state.copy(pState = setter(state.pState, newPlayer), universeCount = state.universeCount * v), Option.when(newPlayer.score >= 21)(state.universeCount * v))

  val player1TurnP2 = playerTurnP2(_.p1, (s, p) => s.copy(p1 = p))
  val player2TurnP2 = playerTurnP2(_.p2, (s, p) => s.copy(p2 = p))

  val fullTurnP2: Reader[P2State, (List[P2State], (BigInt, BigInt))] = Reader { state =>
    // get all possible results of a player 1 turn...
    val p1 = player1TurnP2(state)
    val (p1Win, p1Cont) = p1.partition(_._2.isDefined)
    // For all turns where player one didn't win, then do a player 2 turn...
    val (p2Win, p2Cont) = p1Cont.map((s, _) => s).flatMap(player2TurnP2.apply).partition(_._2.isDefined)
    // return a list of problem states that have no winner
    // and count the ones where a player won.
    (p2Cont.map((s, _) => s), (
      p1Win.map((_, p) => p.get).sum,
      p2Win.map((_, p) => p.get).sum
    ))
  }

  override def part2(input: ProblemState): BigInt =
    def runDiracDice: (BigInt, BigInt) = {
      var p1Wins = BigInt(0)
      var p2Wins = BigInt(0)
      var states = List[P2State](P2State(input, BigInt(1)))
      while (states.nonEmpty) {
        val res = states.map(fullTurnP2.apply)
        states = res.flatMap { case (s, (p1, p2)) =>
          p1Wins += p1
          p2Wins += p2
          s
        }
      }
      (p1Wins, p2Wins)
    }
    val (l, r) = runDiracDice
    l.max(r)

