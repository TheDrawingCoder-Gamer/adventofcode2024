package gay.menkissing.advent
package y2021

import cats.data.*
import cats.implicits.*
import cats.*

import gay.menkissing.common.*

object Day21 extends Problem[Day21.ProblemState, BigInt]:
  case class Player(score: Int, space: Int):
    def move(n: Int): Player =
      val daSpace = (space + n) % 10
      val good = if daSpace == 0 then 10 else daSpace
      copy(score = score + good, space = good)
  
  case class ProblemState(dice: Int, nrolls: Int, p1: Player, p2: Player):
    def nextDiceDeterministic: ProblemState =
      copy(dice = (dice + 1) % 100, nrolls = nrolls + 1)
  
  val rollState: State[ProblemState, Int] = State: x =>
    (x.nextDiceDeterministic, x.dice + 1)
  
  
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
    val List(player1, player2) = 
      str.linesIterator.map: 
        case s"Player $_ starting position: $n" => Player(0, n.toInt)
      .toList
    ProblemState(0, 0, player1, player2)
  
  override def part1(input: ProblemState): BigInt =
    val (state, winner) = fullTurn.untilDefinedM.run(input).value
    BigInt:
      winner match
        case 1 => state.p2.score * state.nrolls
        case 2 => state.p1.score * state.nrolls
        case _ => 0
  
  
  case class P2State(pState: ProblemState, universeCount: BigInt)
  
  val dieCombos: List[(Int, BigInt)] =
    
    val possibleRolls =
      for
        x <- 1 to 3
        y <- 1 to 3
        z <- 1 to 3
      yield x + y + z
    possibleRolls.groupMapReduce(identity)(_ => BigInt(1))(_ + _).toList
  
  type P2Func[B] = Kleisli[List, P2State, B]
  def playerTurnP2(accessor: ProblemState => Player, setter: (ProblemState, Player) => ProblemState): P2Func[(P2State, Option[BigInt])] =
    Kleisli: state =>
      dieCombos.map: (it, v) =>
        val newPlayer = accessor(state.pState).move(it)
        (state.copy(pState = setter(state.pState, newPlayer),
          universeCount = state.universeCount * v),
          Option.when(newPlayer.score >= 21)(state.universeCount * v))
  
  val player1TurnP2 = playerTurnP2(_.p1, (s, p) => s.copy(p1 = p))
  val player2TurnP2 = playerTurnP2(_.p2, (s, p) => s.copy(p2 = p))
  
  val fullTurnP2: Reader[P2State, ((BigInt, BigInt), Chain[P2State])] = Reader: state =>
    // get all possible results of a player 1 turn...
    val p1 = player1TurnP2(state)
    val (p1Win, p1Cont) = p1.partition(_._2.isDefined)
    // For all turns where player one didn't win, then do a player 2 turn...
    val (p2Win, p2Cont) = p1Cont.map((s, _) => s).flatMap(player2TurnP2.apply).partition(_._2.isDefined)
    // return a list of problem states that have no winner
    // and count the ones where a player won.
    val p1WinCount = p1Win.map((_, p) => p.get).sum
    val p2WinCount = p2Win.map((_, p) => p.get).sum
    ((p1WinCount, p2WinCount), Chain.fromSeq(p2Cont.map((s, _) => s)))
  
  
  
  
  override def part2(input: ProblemState): BigInt =
    val p2State = P2State(input, BigInt(1))
    val (l, r) = unfolded((BigInt(0), BigInt(0), Chain(p2State))):
      case (p1Sum, p2Sum, states) =>
        Option.when(states.nonEmpty):
          val r = states.foldLeft((p1Sum, p2Sum, Chain[P2State]())):
            case ((p1, p2, allStates), state) =>
              val (counts, list) = fullTurnP2(state)
  
              (p1 + counts._1, p2 + counts._2, allStates ++ list)
          ((r._1, r._2), r)
    l.max(r)

