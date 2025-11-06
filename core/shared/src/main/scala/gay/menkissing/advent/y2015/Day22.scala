package gay.menkissing.advent
package y2015

import monocle.syntax.all.*
import gay.menkissing.common.*
import cats.data.State
import cats.implicits.*
import cats.syntax.validated

object Day22 extends Problem:
  type Input = Boss
  type Output = Int

  val debugger = Debuginator(active = true)

  type GameStateState[A] = State[GameState, A]

  case class Boss(hp: Int, damage: Int)
  case class Player(mana: Int, hp: Int, armor: Int)
  object Player:
    val initial: Player = Player(500, 50, 0)

  enum Spell(val cost: Int):
    case MagicMissile extends Spell(53)
    case Drain extends Spell(73)
    case Shield extends Spell(113)
    case Poison extends Spell(173)
    case Recharge extends Spell(229)

    val cast: GameStateState[Int] =
      for
        _ <-
          State.modify[GameState](_.focus(_.player.mana).modify(_ - this.cost))
        _ <- State.modify[GameState](_.focus(_.manaSpent).modify(_ + this.cost))
        _ <-
          this match
            case MagicMissile =>
              State.modify[GameState](_.focus(_.boss.hp).modify(_ - 4))
            case Drain =>
              State.modify[GameState](
                _.focus(_.boss.hp).modify(_ - 2).focus(_.player.hp)
                  .modify(_ + 2)
              )
            case Shield   => Effect.Shield.start
            case Poison   => Effect.Poison.start
            case Recharge => Effect.Recharge.start
      yield this.cost

  enum Effect(val initTimer: Int):
    case Shield extends Effect(6)
    case Poison extends Effect(6)
    case Recharge extends Effect(5)
    val start: GameStateState[Unit] =
      State.modify: state =>
        val newState = state.focus(_.activeEffects).modify(_.addEffect(this))
        this match
          case Shield   => newState.focus(_.player.armor).replace(7)
          case Poison   => newState
          case Recharge => newState

    val perTurn: GameStateState[Unit] =
      State
        .inspect[GameState, Boolean](_.activeEffects.effectLens(this).get != 0)
        .flatMap: notZero =>
          if notZero then
            for
              _ <-
                this match
                  case Shield => State.pure(())
                  case Poison =>
                    State.modify[GameState](_.focus(_.boss.hp).modify(_ - 3))
                  case Recharge =>
                    State
                      .modify[GameState](_.focus(_.player.mana).modify(_ + 101))
              _ <-
                State.modify[GameState](
                  _.focus(_.activeEffects)
                    .modify(_.effectLens(this).modify(_ - 1))
                )
              isZero <-
                State.inspect[GameState, Boolean](
                  _.activeEffects.effectLens(this).get == 0
                )
              _ <- if isZero then this.end else State.pure(())
            yield ()
          else State.pure(())

    val end: GameStateState[Unit] =
      State.modify: state =>
        this match
          case Shield   => state.focus(_.player.armor).replace(0)
          case Poison   => state
          case Recharge => state

  case class Effects(shield: Int = 0, poison: Int = 0, recharge: Int = 0):
    def effectLens(effect: Effect) =
      effect match
        case Effect.Shield   => this.focus(_.shield)
        case Effect.Poison   => this.focus(_.poison)
        case Effect.Recharge => this.focus(_.recharge)

    def addEffect(effect: Effect): Effects =
      val lens = effectLens(effect)
      debugger.assert(lens.exist(_ == 0))
      lens.replace(effect.initTimer)

  case class GameState
    (
      player: Player,
      boss: Boss,
      activeEffects: Effects,
      manaSpent: Int
    ):
    private def spellIsValid(spell: Spell): Boolean =
      (player.mana >= spell.cost) &&
        (spell match
          case Spell.MagicMissile => true
          case Spell.Drain        => true
          case Spell.Shield       => activeEffects.shield <= 1
          case Spell.Poison       => activeEffects.poison <= 1
          case Spell.Recharge     => activeEffects.recharge <= 1)

    def validSpells: List[Spell] = Spell.values.toList.filter(spellIsValid)

  val applyEffects: GameStateState[Unit] =
    Effect.values.toList.map(_.perTurn).sequenceVoid

  def playerTurn(spell: Spell): GameStateState[Unit] =
    for
      _ <- applyEffects
      _ <- spell.cast
    yield ()

  def playerTurnP2(spell: Spell): GameStateState[Unit] =
    State.modify[GameState](_.focus(_.player.hp).modify(_ - 1)) *>
      State.inspect[GameState, Int](_.player.hp)
        .flatMap(hp => if hp > 0 then playerTurn(spell) else State.pure(()))

  val bossTurn: GameStateState[Unit] =
    for
      _ <- applyEffects
      isDeadAlreadyLmao <- State.inspect[GameState, Boolean](_.boss.hp <= 0)
      _ <-
        if isDeadAlreadyLmao then State.pure(())
        else
          for
            damage <- State.inspect[GameState, Int](_.boss.damage)
            armor <- State.inspect[GameState, Int](_.player.armor)
            damageDealt = math.max(1, damage - armor)
            _ <-
              State
                .modify[GameState](_.focus(_.player.hp).modify(_ - damageDealt))
          yield ()
    yield ()

  def fullTurn(spell: Spell): GameStateState[Unit] =
    playerTurn(spell) *> bossTurn

  def fullTurnP2(spell: Spell): GameStateState[Unit] =
    playerTurnP2(spell) *> bossTurn

  lazy val input = FileIO.getInput(2015, 22)

  def parse(str: String): Input =
    val Array(
      s"Hit Points: $hp",
      s"Damage: $dmg"
    ) = str.linesIterator.toArray: @unchecked
    Boss(hp.toInt, dmg.toInt)

  def part1(input: Boss): OutputP1 =
    val state = GameState(Player.initial, input, Effects(), 0)
    // This is the worst thing i have ever done
    dijstraBy(
      state,
      _.boss.hp <= 0,
      (a, b) => b.manaSpent - a.manaSpent,
      state =>
        state.validSpells.map(spell => fullTurn(spell).runS(state).value)
          .filter(_.player.hp > 0)
    ).get.last.manaSpent

  def part2(input: Boss): OutputP2 =
    val state = GameState(Player.initial, input, Effects(), 0)
    dijstraBy(
      state,
      _.boss.hp <= 0,
      (a, b) => b.manaSpent - a.manaSpent,
      state =>
        state.validSpells.map(spell => fullTurnP2(spell).runS(state).value)
          .filter(_.player.hp > 0)
    ).get.last.manaSpent
