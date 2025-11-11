package gay.menkissing.advent
package y2015

import monocle.syntax.all.*
import gay.menkissing.common.*
import cats.data.*
import cats.*
import cats.derived.*
import cats.syntax.all.*

object Day22 extends Problem:
  type Input = Boss
  type Output = Int

  val debugger = Debuginator(active = true)

  type GameStateState[A] = State[GameState, A]

  def modify(f: GameState => GameState): GameStateState[Unit] = State.modify(f)
  def inspect[A](f: GameState => A): GameStateState[A] = State.inspect(f)

  final case class Boss(hp: Int, damage: Int) derives Show
  final case class Player(mana: Int, hp: Int, armor: Int) derives Show
  object Player:
    val initial: Player = Player(500, 50, 0)

  enum Spell(val cost: Int) derives Show:
    case MagicMissile extends Spell(53)
    case Drain extends Spell(73)
    case Shield extends Spell(113)
    case Poison extends Spell(173)
    case Recharge extends Spell(229)

    val cast: GameStateState[Int] =
      for
        _ <- modify(_.focus(_.player.mana).modify(_ - this.cost))
        _ <- modify(_.focus(_.manaSpent).modify(_ + this.cost))
        _ <- modify(_.focus(_.lastSpell).replace(Some(this)))
        _ <-
          this match
            case MagicMissile => modify(_.focus(_.boss.hp).modify(_ - 4))
            case Drain        =>
              modify(
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
      modify: state =>
        val newState = state.focus(_.activeEffects).modify(_.addEffect(this))
        this match
          case Shield   => newState.focus(_.player.armor).replace(7)
          case Poison   => newState
          case Recharge => newState

    lazy val perTurn: GameStateState[Unit] =
      inspect(_.activeEffects.effectLens(this).get != 0).ifMHalf:
        for
          _ <-
            this match
              case Shield   => State.pure(())
              case Poison   => modify(_.focus(_.boss.hp).modify(_ - 3))
              case Recharge => modify(_.focus(_.player.mana).modify(_ + 101))
          _ <-
            modify(
              _.focus(_.activeEffects).modify(_.effectLens(this).modify(_ - 1))
            )
          _ <-
            inspect(
              _.activeEffects.effectLens(this).get == 0
            ).ifM(this.end, ().pure)
        yield ()

    val end: GameStateState[Unit] =
      modify: state =>
        this match
          case Shield   => state.focus(_.player.armor).replace(0)
          case Poison   => state
          case Recharge => state

  final case class Effects
    (shield: Int = 0, poison: Int = 0, recharge: Int = 0)
      derives Show:
    def effectLens(effect: Effect) =
      effect match
        case Effect.Shield   => this.focus(_.shield)
        case Effect.Poison   => this.focus(_.poison)
        case Effect.Recharge => this.focus(_.recharge)

    def addEffect(effect: Effect): Effects =
      val lens = effectLens(effect)
      debugger.assert(lens.exist(_ == 0))
      lens.replace(effect.initTimer)

  final case class GameState
    (
      player: Player,
      boss: Boss,
      activeEffects: Effects,
      manaSpent: Int,
      lastSpell: Option[Spell]
    ) derives Show:
    private def spellIsValid(spell: Spell): Boolean =
      (player.mana >= spell.cost) && spell.match
        case Spell.MagicMissile => true
        case Spell.Drain        => true
        case Spell.Shield       => activeEffects.shield <= 1
        case Spell.Poison       => activeEffects.poison <= 1
        case Spell.Recharge     => activeEffects.recharge <= 1

    def validSpells: List[Spell] = Spell.values.toList.filter(spellIsValid)

  val applyEffects: GameStateState[Unit] =
    Effect.values.toList.map(_.perTurn).sequenceVoid

  def playerTurn(spell: Spell): GameStateState[Unit] =
    for
      _ <- applyEffects
      _ <- spell.cast
    yield ()

  def playerTurnP2(spell: Spell): GameStateState[Unit] =
    modify(_.focus(_.player.hp).modify(_ - 1)) *> inspect(_.player.hp > 0)
      .ifMHalf(playerTurn(spell))

  val bossTurn: GameStateState[Unit] =
    for
      _ <- applyEffects
      _ <-
        inspect(_.boss.hp > 0).ifMHalf:
          for
            damage <- inspect(_.boss.damage)
            armor <- inspect(_.player.armor)
            damageDealt = math.max(1, damage - armor)
            _ <- modify(_.focus(_.player.hp).modify(_ - damageDealt))
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
    val state = GameState(Player.initial, input, Effects(), 0, None)
    // This is the worst thing i have ever done
    val path =
      dijstraBy(
        state,
        _.boss.hp <= 0,
        (a, b) => b.manaSpent - a.manaSpent,
        state =>
          state.validSpells.map(spell => fullTurn(spell).runS(state).value)
            .filter(_.player.hp > 0)
      ).get

    debugger.verbose(path.flatMap(_.lastSpell).foldString("\n"))
    path.last.manaSpent

  def part2(input: Boss): OutputP2 =
    val state = GameState(Player.initial, input, Effects(), 0, None)
    dijstraBy(
      state,
      _.boss.hp <= 0,
      (a, b) => b.manaSpent - a.manaSpent,
      state =>
        state.validSpells.map(spell => fullTurnP2(spell).runS(state).value)
          .filter(_.player.hp > 0)
    ).get.last.manaSpent
