package adventofcode

import java.time.{LocalTime, OffsetTime}
import java.util.concurrent.atomic.AtomicInteger

import adventofcode.common.{Puzzle, Test}

/** --- Day 22: Wizard Simulator 20XX ---
  * In this version, combat still proceeds with the player and the boss taking alternating turns.
  * The player still goes first.
  * Now, however, you don't get any equipment; instead, you must choose one of your spells to cast.
  * The first character at or below 0 hit points loses.
  *
  * Since you're a wizard, you don't get to wear armor, and you can't attack normally.
  * However, since you do magic damage, your opponent's armor is ignored,
  * and so the boss effectively has zero armor as well. As before,
  * if armor (from a spell, in this case) would reduce damage below 1,
  * it becomes 1 instead - that is, the boss' attacks always deal at least 1 damage.
  *
  * On each of your turns, you must select one of your spells to cast.
  * If you cannot afford to cast any spell, you lose. Spells cost mana; you start with 500 mana,
  * but have no maximum limit. You must have enough mana to cast a spell, and its cost is immediately
  * deducted when you cast it. Your spells are Magic Missile, Drain, Shield, Poison, and Recharge.
  *
  * Effects all work the same way.
  * Effects apply at the start of both the player's turns and the boss' turns.
  * Effects are created with a timer (the number of turns they last); at the start of each turn,
  * after they apply any effect they have, their timer is decreased by one.
  * If this decreases the timer to zero, the effect ends. You cannot cast a spell that would start an effect
  * which is already active. However, effects can be started on the same turn they end.
  *
  * You start with 50 hit points and 500 mana points.
  * The boss's actual stats are in your puzzle input. What is the least amount of mana you can spend
  * and still win the fight?
  * (Do not include mana recharge effects as "spending" negative mana.)
  */

object Day22 {

  private def nameOf(obj: Any) = obj.getClass.getSimpleName.replace("$","").replaceAll("""([A-Z])""", " $1").trim

  /**
    * Character
    */
  abstract class Char {

    def isAlive: Boolean = hitPoints > 0

    val name: String = nameOf(this)

    var hitPoints: Int

    val actions: List[Action]

    def describe: String
  }

  /**
    * Fight Player vs Boss
    */
  trait Fight {
    val player: Player
    val boss: Boss
    def actor: Char
    def addEffect(e: Effect): Unit
  }

  /**
    * Action, that character can take
    */
  trait Action {
    def describe(f: Fight): String

    def apply(f: Fight) {}

    def name: String = nameOf(this)
  }

  /**
    * Spell is magic action, costs mana
    */
  abstract class Spell(val cost: Int) extends Action {
    override def describe(f: Fight): String = s"${f.player.name} casts $name"

    override def apply(f: Fight): Unit = {
      f.player.mana -= cost
    }
  }

  /**
    * Effect is a spell that lasts for some turns
    */
  abstract class Effect(val turns: Int, cost: Int) extends Spell(cost) {
    override def apply(f: Fight): Unit = {
      super.apply(f)
      f.addEffect(this)
    }
  }

  /**
    * Effect, that is applied every turn
    */
  trait TurnEffect extends Effect {
    def beforeTurn(f: Fight)

    def describeEffect(f: Fight): String
  }

  /**
    * Effect, that affects environment
    */
  trait LastingEffect extends Effect {
    def unapply(f: Fight)

    def describeUnapply(f: Fight): String
  }

  /**
    * Boss, physical attack only, no armor against magic attacks
    */
  class Boss(var hitPoints: Int,
             val damage: Int)
    extends Char with Cloneable {
    def copy: Boss = super.clone().asInstanceOf[Boss]

    override def describe = s"$name has $hitPoints hit points."

    override val actions = List(PhysicalAttack)
  }

  object PhysicalAttack extends Action {
    override def describe(f: Fight): String =
      s"${f.boss.name} attacks for ${f.boss.damage} - ${f.player.armor} " +
        s"= ${(f.boss.damage - f.player.armor) max 1} damage!"

    override def apply(f: Fight): Unit = {
      f.player.hitPoints -= (f.boss.damage - f.player.armor) max 1
    }
  }

  /**
    * Player, casts magical actions
    */
  class Player(var hitPoints: Int,
               var mana: Int)
    extends Char with Cloneable {
    def copy: Player = super.clone().asInstanceOf[Player]

    var armor: Int = 0

    override def describe = s"$name has $hitPoints hit points, $armor armor, $mana mana"

    override val actions = List(MagicMissile, Drain, Shield, Poison, Recharge)

    def ->(boss: Boss): State = InitialState(Turn(this, boss, this))
    def -->(boss: Boss): State = InitialState(Turn(this, boss, this).applyEffects())
  }

  // Spells and effects:

  object MagicMissile extends Spell(53) {
    override def apply(f: Fight): Unit = {
      super.apply(f)
      f.boss.hitPoints -= 4
    }

    override def describe(f: Fight) = s"${super.describe(f)}, dealing 4 damage"
  }

  object Drain extends Spell(73) {
    override def apply(f: Fight) = {
      super.apply(f)
      f.boss.hitPoints -= 2
      f.player.hitPoints += 2
    }

    override def describe(f: Fight) =
      s"${super.describe(f)}, dealing 2 damage and healing 2 hit points"
  }

  object Shield extends Effect(turns = 6, cost = 113) with LastingEffect {
    override def describe(f: Fight) = s"${super.describe(f)}, increasing armor by 7"

    override def apply(f: Fight) = {
      super.apply(f)
      f.player.armor += 7
    }

    override def unapply(f: Fight) = {
      f.player.armor -= 7
    }

    override def describeUnapply(f: Fight) = s"$name wears off, decreasing armor by 7"
  }

  object Poison extends Effect(turns = 6, cost = 173) with TurnEffect {
    override def beforeTurn(f: Fight) = {
      f.boss.hitPoints -= 3
    }

    override def describeEffect(f: Fight) = s"$name deals 3 damage"
  }

  object Recharge extends Effect(turns = 5, cost = 229) with TurnEffect {
    override def beforeTurn(f: Fight): Unit = {
      f.player.mana += 101
    }

    override def describeEffect(f: Fight) = s"$name provides 101 mana"
  }

  object Hell extends Effect(turns = 10000, cost = 0) with TurnEffect {
    override def beforeTurn(f: Fight): Unit = {
      if (f.actor.isInstanceOf[Player]) {
        f.player.hitPoints -= 1
      }
    }

    override def describeEffect(f: Fight): String =
      if (f.actor.isInstanceOf[Player]) s"${f.player.name} loses 1 hit point"
      else ""
  }

  case class Turn(player: Player,
                  boss: Boss,
                  var actor: Char,
                  var effects: Map[Effect, Int] = Map())
    extends Fight {
    def describe: String = "\n" +
      s"-- ${actor.name} turn --\n\n" +
      s"- ${player.describe}\n" +
      s"- ${boss.describe}\n" +
      effects.map {
        case (e, time) => e match {
          case t: TurnEffect => s"${t.describeEffect(this)}; it's timer is now ${time - 1}.\n"
          case _ => s"${e.name}'s timer is now ${time - 1}.\n"
        }
      }.mkString

    def possibleActions: Iterator[Action] = actor.actions.iterator
      .filterNot { case spell: Spell => spell.cost > player.mana  ; case _ => false }
      .filterNot { case effect: Effect => effects.contains(effect); case _ => false }

    def result: Option[Char] =
      if (player.isAlive && boss.isAlive) None
      else if (! player.isAlive) Some(boss)
      else Some(player)

    def nextActor: Char = if (actor.isInstanceOf[Player]) boss else player

    def applyEffects(): Turn = {
      effects.keys.foreach {
        case t: TurnEffect => t.beforeTurn(this)
        case _ =>
      }
      val (active, ended) = effects.mapValues { _ - 1 }.partition { _._2 > 0 }
      ended.keys.foreach {
        case t: LastingEffect => t.unapply(this)
        case _ =>
      }
      effects = active
      this
    }

    override def addEffect(e: Effect): Unit = {
      effects += (e -> e.turns)
    }

    def apply(action: Action): Turn = {
      val next = Turn(
        player.copy,
        boss.copy,
        actor,
        effects
      )
      if (! next.possibleActions.contains(action))
        sys.error(s"${action.name} is not available for ${actor.name}")
      action.apply(next)
      next.actor = nextActor
      next
    }

    def next(action: Action): Turn = apply(action).applyEffects()

    def next2(action: Action): Turn = {
      val next = Turn(player.copy, boss.copy, actor, effects)
      action.apply(next)
      next.actor = next.nextActor
      if (next.result.isEmpty) next.applyEffects()
      if (next.result.isEmpty) PhysicalAttack.apply(next)
      next.actor = actor
      if (next.result.isEmpty) next.applyEffects()
      next
    }
  }

  sealed abstract class State {
    def turn: Turn
    val spentMana: Int = this match {
      case _ : InitialState => 0
      case Move(prev, a: Spell, _) => prev.spentMana + a.cost
      case Move(prev, _, _) => prev.spentMana
    }
    def apply(action: Action) = Move(this, action, turn next2 action)
    def describe(actions: Action*): State = actions.foldLeft(this) { (state, action) =>
      if (state.isInstanceOf[InitialState]) {
        println(state.turn.describe)
        state.turn.applyEffects()
      }
      if (state.turn.result.isDefined)
        sys.error(s"Cannot apply ${action.name}, game is ended!")
      println(action.describe(state.turn))
      val next = state.turn apply action
      println(next.describe)
      next.applyEffects()
      Move(state, action, next)
    }
    def spells: List[Spell] = this match {
      case _: InitialState => Nil
      case Move(prev, action: Spell, _) => action :: prev.spells
      case Move(prev, _, _) => prev.spells
    }
    def traverse(f: (State, () => Unit) => Boolean): Unit =
      turn.possibleActions.foreach { action =>
        val next = apply(action)
        if (! f(next, {() => next.traverse(f)})) return
      }
    def withEffect(e: Effect): State = { turn.addEffect(e); this }
  }
  case class InitialState(turn: Turn) extends State
  case class Move(previous: State, action: Action, turn: Turn) extends State

  case object WizardSimulator extends Puzzle[Boss, Int] {

    override def parse(input: String): Boss = input.split(" ").toList match {
      case List(hp, d) => new Boss(hp.toInt, d.toInt)
    }

    private def findMinimalMana(game: State) = {
      var bestSolution = Int.MaxValue
      game.traverse { (state, next) =>
        state.turn.result match {
          case Some(_: Player) =>
            val spells = state.spells.reverse
            if (state.spentMana < bestSolution) {
              printf("[%s] %10d %s\n", LocalTime.now(), state.spentMana, spells.map { _.name.charAt(0) }.mkString)
              bestSolution = state.spentMana
            }
          case None if state.spentMana < bestSolution => next()
          case _ =>
        }
        state.spentMana < bestSolution
      }
      bestSolution
    }

    override def part1(boss: Boss): Int = {
      findMinimalMana(new Player(hitPoints = 50, mana = 500) --> boss)
    }

    override def part2(boss: Boss): Int = {
      findMinimalMana(new Player(hitPoints = 50, mana = 500) --> boss withEffect Hell)
    }
  }

}

object Day22_Solution extends Test(Day22.WizardSimulator) {

  import Day22._

  assert("First fight") {
    val game = new Player(hitPoints = 10, mana = 250) ->
      new Boss(hitPoints = 13, damage = 8)
    game
      .describe(
        Poison,
        PhysicalAttack,
        MagicMissile
      )
      .turn.result.get.isInstanceOf[Player]
  }
  assert("Second fight") {
    (new Player(hitPoints = 10, mana = 250) ->
      new Boss(hitPoints = 14, damage = 8))
      .describe(
        Recharge,
        PhysicalAttack,
        Shield,
        PhysicalAttack,
        Drain,
        PhysicalAttack,
        Poison,
        PhysicalAttack,
        MagicMissile
      )
      .turn.result.get.isInstanceOf[Player]
  }

  Part1 solve "58 9"
  Part2 solve "58 9"
}