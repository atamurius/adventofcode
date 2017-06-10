package adventofcode

import adventofcode.common.{Puzzle, Test}

/** --- Day 21: RPG Simulator 20XX ---
  * In this game, the player (you) and the enemy (the boss) take turns attacking.
  * The player always goes first. Each attack reduces the opponent's hit points by at least 1.
  * The first character at or below 0 hit points loses.
  *
  * Damage dealt by an attacker each turn is equal to the attacker's damage score minus the
  * defender's armor score. An attacker always does at least 1 damage.
  * So, if the attacker has a damage score of 8, and the defender has an armor score of 3,
  * the defender loses 5 hit points. If the defender had an armor score of 300,
  * the defender would still lose 1 hit point.
  *
  * Your damage score and armor score both start at zero.
  * They can be increased by buying items in exchange for gold.
  * You start with no items and have as much gold as you need.
  * Your total damage or armor is equal to the sum of those stats from all of your items.
  * You have 100 hit points.
  *
  * You must buy exactly one weapon; no dual-wielding.
  * Armor is optional, but you can't use more than one.
  * You can buy 0-2 rings (at most one for each hand).
  * You must use any items you buy.
  * The shop only has one of each item, so you can't buy, for example, two rings of Damage +3.
  *
  * You have 100 hit points. The boss's actual stats are in your puzzle input.
  * What is the least amount of gold you can spend and still win the fight?
  */

object Day21 {

  case class Character(name: String,
                       hitPoints: Int = 100,
                       equipment: List[Item] = Nil,
                       armor: Int = 0,
                       damage: Int = 0) {
    def isAlive: Boolean = hitPoints > 0

    def armorScore: Int = armor + equipment.iterator.map { _.armor }.sum

    def damageScore: Int = damage + equipment.iterator.map { _.damage }.sum

    def attackedBy(attacker: Character): Character = copy(
      hitPoints = hitPoints - (1 max (attacker.damageScore - armorScore))
    )

    override def toString = s"$name ($hitPoints)"

    override def equals(obj: scala.Any): Boolean = obj != null && obj.isInstanceOf[Character] &&
      obj.asInstanceOf[Character].name == name

    override def hashCode(): Int = name.hashCode
  }

  case class Item(name: String,
                  cost: Int,
                  damage: Int = 0,
                  armor: Int = 0)

  val weapons = List(
    Item("Dagger", 8, damage = 4),
    Item("Shortsword", 10, damage = 5),
    Item("Warhammer", 25, damage = 6),
    Item("Longsword", 40, damage = 7),
    Item("Greataxe", 74, damage = 8))

  val armors = List(
    Item("Leather", 13, armor = 1),
    Item("Chainmail", 31, armor = 2),
    Item("Splintmail", 53, armor = 3),
    Item("Bandedmail", 75, armor = 4),
    Item("Platemail", 102, armor = 5))

  val rings = List(
    Item("Damage +1", 25, 1, 0),
    Item("Damage +2", 50, 2, 0),
    Item("Damage +3", 100, 3, 0),
    Item("Defense +1", 20, 0, 1),
    Item("Defense +2", 40, 0, 2),
    Item("Defense +3", 80, 0, 3))

  case class Turn(attacker: Character, opponent: Character) {
    def winner = if (attacker.isAlive) None else Some(opponent)
    def next = Turn(opponent attackedBy attacker, attacker)

    override def toString = s"$attacker ⚔️ ${attacker.damageScore} ➡️ $opponent 🛡️ ${opponent.armorScore}"
  }

  case object RolePlayingGame extends Puzzle[Character, Int] {

    def fight(player: Character, boss: Character, trace: Boolean = false): Character = {
      var turn = Turn(player, boss)
      while (true) {
        if (trace) println(turn)
        turn.winner match {
          case Some(winner) => return winner
          case None =>
            turn = turn.next
        }
      }
      throw new InternalError("Impossible!")
    }

    override def parse(input: String): Character = input.split("""\D+""").toList match {
      case List(hp, damage, armor) => Character("Boss",
        hitPoints = hp.toInt,
        damage = damage.toInt,
        armor = armor.toInt)
    }

    override def part1(boss: Character): Int = {
      var best = Int.MaxValue
      for {
        weapon <- weapons sortBy { _.cost }
        armor <- None :: (armors sortBy { _.cost } map { Some(_) })
        leftRing <- None :: (rings sortBy { _.cost } map { Some(_) })
        rightRing <- None :: (rings sortBy { _.cost } map { Some(_) }) if leftRing.isEmpty || leftRing != rightRing
      } {
        val set = weapon :: List(armor, leftRing, rightRing).flatten
        val cost = set.map { _.cost }.sum
        if (best > cost && fight(Character("Player", equipment = set), boss).name == "Player") {
          println(s"New best set ($$$cost): ${set.map { _.name }}")
          best = cost
        }
      }
      best
    }

    override def part2(boss: Character): Int = {
      var worth = 0
      for {
        weapon <- weapons sortBy { -_.cost }
        armor <- None :: (armors sortBy { -_.cost } map { Some(_) })
        leftRing <- None :: (rings sortBy { -_.cost } map { Some(_) })
        rightRing <- None :: (rings sortBy { -_.cost } map { Some(_) }) if leftRing.isEmpty || leftRing != rightRing
      } {
        val set = weapon :: List(armor, leftRing, rightRing).flatten
        val cost = set.map { _.cost }.sum
        if (worth < cost && fight(Character("Player", equipment = set), boss).name != "Player") {
          println(s"New worth set ($$$cost): ${set.map { _.name }}")
          worth = cost
        }
      }
      worth
    }
  }
}

object Day21_Solution extends Test(Day21.RolePlayingGame) {

  import Day21._

  assert("Player should win") {
    puzzle.fight(
      Character("Player", hitPoints = 8, damage = 5, armor = 5),
      Character("Boss  ", hitPoints = 12, damage = 7, armor = 2),
      trace = true
    ).name == "Player"
  }

  Part1 solve "104 8 1"
  Part2 solve "104 8 1"
}