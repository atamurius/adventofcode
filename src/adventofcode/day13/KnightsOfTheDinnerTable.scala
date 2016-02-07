package adventofcode.day13

import adventofcode.{Test, Puzzle}

/** --- Day 13: Knights of the Dinner Table ---
  *
  * In years past, the holiday feast with your family hasn't gone so well.
  * Not everyone gets along! This year, you resolve, will be different.
  * You're going to find the optimal seating arrangement and avoid all those awkward conversations.
  *
  * You start by writing up a list of everyone invited and the amount their happiness would increase
  * or decrease if they were to find themselves sitting next to each other person.
  * You have a circular table that will be just big enough to fit everyone comfortably,
  * and so each person will have exactly two neighbors.
  *
  * Part 1:
  * What is the total change in happiness for the optimal seating arrangement of the actual guest list?
  */

case class Rule(person: String, nextTo: String, delta: Int) {
  def applied(first: String, second: String) =
    person == first && nextTo == second || person == second && nextTo == first
}

case object KnightsOfTheDinnerTable extends Puzzle[Seq[Rule],Int] {

  def parse(input: String) = {
    val rule = """(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+).""".r
    input split "\n" map {_.trim match {
      case rule(person, "lose", value, next) => Rule(person, next, - value.toInt)
      case rule(person, "gain", value, next) => Rule(person, next, + value.toInt)
    }}
  }

  def total(rules: Seq[Rule])(persons: List[String]) =
    (persons.last :: persons).sliding(2) flatMap {
      case List(a,b) => rules filter {_.applied(a,b)}
    } map {_.delta} sum

  def personsFrom(rules: Seq[Rule]) = (rules map {_.person}).toSet.toList

  def part1(input: Seq[Rule]) = personsFrom(input).permutations map total(input) max

  def part2(input: Seq[Rule]) = ("I" :: personsFrom(input)).permutations map total(input) max
}

object Solution extends Test(KnightsOfTheDinnerTable) {
  Part1 on """Alice would gain 54 happiness units by sitting next to Bob.
              Alice would lose 79 happiness units by sitting next to Carol.
              Alice would lose 2 happiness units by sitting next to David.
              Bob would gain 83 happiness units by sitting next to Alice.
              Bob would lose 7 happiness units by sitting next to Carol.
              Bob would lose 63 happiness units by sitting next to David.
              Carol would lose 62 happiness units by sitting next to Alice.
              Carol would gain 60 happiness units by sitting next to Bob.
              Carol would gain 55 happiness units by sitting next to David.
              David would gain 46 happiness units by sitting next to Alice.
              David would lose 7 happiness units by sitting next to Bob.
              David would gain 41 happiness units by sitting next to Carol.""" gives 330

  Part1 solveFrom "input.txt"

  Part2 solveFrom "input.txt"
}