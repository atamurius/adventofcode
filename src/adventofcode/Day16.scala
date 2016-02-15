package adventofcode

import adventofcode.common.{Test, Puzzle}

/** --- Day 16: Aunt Sue ---
  *
  * Your Aunt Sue has given you a wonderful gift, and you'd like to send her a thank you card.
  * However, there's a small problem: she signed it "From, Aunt Sue".
  *
  * You have 500 Aunts named "Sue".
  *
  * So, to avoid sending the card to the wrong person, you need to figure out which Aunt Sue
  * (which you conveniently number 1 to 500, for sanity) gave you the gift. You open the present and,
  * as luck would have it, good ol' Aunt Sue got you a My First Crime Scene Analysis Machine!
  * Just what you wanted. Or needed, as the case may be.
  *
  * The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few specific compounds in a given sample,
  * as well as how many distinct kinds of those compounds there are. According to the instructions,
  * these are what the MFCSAM can detect:
  *
  *   - children, by human DNA age analysis.
  *   - cats. It doesn't differentiate individual breeds.
  *   - Several seemingly random breeds of dog: samoyeds, pomeranians, akitas, and vizslas.
  *   - goldfish. No other kinds of fish.
  *   - trees, all in one group.
  *   - cars, presumably by exhaust or gasoline or something.
  *   - perfumes, which is handy, since many of your Aunts Sue wear a few kinds.
  *
  * In fact, many of your Aunts Sue have many of these. You put the wrapping from the gift into the MFCSAM.
  * It beeps inquisitively at you a few times and then prints out a message on ticker tape:
  *
  *   - children: 3
  *   - cats: 7
  *   - samoyeds: 2
  *   - pomeranians: 3
  *   - akitas: 0
  *   - vizslas: 0
  *   - goldfish: 5
  *   - trees: 3
  *   - cars: 2
  *   - perfumes: 1
  *
  * You make a list of the things you can remember about each Aunt Sue.
  * Things missing from your list aren't zero - you simply don't remember the value.
  *
  * Part 1:
  * What is the number of the Sue that got you the gift?
  *
  * Part 2:
  * As you're about to send the thank you note, something in the MFCSAM's instructions catches your eye.
  * Apparently, it has an outdated retroencabulator, and so the output from the machine isn't exact values -
  * some of them indicate ranges.
  *
  * In particular, the cats and trees readings indicates that there are greater than that many
  * (due to the unpredictable nuclear decay of cat dander and tree pollen),
  * while the pomeranians and goldfish readings indicate that there are fewer than that
  * many (due to the modial interaction of magnetoreluctance).
  *
  * What is the number of the real Aunt Sue?
  */
object Day16 {

  case class Aunt(name: String, props: Map[String,Int])

  case object AuntSue extends Puzzle[Seq[Aunt],String] {

    var facts = Map[String,Int]()

    def parse(input: String) = {
      val aunt = """\s*(\w+[^:]*): (.*\S)\s*""".r
      val fact = """(\w+): (\d+)""".r
      input split "[\n\r]+" map {
        case aunt(name, factsStr) =>
          val facts = fact findAllMatchIn factsStr map {m => (m group 1) -> (m group 2).toInt}
          Aunt(name, Map(facts.toSeq : _*))
        case other => throw new IllegalArgumentException(s"Parsing failed: '$other' is not valid aunt")
      }
    }

    def find(aunts: Seq[Aunt], facts: Map[String,Int => Boolean]) = (aunts filter {a =>
      facts forall {case (name,fact) => ! (a.props contains name) || fact(a.props(name))}
    }).toList match {
      case Nil => "Nothing"
      case List(Aunt(name,_)) => name
      case _ => "Undefined"
    }

    def eq(x: Int)(y: Int) = x == y
    def lt(x: Int)(y: Int) = x >  y
    def gt(x: Int)(y: Int) = x <  y

    def part1(input: Seq[Aunt]) = find(input, facts mapValues eq)

    def part2(input: Seq[Aunt]) = {
      val facts = this.facts map {
        case (t@"cats",        n) => (t, gt(n) _)
        case (t@"trees",       n) => (t, gt(n) _)
        case (t@"pomeranians", n) => (t, lt(n) _)
        case (t@"goldfish",    n) => (t, lt(n) _)
        case (t, n) => (t, eq(n) _)
      }
      find(input, facts)
    }
  }

  object Solution extends Test(AuntSue) {

    puzzle.facts = Map(
      "children" -> 3,
      "cats" -> 7,
      "samoyeds" -> 2,
      "pomeranians" -> 3,
      "akitas" -> 0,
      "vizslas" -> 0,
      "goldfish" -> 5,
      "trees" -> 3,
      "cars" -> 2,
      "perfumes" -> 1)

    Part1 solveFrom "Day16.txt"
    Part2 solveFrom "Day16.txt"
  }
}
