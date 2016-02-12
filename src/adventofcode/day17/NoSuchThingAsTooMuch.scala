package adventofcode.day17

import adventofcode.{Test, Puzzle}

/** --- Day 17: No Such Thing as Too Much ---
  *
  * The elves bought too much eggnog again - 150 liters this time.
  * To fit it all into your refrigerator, you'll need to move it into smaller containers.
  * You take an inventory of the capacities of the available containers.
  *
  * Part 1:
  * Filling all containers entirely,
  * how many different combinations of containers can exactly fit all 150 liters of eggnog?
  *
  * Part 2:
  * While playing with all the containers in the kitchen, another load of eggnog arrives!
  * The shipping and receiving department is requesting as many containers as you can spare.
  *
  * Find the minimum number of containers that can exactly fit all 150 liters of eggnog.
  * How many different ways can you fill that number of containers and still hold exactly 150 litres?
  */
case object NoSuchThingAsTooMuch extends Puzzle[List[Int], Int] {

  def parse(input: String) = input split """\D+""" map Integer.parseInt toList

  var total = 0

  def sliding(vs: List[Int]): Seq[(Int,List[Int])] = vs.tails.toSeq.init map {t => t.head -> t.tail}

  def combinations(target: Int)(vs: List[Int]): Seq[List[Int]] =
    sliding(vs) flatMap {
      case (v, rest) if v == target => Seq(List(v))
      case (v, rest) if v < target => combinations(target - v)(rest) map {v :: _}
      case _ => Nil
    }

  def part1(input: List[Int]) = combinations(total)(input).size

  def part2(input: List[Int]) = {
    val combos = combinations(total)(input)
    val min = (combos map {_.size}).min
    combos count {_.size == min}
  }
}

object Solution extends Test(NoSuchThingAsTooMuch) {

  Test(puzzle.sliding) labeled "sliding" on List(20, 15, 10, 5, 5) gives Seq(
    20 -> List(15, 10, 5, 5),
    15 -> List(10, 5, 5),
    10 -> List(5, 5),
     5 -> List(5),
     5 -> List())

  Test(puzzle.combinations(25)) labeled "combinations" on List(20, 15, 10, 5, 5) gives Seq(
    List(20, 5),
    List(20, 5),
    List(15, 10),
    List(15, 5, 5))

  puzzle.total = 150
  Part1 solve "50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40"

  Part2 solve "50 44 11 49 42 46 18 32 26 40 21 7 18 43 10 47 36 24 22 40"
}