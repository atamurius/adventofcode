package adventofcode.day14

import adventofcode.{Test, Puzzle}

/** --- Day 14: Reindeer Olympics ---
  *
  * This year is the Reindeer Olympics! Reindeer can fly at high speeds, but must rest occasionally to
  * recover their energy. Santa would like to know which of his reindeer is fastest, and so he has them race.
  *
  * Reindeer can only either be flying (always at their top speed) or resting (not moving at all),
  * and always spend whole seconds in either state.
  *
  * Part 1:
  * Given the descriptions of each reindeer (in your puzzle input), after exactly 2503 seconds,
  * what distance has the winning reindeer traveled?
  *
  * Part 2:
  * Instead, at the end of each second, he awards one point to the reindeer currently in the lead.
  * (If there are multiple reindeer tied for the lead, they each get one point.)
  * He keeps the traditional 2503 second time limit, of course, as doing otherwise would be entirely ridiculous.
  */

case class Reindeer(name: String, speed: Int, time: Int, rest: Int) {
  var position = 0
  var timeLeft = time
  var resting = false
  var points = 0
  def tick(t: Int): Unit = {
    timeLeft -= t
    if (! resting)
      position += t * speed
    if (timeLeft == 0) {
      resting = ! resting
      timeLeft = if (resting) rest else time
    }
  }
}

case object ReindeerOlympics extends Puzzle[Seq[Reindeer],Int]{

  def parse(input: String) = {
    val reindeer = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds.""".r
    input split "\n" map {_.trim match {
      case reindeer(name, speed, time, rest) => Reindeer(name, speed.toInt, time.toInt, rest.toInt)
    }}
  }

  def part2(input: Seq[Reindeer]) = {
    for (t <- 1 to time) {
      input foreach {_ tick 1}
      val max = (input map {_.position}).max
      input filter {_.position == max} foreach {_.points += 1}
    }
    input map {_.points} max
  }

  def run(time: Int)(implicit rs: Seq[Reindeer]): Seq[Reindeer] =
    if (time == 0) rs
    else {
      val t = Math.min(time, rs map {_.timeLeft} min)
      rs foreach {_ tick t}
      run(time - t)
    }

  var time = 0

  def part1(input: Seq[Reindeer]) = run(time)(input) map {_.position} max
}

object Solution extends Test(ReindeerOlympics) {

  puzzle.time = 1000
  Part1 on """Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
              Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.""" gives 1120

  puzzle.time = 2503
  Part1 solve """Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
                 Blitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds.
                 Rudolph can fly 20 km/s for 7 seconds, but then must rest for 132 seconds.
                 Cupid can fly 12 km/s for 4 seconds, but then must rest for 43 seconds.
                 Donner can fly 9 km/s for 5 seconds, but then must rest for 38 seconds.
                 Dasher can fly 10 km/s for 4 seconds, but then must rest for 37 seconds.
                 Comet can fly 3 km/s for 37 seconds, but then must rest for 76 seconds.
                 Prancer can fly 9 km/s for 12 seconds, but then must rest for 97 seconds.
                 Dancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds."""

  Part2 solve """Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
                 Blitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds.
                 Rudolph can fly 20 km/s for 7 seconds, but then must rest for 132 seconds.
                 Cupid can fly 12 km/s for 4 seconds, but then must rest for 43 seconds.
                 Donner can fly 9 km/s for 5 seconds, but then must rest for 38 seconds.
                 Dasher can fly 10 km/s for 4 seconds, but then must rest for 37 seconds.
                 Comet can fly 3 km/s for 37 seconds, but then must rest for 76 seconds.
                 Prancer can fly 9 km/s for 12 seconds, but then must rest for 97 seconds.
                 Dancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds."""
}
