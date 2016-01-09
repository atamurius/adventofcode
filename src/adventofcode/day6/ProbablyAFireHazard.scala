package adventofcode.day6

import adventofcode.{Test, Puzzle}

/* --- Day 6: Probably a Fire Hazard ---
 * http://adventofcode.com/day/6
 *
 * Because your neighbors keep defeating you in the holiday house decorating contest
 * year after year, you've decided to deploy one million lights in a 1000x1000 grid.
 *
 * Furthermore, because you've been especially nice this year, Santa has mailed you
 * instructions on how to display the ideal lighting configuration.
 *
 * Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner
 * are at 0,0, 0,999, 999,999, and 999,0.
 *
 * Part 1:
 * The instructions include whether to turn on, turn off, or toggle various inclusive ranges
 * given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle,
 * inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square.
 * The lights all start turned off.
 *
 * To defeat your neighbors this year, all you have to do is set up your lights by
 * doing the instructions Santa sent you in order.
 *
 * Part 2:
 * You just finish implementing your winning light pattern when you realize you mistranslated Santa's
 * message from Ancient Nordic Elvish.
 *
 * The light grid you bought actually has individual brightness controls;
 * each light can have a brightness of zero or more. The lights all start at zero.
 *
 * The phrase turn on actually means that you should increase the brightness of those lights by 1.
 *
 * The phrase turn off actually means that you should decrease the brightness of those lights by 1,
 * to a minimum of zero.
 *
 * The phrase toggle actually means that you should increase the brightness of those lights by 2.
 *
 * What is the total brightness of all lights combined after following Santa's instructions?
 */

case class
  Instruction( op: String,
               x1: Int,
               y1: Int,
               x2: Int,
               y2: Int) {

  import Math.{min,max}

  val left   = min(x1,x2)
  val top    = min(y1,y2)
  val right  = max(x1,x2)
  val bottom = max(y1,y2)

  def map(array: Array[Array[Int]])(f: ((Int,String)) => Int) = for {
    x <- left to right
    y <- top to bottom
  } array(y)(x) = f( array(y)(x), op )
}

case object ProbablyAFireHazard extends Puzzle[Seq[Instruction],Int] {
  val pattern = """^(turn (on|off)|toggle) (\d+),(\d+) through (\d+),(\d+)$""".r

  def parse(input: String) = input split "\n" map {
    case pattern(op, _,x1,y1,x2,y2) => Instruction(op, x1.toInt, y1.toInt, x2.toInt, y2.toInt)
  }

  def part1(input: Seq[Instruction]) = {
    val array = Array.ofDim[Int](1000,1000)
    input foreach {_.map(array) {
      case (_, "turn on")  => 1
      case (_, "turn off") => 0
      case (0, "toggle")   => 1
      case (1, "toggle")   => 0
    }}
    (array map {_ count {_ == 1}}).sum
  }

  def part2(input: Seq[Instruction]) = {
    val array = Array.ofDim[Int](1000,1000)
    input foreach {_.map(array) {
      case (x, "turn on")  => x + 1
      case (x, "turn off") => Math.max(0, x - 1)
      case (x, "toggle")   => x + 2
    }}
    (array map {_.sum}).sum
  }
}

object Solution extends Test(ProbablyAFireHazard) {
  Part2 solveFrom "input.txt"
}