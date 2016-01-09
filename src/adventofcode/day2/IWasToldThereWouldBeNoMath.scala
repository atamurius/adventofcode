package adventofcode.day2

import adventofcode.{Test, Puzzle}

/* --- Day 2: I Was Told There Would Be No Math ---
 * http://adventofcode.com/day/2
 *
 * The elves are running low on wrapping paper, and so they need to submit an order for more.
 * They have a list of the dimensions (length l, width w, and height h) of each present,
 * and only want to order exactly as much as they need.
 *
 * Fortunately, every present is a box (a perfect right rectangular prism),
 * which makes calculating the required wrapping paper for each gift a little easier:
 * find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l.
 * The elves also need a little extra paper for each present: the area of the smallest side.
 *
 * Part 1:
 * All numbers in the elves' list are in feet. How many total square feet of wrapping paper should they order?
 *
 * Part 2:
 * The elves are also running low on ribbon. Ribbon is all the same width, so they only have
 * to worry about the length they need to order, which they would again like to be exact.
 *
 * The ribbon required to wrap a present is the shortest distance around its sides,
 * or the smallest perimeter of any one face. Each present also requires a bow made out
 * of ribbon as well; the feet of ribbon required for the perfect bow is equal to the cubic
 * feet of volume of the present. Don't ask how they tie the bow, though; they'll never tell.
 *
 * How many total feet of ribbon should they order?
 */

case class Box(l: Int, w: Int, h: Int) {
  def surface = 2*l*w + 2*w*h + 2*h*l

  def paper = surface + List(l*w, w*h, h*l).min

  def ribbon = List(l+w,w+h,h+l).min * 2 + l*w*h
}

case object IWasToldThereWouldBeNoMath extends Puzzle[Seq[Box],Int] {

  def parse(input: String): Seq[Box] = {
    val box = """(\d+)x(\d+)x(\d+)""".r
    input split "\n" map {
      case box(a, b, c) => Box(a.toInt, b.toInt, c.toInt)
    }
  }

  def part1(boxes: Seq[Box]) = (boxes map {_.paper}).sum

  def part2(boxes: Seq[Box]) = (boxes map {_.ribbon}).sum
}

object Solution extends Test(IWasToldThereWouldBeNoMath) {
  Part1 on "2x3x4" gives 58
  Part1 on "1x1x10" gives 43

  Part2 on "2x3x4" gives 34
  Part2 on "1x1x10" gives 14

  Part2 solveFrom "input.txt"
}