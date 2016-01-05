package adventofcode.day1

import adventofcode._

/*
 * Santa is trying to deliver presents in a large apartment building,
 * but he can't find the right floor - the directions he got are a little confusing.
 * He starts on the ground floor (floor 0) and then follows the instructions one character at a time.
 *
 * An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ),
 * means he should go down one floor.
 *
 * The apartment building is very tall, and the basement is very deep; he will never
 * find the top or bottom floors.
 *
 * Part 1:
 * To what floor do the instructions take Santa?
 *
 * Part 2:
 * Now, given the same instructions, find the position of the first character that causes
 * him to enter the basement (floor -1). The first character in the instructions has position 1,
 * the second character has position 2, and so on.
 */

case object NotQuiteLisp extends Puzzle[Seq[Int],Int] {
  def parse(input: String): Seq[Int] = input map {
    case '(' => +1
    case ')' => -1
  }
  def part1(input: Seq[Int]) = input.sum
  def part2(input: Seq[Int]) = (input scanLeft 0) {_ + _} indexOf -1
}

object Solution extends Test(NotQuiteLisp) {
  Part1 on "(())" gives 0
  Part1 on "()()" gives 0
  Part1 on "(((" gives 3
  Part1 on "(()(()(" gives 3
  Part1 on "))(((((" gives 3
  Part1 on "())" gives -1
  Part1 on "))(" gives -1
  Part1 on ")))" gives -3
  Part1 on ")())())" gives -3

  Part2 on ")" gives 1
  Part2 on "()())" gives 5

  Part2 solveFrom "input.txt"
}