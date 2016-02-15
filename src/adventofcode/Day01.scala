package adventofcode

import adventofcode.common.{Puzzle, Test}

/** --- Day 1: Not Quite Lisp ---
 *
 * Santa is trying to deliver presents in a large apartment building,
 * but he can't find the right floor - the directions he got are a little confusing.
 * He starts on the ground floor (floor 0) and then follows the instructions one character at a time.
 *
 * An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ),
 * means he should go down one floor.
 *
 * The apartment building is very tall, and the basement is very deep; he will never
 * find the top or bottom floors.
 */
object Day01 {

  case object NotQuiteLisp extends Puzzle[Seq[Int], Int] {
    def parse(input: String): Seq[Int] = input map {
      case '(' => +1
      case ')' => -1
    }

    /** To what floor do the instructions take Santa? */
    def part1(input: Seq[Int]) = input.sum

    /**
      * Now, given the same instructions, find the position of the first character that causes
      * him to enter the basement (floor -1). The first character in the instructions has position 1,
      * the second character has position 2, and so on.
      */
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

    Part1 solveFrom "Day01.txt"
    Part2 solveFrom "Day01.txt"
  }
}