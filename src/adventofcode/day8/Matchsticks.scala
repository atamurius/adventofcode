package adventofcode.day8

import adventofcode.{Test, Puzzle}

import scala.annotation.tailrec

/* --- Day 8: Matchsticks ---
 * http://adventofcode.com/day/8
 *
 * Space on the sleigh is limited this year, and so Santa will be bringing his
 * list as a digital copy. He needs to know how much space it will take up when stored.
 *
 * It is common in many programming languages to provide a way to escape special characters
 * in strings. For example, C, JavaScript, Perl, Python, and even PHP handle special
 * characters in very similar ways.
 *
 * However, it is important to realize the difference between the number of characters in
 * the code representation of the string literal and the number of characters in the in-memory string itself.
 *
 * Part 1:
 * Santa's list is a file that contains many double-quoted string literals, one on each line.
 * The only escape sequences used are \\ (which represents a single backslash), \" (which represents
 * a lone double-quote character), and \x plus two hexadecimal characters
 * (which represents a single character with that ASCII code).
 *
 * Disregarding the whitespace in the file, what is the number of characters of code
 * for string literals minus the number of characters in memory for the values of the
 * strings in total for the entire file?
 *
 * Part 2:
 * Your task is to find the total number of characters to represent the newly encoded strings
 * minus the number of characters of code in each original string literal.
 */
case object Matchsticks extends Puzzle[Seq[String],Int] {
  def parse(input: String) = input split "\n"

  @tailrec
  def decodedLength(acc: Int, str: List[Char]): Int = str match {
    case Nil => acc
    case '\\' :: 'x' :: _ :: _  :: rest => decodedLength(acc + 1, rest)
    case '\\' :: _              :: rest => decodedLength(acc + 1, rest)
    case _                      :: rest => decodedLength(acc + 1, rest)
  }

  def decoded(s: String) = decodedLength(0, s.substring(1,s.length-1).toList)

  @tailrec
  def encodedLength(acc: Int, str: List[Char]): Int = str match {
    case Nil => acc + 2
    case '\\' :: rest => encodedLength(acc + 2, rest)
    case '"'  :: rest => encodedLength(acc + 2, rest)
    case _    :: rest => encodedLength(acc + 1, rest)
  }

  def encoded(s: String) = encodedLength(0, s.toList)

  def part1(input: Seq[String]) = (input map {s => s.length - decoded(s)}).sum

  def part2(input: Seq[String]) = (input map {s => encoded(s) - s.length}).sum
}

object Solution extends Test(Matchsticks) {
  Part1 on """""""" gives 2
  Part1 on """"abc"""" gives 2
  Part1 on """"aaa\"aaa"""" gives 3
  Part1 on """"\x27"""" gives 5

  Part2 on """""""" gives 4
  Part2 on """"abc"""" gives 4
  Part2 on """"aaa\"aaa"""" gives 6
  Part2 on """"\x27"""" gives 5

  Part2 solveFrom "input.txt"
}