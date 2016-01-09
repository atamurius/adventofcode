package adventofcode.day5

import adventofcode.{Test, Puzzle}

/* --- Day 5: Doesn't He Have Intern-Elves For This? ---
 * http://adventofcode.com/day/5
 *
 * Santa needs help figuring out which strings in his text file are naughty or nice.
 *
 * Part 1:
 * A nice string is one with all of the following properties:
 *
 *    - It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
 *    - It contains at least one letter that appears twice in a row,
 *      like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
 *    - It does not contain the strings ab, cd, pq, or xy,
 *      even if they are part of one of the other requirements.
 *
 * How many strings are nice?
 *
 * Part 2:
 * Now, a nice string is one with all of the following properties:
 *
 *    - It contains a pair of any two letters that appears at least twice
 *      in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa),
 *      but not like aaa (aa, but it overlaps).
 *    - It contains at least one letter which repeats with exactly one letter
 *      between them, like xyx, abcdefeghi (efe), or even aaa.
 *
 * How many strings are nice under these new rules?
 */

case object DoesntHeHaveInternElvesForThis extends Puzzle[Seq[String],Int] {
  def parse(input: String): Seq[String] = input split "\n"

  def containsVowels(string: String) = (string count {"aeiou" contains _}) >= 3

  def doubleLetter(string: String) = string matches """.*(.)\1.*"""

  def noBadStrings(string: String) = ! (string matches ".*(ab|cd|pq|xy).*")

  def nice(fs: (String => Boolean)*)(string: String) = fs forall {_(string)}

  def part1(input: Seq[String]): Int = input count nice(containsVowels,doubleLetter,noBadStrings)

  def doublePair(string: String) = string matches """.*(..).*\1.*"""

  def doubleLetterWithGap(string: String) = string matches """.*(.).\1.*"""

  def part2(input: Seq[String]): Int = input count nice(doublePair,doubleLetterWithGap)
}

object Solution extends Test(DoesntHeHaveInternElvesForThis) {
  Part1 on "ugknbfddgicrmopn" gives 1
  Part1 on "aaa" gives 1
  Part1 on "jchzalrnumimnmhp" gives 0
  Part1 on "haegwjzuvuyypxyu" gives 0
  Part1 on "dvszwmarrgswjxmb" gives 0

  Part2 on "qjhvhtzxzqqjkmpb" gives 1
  Part2 on "xxyxx" gives 1
  Part2 on "uurcxstgmygtbstg" gives 0
  Part2 on "ieodomkazucvgmuy" gives 0

  Part2 solveFrom "input.txt"
}