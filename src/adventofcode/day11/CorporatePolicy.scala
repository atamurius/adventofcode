package adventofcode.day11

import adventofcode.{Test, Puzzle}

/** --- Day 11: Corporate Policy ---
  * http://adventofcode.com/day/11
  *
  * Santa's previous password expired, and he needs help choosing a new one.
  *
  * To help him remember his new password after the old one expires, Santa has devised a method of coming up
  * with a password based on the previous one. Corporate policy dictates that passwords must be exactly eight
  * lowercase letters (for security reasons), so he finds his new password by incrementing his old password
  * string repeatedly until it is valid.
  *
  * Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on.
  * Increase the rightmost letter one step; if it was z, it wraps around to a, and repeat with the next
  * letter to the left until one doesn't wrap around.
  *
  * Unfortunately for Santa, a new Security-Elf recently started, and he has imposed some additional password requirements:
  *
  *     - Passwords must include one increasing straight of at least three letters,
  *       like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
  *     - Passwords may not contain the letters i, o, or l, as these letters can be mistaken for
  *       other characters and are therefore confusing.
  *     - Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.
  *
  * Part 1:
  * Given Santa's current password (your puzzle input), what should his next password be?
  */
case object CorporatePolicy extends Puzzle.Simple[String] {

  def next(s: Array[Char]): Unit = {
    var o = 1
    for (i <- s.indices.reverse) {
      val v = s(i) - 'a' + o
      s(i) = (v % ('z' - 'a' + 1) + 'a').asInstanceOf[Char]
      o = v / ('z' - 'a' + 1)
    }
  }

  def nextString(s: String): String = { val a = s.toCharArray; next(a); new String(a) }

  def strait(s: Array[Char]) = s sliding 3 exists {
    case Array(a,b,c) => a + 1 == b && b + 1 == c
  }

  def forbidden(s: Array[Char]) = s exists {"iol" contains _}

  def pairs(s: List[Char]): Int = s match {
    case a :: b :: rest if a == b => 1 + pairs(rest)
    case Nil => 0
    case a :: rest => pairs(rest)
  }

  def part1(input: String) = {
    val pass = input.toCharArray
    do next(pass)
    while (! strait(pass) || forbidden(pass) || pairs(pass.toList) < 2)
    new String(pass)
  }

  def part2(input: String) = part1(input)
}

object Solution extends Test(CorporatePolicy) {

  Test(CorporatePolicy.nextString) labeled "next" forall (
    "aaa" -> "aab",
    "aaz" -> "aba",
    "azz" -> "baa")

  Part1 on "abcdefgh" gives "abcdffaa"
  Part1 on "ghijklmn" gives "ghjaabcc"

  Part1 solve "hxbxwxba"
  Part2 solve "hxbxxyzz"
}