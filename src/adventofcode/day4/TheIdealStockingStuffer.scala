package adventofcode.day4

import java.security.MessageDigest
import adventofcode.{Test, Puzzle}

/*
 * Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts
 * for all the economically forward-thinking little girls and boys.
 *
 * Part 1:
 * To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes.
 * The input to the MD5 hash is some secret key (your puzzle input, given below)
 * followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive
 * number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
 *
 * Part 2:
 * Now find one that starts with six zeroes.
 */

case object TheIdealStockingStuffer extends Puzzle.Simple[Int] {

  val md5Digest = MessageDigest getInstance "MD5"

  def md5(salt: String, n: Int) = md5Digest digest (salt + n).getBytes

  def find(salt: String, good: Array[Byte] => Boolean): Int = {
    var n = 1
    while (true) {
      val s = md5(salt, n)
      if (good(s)) {
        return n
      }
      n += 1
    }
    0
  }

  // brutforce
  def part1(input: String): Int = find(input, { bs =>
    bs(0) == 0 && bs(1) == 0 && (0xff & bs(2)) < 0x10 // hex 00.00.0*
  })

  def part2(input: String): Int = find(input, { bs =>
    bs(0) == 0 && bs(1) == 0 && bs(2) == 0 // hex 00.00.00
  })
}

object Solution extends Test(TheIdealStockingStuffer) {

  Part1 on "abcdef" gives 609043
  Part1 on "pqrstuv" gives 1048970

  Part2 solve "ckczppom"
}