package adventofcode.day4

import java.security.MessageDigest
import adventofcode.{Test, Puzzle}

case object TheIdealStockingStuffer extends Puzzle.Simple[Int] {

  val md5Digest = MessageDigest getInstance "MD5"

  def toHexString(n: Int) = {
    val s = Integer toHexString n
    if (s.length < 2) '0' + s
    else s
  }

  def md5(salt: String, n: Int) = md5Digest digest (salt + n).getBytes take 3 map {toHexString(_)} mkString ""

  def part1(input: String): Int = (Stream from 1 map {x => (x, md5(input,x))} find {_._2.startsWith("00000")}).get._1

  def part2(input: String): Int = (Stream from 1 map {x => (x, md5(input,x))} find {_._2.startsWith("000000")}).get._1
}

object Solution extends Test(TheIdealStockingStuffer) {

  Part1 on "abcdef" gives 609043
  Part1 on "pqrstuv" gives 1048970

  Part2 solve "ckczppom"
}