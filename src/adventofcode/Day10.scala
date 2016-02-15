package adventofcode

import adventofcode.common.{Puzzle, Test}

import scala.annotation.tailrec

/** --- Day 10: Elves Look, Elves Say ---
  *
  * Today, the Elves are playing a game called look-and-say.
  * They take turns making sequences by reading aloud the previous sequence and using that reading as the next sequence.
  * For example, 211 is read as "one two, two ones", which becomes 1221 (1 2, 2 1s).
  *
  * Look-and-say sequences are generated iteratively, using the previous value as input for the next step.
  * For each step, take the previous value, and replace each run of digits (like 111) with the number
  * of digits (3) followed by the digit itself (1).
  *
  * Part 1:
  * Starting with the digits in your puzzle input, apply this process 40 times. What is the length of the result?
  *
  * Part 2:
  * Now, starting again with the digits in your puzzle input, apply this process 50 times. What is the length of the new result?
  */
object Day10 {
  case object ElvesLookElvesSay extends Puzzle[(List[Char],Int),Int] {

    override def parse(input: String): (List[Char], Int) =
      (input split ",").toSeq match {case Seq(str,num) => (str.toList, num.toInt)}

    def nextString(s: String): String = next(s.toList).mkString

    def next(s: List[Char]) = {
      def num(n: Int) = n.toString.toList.reverse
      @tailrec
      def traverse(s: List[Char], n: Int, res: List[Char]): List[Char] = s match {
        case Nil => res.reverse
        case a :: b :: rest if a == b => traverse(s.tail, n+1, res)
        case a :: rest => traverse(rest, 1, a :: num(n) ++ res)
      }
      traverse(s, 1, Nil)
    }

    def part1(input: (List[Char],Int)): Int = input match {
      case (value, 0) => value.length
      case (value, n) => part1(next(value), n-1)
    }

    def part2(input: (List[Char],Int)): Int = part1(input)
  }

  object Solution extends Test(ElvesLookElvesSay) {

    Test(ElvesLookElvesSay.nextString) labeled "next" forall (
      "1" -> "11",
      "11" -> "21",
      "21" -> "1211",
      "1211" -> "111221",
      "111221" -> "312211")

    Part1 solve "1321131112,40"

    Part2 solve "1321131112,50"
  }
}
