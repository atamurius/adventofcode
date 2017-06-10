package adventofcode

import adventofcode.common.{Puzzle, Test}

import scala.annotation.tailrec

/** --- Day 20: Infinite Elves and Infinite Houses ---
  *
  * To keep the Elves busy, Santa has them deliver some presents by hand, door-to-door.
  * He sends them down a street with infinite houses numbered sequentially: 1, 2, 3, 4, 5, and so on.
  *
  * Each Elf is assigned a number, too, and delivers presents to houses based on that number.
  *
  * There are infinitely many Elves, numbered starting with 1.
  * Each Elf delivers presents equal to ten times his or her number at each house.
  *
  * What is the lowest house number of the house to get at least as many presents as the number in your puzzle input?
  *
  * Part 2
  * The Elves decide they don't want to visit an infinite number of houses.
  * Instead, each Elf will stop after delivering presents to 50 houses.
  * To make up for it, they decide to deliver presents equal to eleven times their number at each house.
  */

object Day20 {

  case object InfiniteElves extends Puzzle[Long, Long] {

    override def parse(input: String): Long = input.toLong

    lazy val primes: Stream[Long] = 1L #:: 2L #:: Stream.from(3, 2).filter { n =>
      primes.drop(1).takeWhile { p => p*p <= n }.forall { n % _ != 0 }
    }.map { _.toLong }

    def products(xs: List[Long]): List[Long] = xs match {
      case Nil => Nil
      case x :: rest =>
        val ps = products(rest)
        x :: ps ++ ps.map { x * _ }
    }

    def peeks: Stream[(Long, Long)] = Stream.from(0).map { i =>
      (primes.take(i + 1).product, products(primes.slice(1, i + 1).toList).sum + 1)
    }

    def peeks2: Stream[(Long, Long)] = Stream.from(0).map { i =>
      val n = primes.take(i + 1).product
      (n, score2(n))
    }

    def factors(n: Long): List[Long] = {
      var ps = primes.drop(1)
      var x = n
      var xs = List[Long]()
      while (x > 1 && ps.head <= n) {
        if (x % ps.head == 0) {
          x /= ps.head
          xs ::= ps.head
        } else {
          ps = ps.tail
        }
      }
      xs
    }

    def score(n: Long): Long = products(factors(n)).toSet.sum + 1

    def score2(n: Long): Long = products(factors(n)).toSet.iterator.filter { n / _ <= 50 }.sum + 1

    override def part1(input: Long): Long = {
      val sum = input / 10
      var start = peeks.takeWhile { _._2 <= sum }.last._1
      println(s"Start: $start")
      while (score(start) < sum) {
        printf("%-10s %s\r", start, List('-','/','|','\\')((start % 4).toInt))
        start += 1
      }
      start
    }

    override def part2(input: Long): Long = {
      val sum = input / 11
      var start = peeks2.takeWhile { _._2 <= sum }.last._1
      println(s"Start: $start")
      while (score2(start) < sum) {
        printf("%-10s %s\r", start, List('-','/','|','\\')((start % 4).toInt))
        start += 1
      }
      start
    }
  }
}

object Day20_Solution extends Test(Day20.InfiniteElves) {

  def products(args: List[Long]) = puzzle.products(args).sorted

  Test(products) labeled "products" forall (
    Nil -> Nil,
    List(2L) -> List(2L),
    List(2L,3L) -> List(2L,3L,6L),
    List(2L,3L,5L) -> List(2L,3L,5L,6L,10L,15L,30L)
  )

  Test(puzzle.score) labeled "score" forall (
    1L -> 1L, // 1
    2L -> 3L, // 1 * 2
    3L -> 4L,
    4L -> 7L,
    5L -> 6L,
    6L -> 12L, // 1 * 2 * 3
    7L -> 8L,
    8L -> 15L,
    9L -> 13L
  )

  Part1 on "140" gives 8

  Part1 solve "33100000"
  Part2 solve "33100000"
}

