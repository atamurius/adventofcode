package adventofcode

import adventofcode.common.{Test, Puzzle}

import scala.annotation.tailrec

/** --- Day 18: Like a GIF For Your Yard ---
  *
  * After the million lights incident, the fire code has gotten stricter: now, at most ten thousand lights are allowed.
  * You arrange them in a 100x100 grid.
  *
  * Never one to let you down, Santa again mails you instructions on the ideal lighting configuration.
  * With so few lights, he says, you'll have to resort to animation.
  *
  * Start by setting your lights to the included initial configuration (your puzzle input).
  * A # means "on", and a . means "off".
  *
  * Then, animate your grid in steps, where each step decides the next configuration based on the current one.
  * Each light's next state (either on or off) depends on its current state and the current states of the eight
  * lights adjacent to it (including diagonals).
  * Lights on the edge of the grid might have fewer than eight neighbors; the missing ones always count as "off".
  *
  * The state a light should have next is based on its current state (on or off)
  * plus the number of neighbors that are on:
  *
  *   - A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
  *   - A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
  *
  * Part 1:
  * In your grid of 100x100 lights, given your initial configuration, how many lights are on after 100 steps?
  *
  * Part 2:
  * You flip the instructions over; Santa goes on to point out that this is all just an implementation
  * of Conway's Game of Life. At least, it was, until you notice that something's wrong with the grid of
  * lights you bought: four lights, one in each corner, are stuck on and can't be turned off.
  */
object Day18 {

  import scala.collection.mutable.ArrayBuffer

  class Screen(val values: ArrayBuffer[ArrayBuffer[Boolean]]) {

    val buffer = values map {_.clone}

    def this(width: Int, height: Int) = this(ArrayBuffer.fill(width, height)(false))

    def this(values: Seq[Seq[Boolean]]) = this(ArrayBuffer( values map {line => ArrayBuffer(line : _*)} : _* ))

    val width = values.size

    val height = values.headOption map {_.size} getOrElse 0

    def apply(x: Int, y: Int) =
      if ((values isDefinedAt x) && (values(x) isDefinedAt y)) values(x)(y)
      else false

    def neighbors(x: Int, y: Int) = (
        for {
          dx <- -1 to 1
          dy <- -1 to 1
          if dx != 0 || dy != 0
        } yield apply(x + dx, y + dy)
      ) count identity

    def mutate(f: (Boolean,Int) => Boolean) = {
      for {
        x <- 0 until width
        y <- 0 until height
      } {
        buffer(x)(y) = f(apply(x,y), neighbors(x, y))
      }
      for {
        x <- 0 until width
        y <- 0 until height
      } {
        values(x)(y) = buffer(x)(y)
      }
      this
    }

    def print(): Unit = {
      println((0 until width) map {x => (0 until height) map {y => if (apply(x,y)) '#' else '.'} mkString} mkString "\n")
      println()
    }

    def countOn = (for (x <- 0 until width; y <- 0 until height) yield if (apply(x,y)) 1 else 0).sum
  }

  class BrokenScreen(values: ArrayBuffer[ArrayBuffer[Boolean]]) extends Screen(values) {
    var broken = Map[(Int,Int),Boolean]()
    def + (p: (Int,Int,Boolean)) = {
      p match {
        case (x,y,value) => broken += (((x + width) % width, (y + height) % height) -> value)
      }
      this
    }
    override def apply(x: Int, y: Int) = broken.getOrElse((x,y), super.apply(x,y))
  }

  case object LikeAGifForYourYard extends Puzzle[(Screen,Int),Int] {

    def parse(input: String) = (input split "\n").toList match {
      case first :: rest => new Screen(rest map {_.trim map {_ == '#'}}) -> first.trim.toInt}

    @tailrec
    def iterate(screen: Screen, count: Int)(f: (Boolean, Int) => Boolean): Screen =
     if (count == 0) screen
     else iterate(screen mutate f, count - 1)(f)

    def part1(input: (Screen,Int)) = iterate(input._1, input._2)({
      case (true, n) => n == 2 || n == 3
      case (false, n) => n == 3
    }).countOn

    def part2(input: (Screen,Int)) = input match {
      case (screen, n) =>
        val broken = new BrokenScreen(screen.values) + (0,0,true) + (0,-1,true) + (-1,0,true) + (-1,-1,true)
        part1((broken, n))
    }
  }
}

object Dat18_Solution extends Test(Day18.LikeAGifForYourYard) {

  Part1 on """4
              .#.#.#
              ...##.
              #....#
              ..#...
              #.#..#
              ####..""" gives 4

  Part2 on """5
              .#.#.#
              ...##.
              #....#
              ..#...
              #.#..#
              ####..""" gives 17

  Part1 solveFrom "Day18.txt"
  Part2 solveFrom "Day18.txt"
}