package adventofcode

trait Puzzle[D,R] {
  def parse(input: String): D

  def part1(input: D): R
  def part2(input: D): R

  def title = toString.replaceAll("([A-Z][a-z]*)", "$1 ").trim
}

object Puzzle {

  trait Simple[T] extends Puzzle[String,T] {
    def parse(input: String): String = input
  }
}