package adventofcode

import scala.io.Source

class Test[D,T](puzzle: Puzzle[D,T]) extends App {

  object Part1 {
    def on(input: String) = Case(input, "Part 1", puzzle.part1)
    def solve(input: String): Unit = {
      println(s"${puzzle.title}:\nThe solution of Part1 is ${puzzle.part1(puzzle parse input)}")
    }
  }

  object Part2 {
    def on(input: String) = Case(input, "Part 2", puzzle.part2)
    def solve(input: String): Unit = {
      println(s"${puzzle.title}:\nThe solution of Part2 is ${puzzle.part2(puzzle parse input)}")
    }
    def solveFrom(file: String): Unit = {
      solve(Source.fromFile(
        getClass.getResource(file).toURI).
        mkString)
    }
  }

  case class Case(input: String, name: String, f: D => T) {
    def gives(expected: T) {
      val actual = f(puzzle parse input)
      if (actual != expected)
        throw new AssertionError(
          s"${puzzle.title} $name on $input expected to be $expected, but is $actual")
      else
        println(s"$name test on $input passed")
    }
  }
}

