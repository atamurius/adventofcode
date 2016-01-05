package adventofcode

import scala.io.Source

class Test[D,T](puzzle: Puzzle[D,T]) extends App {

  class Part(name: String, f: D => T) {
    def on(input: String) = new Case(input)
    def solve(input: String): Unit = {
      println(s"${puzzle.title}:\nThe solution of $name is ${f(puzzle parse input)}")
    }
    def solveFrom(file: String): Unit = {
      solve(Source.fromFile(
        puzzle.getClass.getResource(file).toURI).
        mkString)
    }
    class Case(input: String) {
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

  object Part1 extends Part("Part 1", puzzle.part1)
  object Part2 extends Part("Part 2", puzzle.part2)

}

