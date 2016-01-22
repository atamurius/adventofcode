package adventofcode

import scala.io.Source

class Test[D,T](puzzle: Puzzle[D,T]) extends App {

  var testFailed = false

  class Part(name: String, f: D => T) {
    def on(input: String) = new Case(input)
    def solve(input: String): Unit = {
      if (testFailed) return
      val start = System.currentTimeMillis
      val result = f(puzzle parse input)
      val time = (System.currentTimeMillis - start)/1000D
      println(s"${puzzle.title}:\n[***] The solution of $name is $result\n[***] Time: ${time}s")
    }
    def solveFrom(file: String): Unit = {
      solve(Source.fromFile(
        puzzle.getClass.getResource(file).toURI).
        mkString)
    }
    class Case(input: String) {
      def gives(expected: T) {
        val actual = f(puzzle parse input)
        if (actual != expected) {
          println(s"[ERR] ${puzzle.title} $name on $input expected to be $expected, but is $actual")
          testFailed = true
        }
        else
          println(s"[OK ] $name test on $input passed")
      }
    }
  }

  def assert[R](msg: String, actual: R, expected: R): Unit = {
    if (actual != expected) {
      println(s"[ERR] ${puzzle.title} test $msg expected to be $expected, but is $actual")
      testFailed = true
    }
    else
      println(s"[OK ] ${puzzle.title} test $msg passed")
  }

  object Part1 extends Part("Part 1", puzzle.part1)
  object Part2 extends Part("Part 2", puzzle.part2)

}

