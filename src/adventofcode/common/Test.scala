package adventofcode.common

import scala.io.Source

class Test[D,T,P](val puzzle: P with Puzzle[D,T]) extends App {

  private var testFailed = false

  def assert(descr: String)(f: => Boolean): Unit = {
    if (f) {
      print(GREEN, s"[OK ] $descr")
    } else {
      print(RED, s"[ERR] ${puzzle.title} failed: $descr")
    }
  }

  class Case[R](label: String, actual: R) {
    def gives(expected: R) {
      if (actual != expected) {
        print(RED, s"[ERR] ${puzzle.title} $label expected to be $expected, but is $actual")
        testFailed = true
      }
      else
        print(GREEN, s"[OK ] $label passed")
    }
  }

  class Part(name: String, f: D => T) {
    def on(input: String) = new Case(s"$name on $input", f(puzzle parse input))
    def solve(input: String): Unit = {
      if (testFailed) return
      val start = System.currentTimeMillis
      val result = f(puzzle parse input)
      val time = (System.currentTimeMillis - start)/1000D
      println(s"${puzzle.title}:")
      print(YELLOW, s"[***] The solution of $name is $result")
      print(YELLOW, s"[***] Time: ${time}s")
    }
    def solveFrom(file: String): Unit = {
      solve(Source.fromFile(
        puzzle.getClass.getResource(file).toURI).
        mkString)
    }
  }

  private var testCnt = 0



  case class Test[A,B](f: A => B) {
    var label = s"Test ${testCnt += 1}"
    def labeled(label: String) = { this.label = label; this }
    def on(arg: A) = new Case(s"$label on $arg", f(arg))
    def forall(pairs: (A,B)*) = pairs foreach {case (arg, expected) =>
      val actual = f(arg)
      if (actual != expected) {
        print(RED, s"[ERR] ${puzzle.title} $label on $arg expected to be $expected, but is $actual")
        testFailed = true
      }
      else
        print(GREEN, s"[OK ] $label on $arg passed")
    }
  }

  object Part1 extends Part("Part 1", puzzle.part1)
  object Part2 extends Part("Part 2", puzzle.part2)

  private val RED = "31"
  private val GREEN = "32"
  private val YELLOW = "33"
  private val ctrl = 0x1b.asInstanceOf[Char]
  private def print(color: String, text: String): Unit = {
    println(s"$ctrl[${color}m$text$ctrl[0m")
  }
}

