package adventofcode

import adventofcode.common.{Puzzle, Test}

/** --- Day 23: Opening the Turing Lock ---
  *
  * Little Jane Marie just got her very first computer for Christmas from some unknown benefactor.
  * It comes with instructions and an example program, but the computer itself seems to be malfunctioning.
  * She's curious what the program does, and would like you to help her run it.
  *
  * The manual explains that the computer supports two registers and six instructions
  * (truly, it goes on to remind the reader, a state-of-the-art technology).
  * The registers are named a and b, can hold any non-negative integer, and begin with a value of 0.
  *
  * All three jump instructions work with an offset relative to that instruction.
  * The offset is always written with a prefix + or - to indicate the direction of the jump
  * (forward or backward, respectively). For example, jmp +1 would simply continue with the next instruction,
  * while jmp +0 would continuously jump back to itself forever.
  *
  * The program exits when it tries to run an instruction beyond the ones defined.
  *
  * What is the value in register b when the program in your puzzle input is finished executing?
  */

object Day23 {

  sealed trait Value
  case class Num(value: Int) extends Value
  case class Reg(name: Char) extends Value

  implicit def toNum(value: Int) = Num(value)
  implicit def toReg(name: Char) = Reg(name)

  sealed trait Instr
  object Instr {
    case class hlf(arg: Reg) extends Instr
    case class tpl(arg: Reg) extends Instr
    case class inc(arg: Reg) extends Instr
    case class jmp(offset: Num) extends Instr
    case class jie(reg: Reg, offset: Num) extends Instr
    case class jio(reg: Reg, offset: Num) extends Instr

    def parse(code: String): Array[Instr] =
      code.split("\n").map(parseInstr).toArray

    private val INSTR = """^\s*([a-z]+)(?:\s+((?:[+-]?\d+)|[a-z])(?:\s*,\s*((?:[+-]?\d+)|[a-z]))?)?\s*$""".r
    def parseInstr(line: String): Instr = line match {
      case INSTR("hlf", reg, null) => hlf(parseReg(reg))
      case INSTR("tpl", reg, null) => tpl(parseReg(reg))
      case INSTR("inc", reg, null) => inc(parseReg(reg))
      case INSTR("jmp", num, null) => jmp(parseNum(num))
      case INSTR("jie", reg, num) => jie(parseReg(reg), parseNum(num))
      case INSTR("jio", reg, num) => jio(parseReg(reg), parseNum(num))
      case _ => sys.error(s"Unknown instruction or arguments: $line")
    }

    def parseNum(num: String) = Num(num.toInt)

    def parseReg(reg: String): Reg = reg match {
      case "a" => Reg('a')
      case "b" => Reg('b')
      case _ => sys.error(s"Register expected, but got $reg")
    }
  }

  case class Machine(val code: Array[Instr],
                     var a: Int = 0,
                     var b: Int = 0,
                     var p: Int = 0) {
    import Instr._

    def step(): Boolean =
      if (! code.indices.contains(p)) false
      else code(p) match {
        case hlf(r) => reg(r, _ >> 1) && next()
        case tpl(r) => reg(r, _ * 3) && next()
        case inc(r) => reg(r, _ + 1) && next()
        case jmp(offset) => jump(offset)
        case jie(r, offset) => if (! odd(reg(r))) jump(offset) else next()
        case jio(r, offset) => if (reg(r) == 1) jump(offset) else next()
        case i =>
          println(s"Unexpected instruction: $i")
          false
      }

    def odd(x: Int) = (x % 2) == 1

    def next(): Boolean = {
      p += 1
      code.indices contains p
    }
    def jump(offset: Num): Boolean = {
      p += offset.value
      code.indices contains p
    }

    def reg(r: Reg): Int = r match {
      case Reg('a') => a
      case Reg('b') => b
    }

    def reg(r: Reg, f: Int => Int): Boolean = r match {
      case Reg('a') =>
        a = f(a)
        true
      case Reg('b') =>
        b = f(b)
        true
      case _ =>
        false
    }

    def print = printf(
      "a: %5d, b: %5d, p: %2d, @p: %s%n",
      a, b, p, if (code.indices contains p) code(p) else "-"
    )

    def run = {
      do {
        print
      } while (step())
      println("---------")
      print
      this
    }
  }

  case object TuringLock extends Puzzle[Array[Instr], Int] {

    override def parse(input: String): Array[Instr] = Instr.parse(input)

    override def part1(code: Array[Instr]): Int = {
      Machine(code).run.b
    }

    override def part2(code: Array[Instr]): Int = {
      Machine(code, a = 1).run.b
    }
  }

}

object Day23_Solution extends Test(Day23.TuringLock) {

  import Day23._
  import Instr._

  assert("Test program #1") {
    Machine(Array(
      inc('a'),
      jio('a', +2),
      tpl('a'),
      inc('a')
    )).run.a == 2
  }

  Part1 solveFrom "Day23.txt"
  Part2 solveFrom "Day23.txt"
}