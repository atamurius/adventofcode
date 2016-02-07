package adventofcode.day7

import adventofcode.{Test, Puzzle}

import scala.collection.immutable.{SortedMap, TreeMap}

/* --- Day 7: Some Assembly Required ---
 * http://adventofcode.com/day/7
 *
 * This year, Santa brought little Bobby Tables a set of wires and bitwise logic gates!
 * Unfortunately, little Bobby is a little under the recommended age range, and he needs help
 * assembling the circuit.
 *
 * Each wire has an identifier (some lowercase letters) and can carry a 16-bit signal
 * (a number from 0 to 65535). A signal is provided to each wire by a gate, another wire,
 * or some specific value. Each wire can only get a signal from one source,
 * but can provide its signal to multiple destinations. A gate provides no signal until
 * all of its inputs have a signal.
 *
 * The included instructions booklet describes how to connect the parts together:
 * x AND y -> z means to connect wires x and y to an AND gate, and then connect its output to wire z.
 * For example:
 *
 *    - 123 -> x means that the signal 123 is provided to wire x.
 *    - x AND y -> z means that the bitwise AND of wire x and wire y is provided to wire z.
 *    - p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then provided to wire q.
 *    - NOT e -> f means that the bitwise complement of the value from wire e is provided to wire f.
 *
 * Other possible gates include OR (bitwise OR) and RSHIFT (right-shift).
 * If, for some reason, you'd like to emulate the circuit instead, almost all programming
 * languages (for example, C, JavaScript, or Python) provide operators for these gates.
 *
 * Part 1:
 * In little Bobby's kit's instructions booklet (provided as your puzzle input),
 * what signal is ultimately provided to wire a?
 *
 * Part 2:
 * Now, take the signal you got on wire a, override wire b to that signal,
 * and reset the other wires (including wire a).
 * What new signal is ultimately provided to wire a?
 */

sealed trait Expr {
  var result = Option.empty[Int]
  def eval(env: Map[String,Expr]): Int
  // remember result
  def getFrom(env: Map[String,Expr]) = result getOrElse {
    result = Some( eval(env) )
    result.get
  }
  def reset(): Unit = {
    result = None
  }
}

/** Value = Signal or Wire */
sealed trait Value extends Expr
case class Signal( value: Int ) extends Value {
  def eval(env: Map[String, Expr]) = value
  override def toString = value.toString
}
case class Wire( name: String ) extends Value {
  def eval(env: Map[String, Expr]) = env(name) getFrom env
  override def toString = s"$name ~ $result"
}

case class BinaryOp( left: Value, right: Value, operation: (Int,Int) => Int ) extends Expr {
  def eval(env: Map[String, Expr]) = operation( left getFrom env, right getFrom env )
  override def toString = s"($left) <op> ($right) ~ $result"
  override def reset() = {
    super.reset()
    left.reset()
    right.reset()
  }
}

case class UnaryOp( arg: Value, operation: Int => Int) extends Expr {
  def eval(env: Map[String, Expr]) = operation( arg getFrom env )
  override def toString = s"<op> ($arg) ~ $result"
  override def reset() = {
    super.reset()
    arg.reset()
  }
}

class Wires( ws: Seq[(Expr,String)] ) {
  var wires = SortedMap(ws map {case (a,b) => (b,a)} : _*)
  def valueOf(w: String) = wires(w) getFrom wires
  def reset(): Unit = {
    wires foreach {case (_, e) => e.reset()}
  }
}

case object SomeAssemblyRequired extends Puzzle[Wires, Int] {

  val number = """^\d+$"""
  val assignment = """^(\w+) -> (\w+)$""".r
  val binary = """^(\w+) (AND|OR|[LR]SHIFT) (\w+) -> (\w+)$""".r
  val unary = """^(NOT) (\w+) -> (\w+)$""".r

  def value(v: String) =
    if (v matches number) Signal(v.toInt)
    else Wire(v)

  def parse(input: String) = new Wires(input split "\n" map {
    case assignment(v, res)               => value(v) -> res
    case binary(left,"AND",right,res)     => BinaryOp(value(left), value(right), {_&_}) -> res
    case binary(left,"OR",right,res)      => BinaryOp(value(left), value(right), {_|_}) -> res
    case binary(left,"LSHIFT",right,res)  => BinaryOp(value(left), value(right), {(a,b) => (a << b) & 0xffff}) -> res
    case binary(left,"RSHIFT",right,res)  => BinaryOp(value(left), value(right), {_>>_}) -> res
    case unary("NOT",arg,res)             => UnaryOp(value(arg), {~_ & 0xffff}) -> res
  })

  def part1(input: Wires) = input valueOf "a"

  def part2(input: Wires) = {
    val a = input valueOf "a"
    input.wires += ("b" -> Signal(a))
    input.reset()
    input valueOf "a"
  }
}

object Solution extends Test(SomeAssemblyRequired) {
  var test = SomeAssemblyRequired parse List(
    "123 -> x",
    "456 -> y",
    "x AND y -> d",
    "x OR y -> e",
    "x LSHIFT 2 -> f",
    "y RSHIFT 2 -> g",
    "NOT x -> h",
    "NOT y -> i"
  ).mkString("\n")

  Test(test.valueOf) labeled "wire" forall (
      "x" -> 123,
      "y" -> 456,
      "d" -> 72,
      "e" -> 507,
      "f" -> 492,
      "g" -> 114,
      "h" -> 65412,
      "i" -> 65079
  )

  Part2 solveFrom "input.txt"
}