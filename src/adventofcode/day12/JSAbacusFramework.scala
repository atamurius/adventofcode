package adventofcode.day12

import adventofcode.{Test, Puzzle}

/** --- Day 12: JSAbacusFramework.io ---
  * http://adventofcode.com/day/12
  *
  * Santa's Accounting-Elves need help balancing the books after a recent order.
  * Unfortunately, their accounting software uses a peculiar storage format. That's where you come in.
  *
  * They have a JSON document which contains a variety of things:
  * arrays ([1,2,3]), objects ({"a":1, "b":2}), numbers, and strings.
  * Your first job is to simply find all of the numbers throughout the document and add them together.
  *
  * Part 1:
  * What is the sum of all numbers in the document?
  */

object Json {

  sealed trait Value

  case class Array(elements: Seq[Value]) extends Value

  case class Object(props: Map[String, Value]) extends Value

  case class Number(value: Int) extends Value

  case class Str(value: String) extends Value

  def numbers(elems:Int*) = Array(elems map Number)

  def obj(elems:(String,Int)*) = Object(Map(elems : _*) mapValues Number)

  import Character.isDigit

  def parse(text: String): Value = parse(text.toList) match {
    case (res, Nil) => res
    case (_, some) => error(s"unhandled tail $some")
  }

  def parse(text: List[Char]): (Value, List[Char]) = text match {
    case c :: _ if isDigit(c) => parseNumber(text)
    case '-' :: rest =>
      val (Number(n), r) = parseNumber(rest)
      (Number(-n), r)
    case '[' :: _ => parseArray(text, Nil)
    case '"' :: rest => parseString(rest, Nil)
    case '{' :: _ => parseObject(text, Map())
  }

  def parseNumber(text: List[Char]) = {
    val (num, rest) = text span isDigit
    (Number(num.mkString.toInt), rest)
  }

  def parseArray(text: List[Char], arr: List[Value]): (Array, List[Char]) = text match {
    case ']' :: rest => (Array(arr.reverse), rest)
    case c :: rest if c == '[' || c == ',' =>
      val (elem, r) = parse(rest)
      parseArray(r, elem :: arr)
  }

  def parseString(text: List[Char], arr: List[Char]): (Str, List[Char]) = text match {
    case '"' :: rest => (Str(arr.reverse.mkString), rest)
    case c :: rest => parseString(rest, c :: arr)
  }

  def parseObject(text: List[Char], map: Map[String,Value]): (Object, List[Char]) = text match {
    case '}' :: rest => (Object(map), rest)
    case c :: '"' :: rest if c == '{' || c == ',' =>
      val (Str(key), ':' :: r) = parseString(rest, Nil)
      val (value, r2) = parse(r)
      parseObject(r2, map + (key -> value))
  }
}

case object JSAbacusFramework extends Puzzle[Json.Value,Int] {

  def parse(input: String) = Json parse input

  def part1(input: Json.Value): Int = input match {
    case Json.Number(num) => num
    case Json.Array(elems) => (elems map part1).sum
    case Json.Object(props) => (props.values map part1).sum
    case _ => 0
  }

  def hasRed(obj: Map[String,Json.Value]) = obj exists {
    case (_, Json.Str("red")) => true
    case _ => false
  }

  def part2(input: Json.Value): Int = input match {
    case Json.Number(num) => num
    case Json.Array(elems) => (elems map part2).sum
    case Json.Object(props) if ! hasRed(props)  => (props.values map part2).sum
    case _ => 0
  }
}

object Solution extends Test(JSAbacusFramework) {

  assert("parse 123", Json parse "123", Json.Number(123))

  assert("parse [1,2,3]", Json parse "[1,2,3]", Json.numbers(1,2,3))

  assert("parse \"test\"", Json parse "\"test\"", Json.Str("test"))

  assert("parse {\"test\":5}", Json parse "{\"test\":5}", Json.obj("test" -> 5))

  Part1 solveFrom "input.json"
  Part2 solveFrom "input.json"
}