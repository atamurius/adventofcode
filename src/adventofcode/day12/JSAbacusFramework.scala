package adventofcode.day12

import java.text.ParseException

import adventofcode.{Test, Puzzle}

import scala.annotation.tailrec

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

object Parser {

  /** Parsing data is list of chars */
  type Data = List[Char]

  /** Parsing result is
    *   None if text cannot be parsed or
    *   parsed value with the rest of the text if parsed successfully
    */
  type Result[T] = Option[(T, Data)]

  def parser[T](parse: Data => Result[T]) = new Parser[T](parse)
  def apply[T] = parser[T] _

  def failed(msg: String) = throw new IllegalArgumentException(msg)

  /** Parser is function from Data to Result */
  class Parser[T](parse: Data => Result[T]) {

    def apply(data: Data) = parse(data)

    def apply(text: String) = parse(text.toList) map {
      case (some, Nil) => some
      case (res, rest) => failed(s"Unexpected tail: $res --> ${rest.mkString}")
    }

    /** Apply function when parsed */
    def parsed[B](f: Result[T] => Result[B]) = new Parser[B](data => f(parse(data)))

    /** if this parser cannot parse data then parse with that parser */
    def or[B >: T](that: Parser[_ <: B]): Parser[B] = parser(data => parse(data) match {
      case None => that(data)
      case some => some
    })

    /** repeat parser 0 or more times and produce list of results */
    def ** = {
      @tailrec
      def parseList(data: Data, xs: List[T] = Nil): Result[List[T]] = parse(data) match {
        case Some((x, rest)) => parseList(rest, x :: xs)
        case None => Some(xs.reverse -> data)
      }
      parser(parseList(_))
    }

    /** apply that parser after this, ignore this result */
    def +>[B] (that: Parser[B]): Parser[B] = for (_ <- this; value <- that) yield value

    /** apply that parser after this, ignore that result */
    def <+ (that: Parser[_]): Parser[T] = for (value <- this; _ <- that) yield value

    /** apply that parser after this, produce pair of results */
    def <+>[B] (that: Parser[B]): Parser[(T,B)] = for (a <- this; b <- that) yield (a,b)

    /** produce optional result, None if this parser fails */
    def ? = this map Some.apply or just(None)

    // --- for loop --

    /** apply function to parsed value if any */
    def map[B](f: T => B) = parsed(_ map {
      case (result, data) => (f(result), data)
    })

    /** apply function to parsed value if any and parse next */
    def flatMap[B](f: T => Parser[B]) = parsed(_ flatMap {
      case (result, data) => f(result)(data)
    })

    /** apply filter to produced value if any */
    def withFilter(p: T => Boolean) = parsed(_ filter {
      case (result, data) => p(result)
    })
  }

  /** do not parse data, just return value */
  def just[T](t: T): Parser[T] = parser(data => Some(t -> data))

  /** parse set of chars where predicate succeeds, but not less than min */
  def zeroOrMore(p: Char => Boolean, min: Int = 0) = parser(_ span p match {
    case (some, rest) if some.length >= min => Some((some.mkString, rest))
    case _ => None
  })

  /** parse set of chars where predicate succeeds, at least 1 */
  def oneOrMore(p: Char => Boolean): Parser[String] = zeroOrMore(p, 1)

  /** try to parse given chars, return them if succeeds */
  def const(s: String): Parser[String] = parser(data =>
    if (data startsWith s) Some((s, data drop s.length))
    else None
  )

  /** do not construct parser till parsing */
  def lazyParser[T](p: => Parser[T]) = parser(data => p(data))

  implicit def toConst(s: String): Parser[String] = const(s)
}

object Json {

  // --- data model ---

  sealed trait Value

  case class Array(elements: Seq[Value]) extends Value

  case class Object(props: Map[String, Value]) extends Value

  case class Number(value: Int) extends Value

  case class Str(value: String) extends Value

  // --- implicit conversions

  implicit def toNum(n: Int): Number = Number(n)
  implicit def toStr(s: String): Str = Str(s)
  implicit def toArr(xs: List[Value]): Array = Array(xs)
  implicit def toObj(ps: Map[String,Value]): Object = Object(ps)

  def array(xs: Value*) = Array(List(xs : _*))

  // -- parsers ---

  import Character.isDigit
  import Parser._

  lazy val NUMBER = (("-" or just("+")) <+> oneOrMore(isDigit)
    ) map {
      case (minus, digits) => Number((minus + digits).toInt)
    }

  lazy val STRING = ("\"" +> zeroOrMore(_ != '"') <+ "\""
    ) map Str

  lazy val ARRAY = ("[" +> (VALUE <+> ("," +> VALUE).**).? <+ "]"
    ) map {
      case None => Array(Nil)
      case Some((first, rest)) => Array(first :: rest)
    }

  lazy val PROPERTY = ( STRING <+> ":" <+> VALUE
    ) map {
      case ((Str(key),_),value) => key -> value
    }

  lazy val OBJECT = ( "{" +> (PROPERTY <+> ("," +> PROPERTY).**).? <+ "}"
    ) map {
      case None => Object(Map())
      case Some((first, rest)) => Object((first :: rest).toMap)
    }

  lazy val VALUE: Parser[Value] = lazyParser(NUMBER or STRING or ARRAY or OBJECT)


  def parse(text: String): Value = VALUE(text).get
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

  import Json._

  Test(Json.parse) labeled "parse" forall (
    "123"               -> 123,
    "-23"               -> -23,
    "\"\""              -> "",
    "\"test\""          -> "test",
    "[]"                -> Nil,
    "[1]"               -> array(1),
    "[1,2]"             -> array(1,2),
    "[1,\"a\",2]"       -> array(1,"a",2),
    "{}"                -> Object(Map()),
    "{\"a\":2}"         -> Object(Map("a" -> 2)),
    "{\"a\":1,\"b\":2}" -> Object(Map("a" -> 1, "b" -> 2))
  )

  Part1 solveFrom "input.json"
  Part2 solveFrom "input.json"
}