package adventofcode

import java.lang.Object

import adventofcode.Day12.Json.Object
import adventofcode.common.{Puzzle, Test}

import scala.annotation.tailrec

/** --- Day 12: JSAbacusFramework.io ---
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
object Day12 {

  /** Parser combinators */
  object Parser {

    type Text = List[Char]

    type Result[+T] = List[(T,Text)]

    implicit def parser[T](p: Text => Result[T]): Parser[T] = new Parser(p)

    /** Parser operators */
    class Parser[+A](val parse: Text => Result[A]) {

      private def parsed[B](f: Result[A] => Result[B]): Parser[B] = parser(text => f(parse(text)))

      def apply(text: Text) = parse(text)

      def apply(text: String): A = {
        val result = parse(text.toList)
        result find {_._2.isEmpty} map {_._1} getOrElse { result match {
          case Nil => throw new IllegalArgumentException(s"Cannot parse $text")
          case (v,rest) :: _ => throw new IllegalArgumentException(s"Value $v parsed, but unexpected tail: $rest")
        }}
      }

      def map[B](f: A => B): Parser[B] = parsed(_ map {case (a,rest) => (f(a),rest)})

      def flatMap[B](f: A => Parser[B]): Parser[B] = parsed(_ flatMap {case (a,rest) => f(a)(rest)})

      def filter(f: A => Boolean): Parser[A] = parsed(_ filter {case (a,rest) => f(a)})

      def | [B >: A](other: => Parser[B]): Parser[B] = parser(text => parse(text) ++ other(text))

      def <+>[B](other: => Parser[B]): Parser[(A,B)] = for (a <- this; b <- other) yield (a,b)
      def  +>[B](other: => Parser[B]): Parser[B]     = for (_ <- this; b <- other) yield b
      def <+ [B](other: => Parser[B]): Parser[A]     = for (a <- this; _ <- other) yield a

      def \+(sep: => Parser[_]): Parser[List[A]] =
        this <+> maybe(sep +> this \+ sep) map {case (x,xs) => x :: xs}

      def \*(sep: => Parser[_]): Parser[List[A]] = maybe(this \+ sep)
    }

    /** Always return t */
    def just[T](t: T) = parser(text => List(t -> text))

    def maybe[T](p: Parser[List[T]]) = p | just(Nil)

    /** Parse first char */
    val pick = parser({
      case Nil => Nil
      case c :: text => List(c -> text)
    })

    def takeWhile(p: Char => Boolean): Parser[Text] = maybe(for {
      c <- pick
      if p(c)
      cs <- takeWhile(p)
    } yield c :: cs)

    def takeSome(p: Char => Boolean) = takeWhile(p) filter {_.nonEmpty}

    def literal(text: Text): Parser[Text] = text match {
      case Nil => just(Nil)
      case t :: ts => for {
        c <- pick
        if c == t
        cs <- literal(ts)
      } yield c :: cs
    }

    implicit def char(c: Char): Parser[Char] = pick filter {_ == c}
    implicit def string(s: String): Parser[String] = literal(s.toList) map {_.mkString}
  }

  object Json {

    // --- data model ---

    sealed trait Value

    case class Array(elements: Seq[Value]) extends Value

    case class Object(props: Map[String, Value]) extends Value

    case class Number(value: Int) extends Value

    case class Str(value: String) extends Value

    case object Null extends Value

    case class Bool(value: Boolean) extends Value

    // --- implicit conversions

    implicit def toNum(n: Int): Number = Number(n)
    implicit def toStr(s: String): Str = Str(s)
    implicit def toArr(xs: List[Value]): Array = Array(xs)
    implicit def toObj(ps: Map[String,Value]): Object = Object(ps)
    implicit def toNull(v: Nothing): Value = Null
    implicit def toBool(v: Boolean): Value  = Bool(v)

    def array(xs: Value*) = Array(List(xs : _*))

    import Character.{isDigit,isWhitespace}

    import Parser._

    // --- SYNTAX ------------------------------------------------------------------------------

    lazy val VALUE: Parser[Value] = NUMBER | STRING | ARRAY | OBJECT | NULL | BOOL

    val NULL = string("null") map
      {_ => Null}

    val BOOL = ("true" | "false") map
      {v => Bool(v == "true")}

    val NUMBER = ('-' | just('+')) <+> takeSome(isDigit) map
      {case (sign, digits) => Number((sign :: digits).mkString.toInt)}

    val STRING_V = '"' +> takeWhile(_ != '"') <+ '"' map {_.mkString}
    val STRING = STRING_V map Str

    val ARRAY = '[' +> (VALUE \* ',') <+ ']' map
      {Array(_)}

    val PROPERTY = STRING_V <+ ':' <+> VALUE

    val OBJECT = '{' +> (PROPERTY \* ',') <+ '}' map
      {xs => Object(xs.toMap)}

    // --- /SYNTAX -----------------------------------------------------------------------------

    def parse(text: String): Value = VALUE(text)
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
}

object Day12_Solution extends Test(Day12.JSAbacusFramework) {

  import Day12._
  import Json._
  import Json.Object

  Test(Json.parse) labeled "parse" forall (
    "123"               -> 123,
    "-23"               -> -23,
    "\"\""              -> "",
    "\"test\""          -> "test",
    "null"              -> Null,
    "true"              -> true,
    "[]"                -> Nil,
    "[1]"               -> array(1),
    "[1,2]"             -> array(1,2),
    "[1,\"a\",2]"       -> array(1,"a",2),
    "{}"                -> Object(Map()),
    "{\"a\":2}"         -> Object(Map("a" -> 2)),
    "{\"a\":1,\"b\":2}" -> Object(Map("a" -> 1, "b" -> 2))
    )

  Part1 solveFrom "Day12.json"
  Part2 solveFrom "Day12.json"
}
