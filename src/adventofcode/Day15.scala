package adventofcode

import adventofcode.common.{Test, Puzzle}

/** --- Day 15: Science for Hungry People ---
  * Today, you set out on the task of perfecting your milk-dunking cookie recipe. All you have to do is find
  * the right balance of ingredients.
  *
  * Your recipe leaves room for exactly 100 teaspoons of ingredients. You make a list of the remaining ingredients
  * you could use to finish the recipe (your puzzle input) and their properties per teaspoon:
  *
  *   - capacity (how well it helps the cookie absorb milk)
  *   - durability (how well it keeps the cookie intact when full of milk)
  *   - flavor (how tasty it makes the cookie)
  *   - texture (how it improves the feel of the cookie)
  *   - calories (how many calories it adds to the cookie)
  *
  * You can only measure ingredients in whole-teaspoon amounts accurately, and you have to be accurate so you can
  * reproduce your results in the future. The total score of a cookie can be found by adding up each of the properties
  * (negative totals become 0) and then multiplying together everything except calories.
  *
  * Part 1:
  * Given the ingredients in your kitchen and their properties,
  * what is the total score of the highest-scoring cookie you can make?
  */
object Day15 {

  case class Ingredient(name: String, props: Map[String,Int])

  case object ScienceForHungryPeople extends Puzzle[Seq[Ingredient],Int] {

    def parse(input: String) = {
      val pattern = """\s*(\w+): (.*)\s*""".r
      input split "\n" map {
        case pattern(name, params) => Ingredient(name, Map(params split ", " map {_ split " "} map {a => a(0) -> a(1).toInt} : _*))
      }
    }

    def score(ingrs: Seq[Ingredient])(counts: Seq[Int]) =
      ((ingrs zip counts).foldLeft(Map[String,Int]()) {(map, pair) =>
        val (Ingredient(_,props), cnt) = pair
        props.foldLeft(map) {(map, pair) => {
          val (prop, cost) = pair
          map + (prop -> (map.getOrElse(prop,0) + cnt*cost))
        }}
      } - "calories").values.map(Math.max(0,_)).product

    def calories(ingrs: Seq[Ingredient])(counts: Seq[Int]) =
      (ingrs map {_.props.getOrElse("calories",0)} zip counts) map {case (a,b) => a*b max 0} sum

    def counts(n: Int, sum: Int): Seq[List[Int]] =
      if (n == 0) List(Nil)
      else if (n == 1) List(List(sum))
      else for {
        x <- 0 to sum
        rest <- counts(n-1, sum - x)
      } yield x :: rest

    def part1(input: Seq[Ingredient]) = counts(input.size, 100) map score(input) max

    def part2(input: Seq[Ingredient]) = counts(input.size, 100) filter {500 == calories(input)(_)} map score(input) max
  }

  object Solution extends Test(ScienceForHungryPeople) {

    Test((puzzle.counts _).tupled) labeled "counts" forall (
      (0,0) -> List(Nil),
      (1,2) -> List(List(2)),
      (2,2) -> List(List(0,2),List(1,1),List(2,0)))

    val test = """Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
                  Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"""

    Part1 on test gives 62842880

    Part2 on test gives 57600000

    val input = """Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
                   Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
                   Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
                   Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1"""

    Part1 solve input

    Part2 solve input
  }
}
