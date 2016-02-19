package adventofcode

import adventofcode.common.{Test, Puzzle}

/** --- Day 19: Medicine for Rudolph ---
  *
  * Rudolph the Red-Nosed Reindeer is sick! His nose isn't shining very brightly, and he needs medicine.
  *
  * Red-Nosed Reindeer biology isn't similar to regular reindeer biology; Rudolph is going to need custom-made medicine.
  * Unfortunately, Red-Nosed Reindeer chemistry isn't similar to regular reindeer chemistry, either.
  *
  * The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission plant,
  * capable of constructing any Red-Nosed Reindeer molecule you need.
  * It works by starting with some input molecule and then doing a series of replacements, one per step,
  * until it has the right molecule.
  *
  * However, the machine has to be calibrated before it can be used.
  * Calibration involves determining the number of molecules that can be generated
  * in one step from a given starting point.
  *
  * Part 1:
  * Your puzzle input describes all of the possible replacements and, at the bottom,
  * the medicine molecule for which you need to calibrate the machine.
  * How many distinct molecules can be created after all the different ways you can do one replacement
  * on the medicine molecule?
  */
object Day19 {

  type Rules = Seq[(String,String)]
  type CRules = Seq[(String,String,Int)]

  case object MedicineForRudolph extends Puzzle[(Rules,String),Int] {

    def parse(input: String) = {
      val rules :: query :: Nil = (input split "(\r\n){2}").toList
      val rulePattern = """\s*(\w+) => (\w+)\s*""".r
      val rs = rules split "[\r\n]+" map {
        case rulePattern(from, to) => from -> to
      }
      (rs, query.trim)
    }

    def replaces(s: String, m: String, r: String, i: Int = 0): List[String] = {
      val pos = s.indexOf(m, i)
      if (pos == -1) Nil
      else (s.substring(0,pos) + r + s.substring(pos + m.length)) :: replaces(s, m, r, pos + 1)
    }

    def produce(rules: Rules)(fs: Set[String]): Set[String] = {
      for {
        f <- fs
        (from,to) <- rules
        res <- replaces(f, from, to)
      } yield res
    }

    def search(rules: CRules, start: String, target: String): Option[Int] = {
      def deep(curr: String, cost: Int = 0): Option[Int] = {
        if (curr == target)
            return Some(cost)
        else for {
          (m, repl, c) <- rules
          f <- replaces(curr, m, repl)
          res <- deep(f, cost + c)
        } return Some(res)
        None
      }
      deep(start)
    }

    def searchDL(rules: Rules, start: String, target: String) = {
      def search(current: Set[String], step: Int = 0): Option[Int] =
        if (current contains target) Some(step)
        else {
          println(s"Current size: ${current.size}, total chars: ${current map (_.length) sum}")
          val next = produce(rules)(current)
          if (next.isEmpty) None
          else search(next, step + 1)
        }
      search(Set(start))
    }

    def toCRules(rules: Rules) = rules map {case (a,b) => (a,b,1)}

    def flip(rules: CRules) = rules map {case (a,b,c) => (b,a,c)}

    def extend(rules: CRules): CRules = rules ++ (for {
      (from1,to1,c1) <- rules
      (from2,to2,c2) <- rules
      r <- replaces(to1,from2,to2)
    } yield (from1, r, c1 + c2))

    def part1(input: (Rules, String)) = produce(input._1)(Set(input._2)).size

    def part2(input: (Rules, String)) = {
      val rules = extend(toCRules(input._1)) sortBy {-_._2.length}
      println(rules map {case (a,b,c) => s"$a -$c-> $b"} mkString "\n")
      search(flip(rules),input._2,"e") getOrElse -1
    }
  }
}

object Day19_Solution extends Test(Day19.MedicineForRudolph) {

  val (rules, start) = puzzle parse """H => HO
                                       H => OH
                                       O => HH

                                       HOH"""

  Test(puzzle.produce(rules)) labeled "produce" on Set(start) gives Set("HOOH","HOHO","OHOH","HHHH")

  Part2 on """ e => H
               e => O
               H => HO
               H => OH
               O => HH

               HOH""" gives 3

  Part2 on """ e => H
               e => O
               H => HO
               H => OH
               O => HH

               HOHOHO""" gives 6

  Part1 solveFrom "Day19.txt"

  Part2 solveFrom "Day19.txt"
}