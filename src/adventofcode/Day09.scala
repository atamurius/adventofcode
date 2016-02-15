package adventofcode

import adventofcode.common.{Puzzle, Test}

/** --- Day 9: All in a Single Night ---
 *
 * Every year, Santa manages to deliver all of his presents in a single night.
 *
 * This year, however, he has some new locations to visit; his elves have provided him the
 * distances between every pair of locations. He can start and end at any two (different)
 * locations he wants, but he must visit each location exactly once. What is the shortest
 * distance he can travel to achieve this?
 *
 * Part 1:
 * What is the distance of the shortest route?
 */
object Day09 {

  class Distances {
    var distances = Map[(String,String),Int]()
    var cities = Set[String]()
    def withDistance(a: String, b: String, dist: Int) = {
      distances += (a,b) -> dist
      distances += (b,a) -> dist
      cities += a
      cities += b
      this
    }
    def between(cs: (String,String)) = distances(cs)
  }

  sealed trait Path {
    def canFlyTo(city: String): Boolean = this match {
      case Begin => true
      case Flight(dest, _, path) if dest == city => false
      case Flight(_, _, path) => path canFlyTo city
    }
    def to(city: String)(implicit ds: Distances) = this match {
      case Begin => Flight(city, 0, this)
      case Flight(dest, dist, _) => Flight(city, dist + (ds between (city,dest)), this)
    }
    val distance = this match {
      case Begin => 0
      case Flight(_, d, _) => d
    }
  }

  case object Begin extends Path
  case class Flight(city: String, dist: Int, path: Path) extends Path

  case object AllInASingleNight extends Puzzle[Distances,Int] {
    val pattern = """(\w+) to (\w+) = (\d+)""".r

    def parse(input: String) = (input split "\n" foldLeft new Distances) { (ds, s) => s.trim match {
      case pattern(a, b, dist) => ds.withDistance(a,b,dist.toInt)
    }}

    def findBestFrom(path: Path)(implicit ds: Distances, better: (Path,Path) => Boolean): Option[Path] = {
      if (ds.cities exists path.canFlyTo) {
        (ds.cities filter path.canFlyTo foldLeft Option.empty[Path]) {(best,city) =>
          (best, findBestFrom(path to city)) match {
            case (None, Some(p)) => Some(p)
            case (Some(b), Some(p)) if better(p,b) => Some(p)
            case _ => best
          }
        }
      }
      else
        Some(path)
    }

    def part2(input: Distances) = findBestFrom(Begin)(input, _.distance > _.distance).get.distance

    def part1(input: Distances) = findBestFrom(Begin)(input, _.distance < _.distance).get.distance
  }

  object Solution extends Test(AllInASingleNight) {
    Part1 on """London to Dublin = 464
                London to Belfast = 518
                Dublin to Belfast = 141""" gives 605

    Part2 on """London to Dublin = 464
                London to Belfast = 518
                Dublin to Belfast = 141""" gives 982

    Part2 solveFrom "Day09.txt"
  }
}
