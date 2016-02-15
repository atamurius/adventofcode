package adventofcode

import adventofcode.common.{Puzzle, Test}

/** --- Day 3: Perfectly Spherical Houses in a Vacuum ---
 *
 * Santa is delivering presents to an infinite two-dimensional grid of houses.
 *
 * He begins by delivering a present to the house at his starting location,
 * and then an elf at the North Pole calls him via radio and tells him where to move next.
 * Moves are always exactly one house to the north (^), south (v), east (>), or west (<).
 * After each move, he delivers another present to the house at his new location.
 *
 * However, the elf back at the north pole has had a little too much eggnog, and
 * so his directions are a little off, and Santa ends up visiting some houses more than once.
 *
 * The next year, to speed up the process, Santa creates a robot version of himself,
 * Robo-Santa, to deliver presents with him.
 *
 * Santa and Robo-Santa start at the same location (delivering two presents to the same
 * starting house), then take turns moving based on instructions from the elf, who is
 * eggnoggedly reading from the same script as the previous year.
 */
object Day03 {

  case object PerfectlySphericalHousesInAVacuum extends Puzzle[Seq[(Int, Int)], Int] {
    def parse(input: String): Seq[(Int, Int)] = input map {
      case '<' => (-1, 0)
      case '>' => (+1, 0)
      case '^' => (0, +1)
      case 'v' => (0, -1)
    }

    def houses(input: Seq[(Int, Int)]) =
      (input scanLeft(0, 0)) { (a, b) => (a, b) match {
        case ((x1, y1), (x2, y2)) => (x1 + x2, y1 + y2)
      }}

    /** How many houses receive at least one present? */
    def part1(moves: Seq[(Int, Int)]): Int = houses(moves).distinct.size

    def partition[T](seq: Seq[T]) = {
      val (even, odd) = seq.zipWithIndex partition {_._2 % 2 == 0}
      List(even, odd) map {_ map { pair => pair._1 }}
    }

    /** This year, how many houses receive at least one present? */
    def part2(moves: Seq[(Int, Int)]): Int =
      (partition(moves) flatMap houses).distinct.length
  }

  object Solution extends Test(PerfectlySphericalHousesInAVacuum) {
    Part1 on ">" gives 2
    Part1 on "^>v<" gives 4
    Part1 on "^v^v^v^v^v" gives 2

    Part2 on "^v" gives 3
    Part2 on "^>v<" gives 3
    Part2 on "^v^v^v^v^v" gives 11

    Part1 solveFrom "Day03.txt"
    Part2 solveFrom "Day03.txt"
  }

}
