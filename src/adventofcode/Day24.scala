package adventofcode

import adventofcode.common.{Puzzle, Test}

/** --- Day 24: It Hangs in the Balance ---
  *
  * It's Christmas Eve, and Santa is loading up the sleigh for this year's deliveries.
  * However, there's one small problem: he can't get the sleigh to balance. If it isn't balanced,
  * he can't defy physics, and nobody gets presents this year.
  *
  * No pressure.
  *
  * Santa has provided you a list of the weights of every package he needs to fit on the sleigh.
  * The packages need to be split into three groups of exactly the same weight, and every package has to fit.
  * The first group goes in the passenger compartment of the sleigh, and the second and third go in
  * containers on either side. Only when all three groups weigh exactly the same amount will the sleigh be able to fly.
  * Defying physics has rules, you know!
  *
  * Of course, that's not the only problem.
  * The first group - the one going in the passenger compartment - needs as few packages as possible
  * so that Santa has some legroom left over.
  * It doesn't matter how many packages are in either of the other two groups,
  * so long as all of the groups weigh the same.
  *
  * Furthermore, Santa tells you, if there are multiple ways to arrange the packages such that the
  * fewest possible are in the first group, you need to choose the way where the first group has
  * the smallest quantum entanglement to reduce the chance of any "complications".
  * The quantum entanglement of a group of packages is the product of their weights, that is,
  * the value you get when you multiply their weights together.
  * Only consider quantum entanglement if the first group has the fewest possible number of packages
  * in it and all groups weigh the same amount.
  *
  * Part 1:
  * What is the quantum entanglement of the first group of packages in the ideal configuration?
  */
object Day24 {

  case object ItHangsInTheBalance extends Puzzle[List[Int],BigInt] {

    def parse(input: String) = (input split "\\s+").toList map Integer.parseInt

    type Ints = List[Int]

    /** list of divisions, such that sum of first part is target and first has not more elements than the rest */
    def divisions(elems: Ints, target: Int, maxsize: Int = 0, first: Ints = Nil, rest: Ints = Nil): Stream[(Ints,Ints)] = {
      // too many elements in first
      if (first.size > maxsize && maxsize > 0 || first.size > rest.size + elems.size) Stream.empty
      // no target value left - division found
      else if (target == 0) Stream((first, rest ++ elems))
      // no more elements or sum is too high -- no division here
      else if (elems.isEmpty || target < 0) Stream.empty
      // next element goes into first or rest part
      else divisions( // elems.head goes into first
          first = elems.head :: first,
          target = target - elems.head,
          rest = rest,
          maxsize = maxsize,
          elems = elems.tail) #:::
        divisions( // elems.head goes into rest
          first = first,
          target = target,
          rest = elems.head :: rest,
          maxsize = maxsize,
          elems = elems.tail)
    }

    def divisions3(elems: Ints, maxsize: Int = 0) =
      divisions(elems, elems.sum / 3, maxsize). // take divisions by 3
        filterNot {case (first,rest) => divisions(rest, rest.sum / 2).isEmpty}. // where the rest can be divided by 2
        map{_._1} // only first part

    /** tries to solve using minimum part length, returns stream with only minimal length solutions */
    def findMin(f: (Ints, Int) => Stream[Ints], elems: Ints): Stream[Ints] = {
      for (n <- 2 until elems.size) {
        val stream = f(elems, n)
        if (stream.nonEmpty) {
          return stream filter {_.size == n}
        }
      }
      Stream.empty
    }

    def divisions4(elems: Ints, maxsize: Int = 0) =
      divisions(elems, elems.sum / 4, maxsize). // take divisions by 4
        filterNot {case (first,rest) => findMin(divisions3, rest).isEmpty}. // where the rest can be divided by 3
        map {_._1} // only first part

    def bigProduct(xs: Ints) = (xs map BigInt.apply).product

    def part1(input: List[Int]): BigInt = (findMin(divisions3, input) map bigProduct).min

    def part2(input: List[Int]): BigInt = (findMin(divisions4, input) map bigProduct).min
  }

  object Solution extends Test(ItHangsInTheBalance) {

    Part1 on "1 2 3 4 5 7 8 9 10 11" gives 99

    Part2 on "1 2 3 4 5 7 8 9 10 11" gives 44

    Part1 solve "1 2 3 5 7 13 17 19 23 29 31 37 41 43 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113"

    Part2 solve "1 2 3 5 7 13 17 19 23 29 31 37 41 43 53 59 61 67 71 73 79 83 89 97 101 103 107 109 113"
  }
}
