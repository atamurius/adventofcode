package adventofcode.day2

import adventofcode.{Test, Puzzle}

case class Box(l: Int, w: Int, h: Int) {
  def surface = 2*l*w + 2*w*h + 2*h*l

  def paper = surface + List(l*w, w*h, h*l).min

  def ribbon = List(l+w,w+h,h+l).min * 2 + l*w*h
}

case object IWasToldThereWouldBeNoMath extends Puzzle[Seq[Box],Int] {

  def parse(input: String): Seq[Box] = {
    val box = """(\d+)x(\d+)x(\d+)""".r
    input split "\n" map {
      case box(a, b, c) => Box(a.toInt, b.toInt, c.toInt)
    }
  }

  def part1(boxes: Seq[Box]) = (boxes map {_.paper}).sum

  def part2(boxes: Seq[Box]) = (boxes map {_.ribbon}).sum
}

object Solution extends Test(IWasToldThereWouldBeNoMath) {
  Part1 on "2x3x4" gives 58
  Part1 on "1x1x10" gives 43

  Part2 on "2x3x4" gives 34
  Part2 on "1x1x10" gives 14

  Part2 solve "20x3x11\n15x27x5\n6x29x7\n30x15x9\n19x29x21\n10x4x15\n1x26x4\n1x5x18\n10x15x23\n10x14x20\n3x5x18\n29x23x30\n7x4x10\n22x24x29\n30x1x2\n19x2x5\n11x9x22\n23x15x10\n11x11x10\n30x28x5\n22x5x4\n6x26x20\n16x12x30\n10x20x5\n25x14x24\n16x17x22\n11x28x26\n1x11x10\n1x24x15\n13x17x21\n30x3x13\n20x25x17\n22x12x5\n22x20x24\n9x2x14\n6x18x8\n27x28x24\n11x17x1\n1x4x12\n5x20x13\n24x23x23\n22x1x25\n18x19x5\n5x23x13\n8x16x4\n20x21x9\n1x7x11\n8x30x17\n3x30x9\n6x16x18\n22x25x27\n9x20x26\n16x21x23\n5x24x17\n15x17x15\n26x15x10\n22x16x3\n20x24x24\n8x18x10\n23x19x16\n1x21x24\n23x23x9\n14x20x6\n25x5x5\n16x3x1\n29x29x20\n11x4x26\n10x23x24\n29x25x16\n27x27x22\n9x7x22\n6x21x18\n25x11x19\n14x13x3\n15x28x17\n14x3x12\n29x8x19\n30x14x20\n20x23x4\n8x16x5\n4x11x18\n20x8x24\n21x13x21\n14x26x29\n27x4x17\n27x4x25\n5x28x6\n23x24x11\n29x22x5\n30x20x6\n23x2x10\n11x4x7\n27x23x6\n10x20x19\n8x20x22\n5x29x22\n16x13x2\n2x11x14\n6x12x4\n3x13x6\n16x5x18\n25x3x28\n21x1x5\n20x16x19\n28x30x27\n26x7x18\n25x27x24\n11x19x7\n21x19x17\n2x12x27\n20x5x14\n8x5x8\n6x24x8\n7x28x20\n3x20x28\n5x20x30\n13x29x1\n26x29x5\n19x28x25\n5x19x11\n11x20x22\n4x23x1\n19x25x12\n3x10x6\n3x14x10\n28x16x12\n23x12x2\n23x12x19\n20x28x10\n9x10x25\n16x21x16\n1x18x20\n9x4x26\n3x25x8\n17x16x28\n9x28x16\n27x3x12\n17x24x12\n13x21x10\n7x17x13\n6x10x9\n7x29x25\n11x19x30\n1x24x5\n20x16x23\n24x28x21\n6x29x19\n25x2x19\n12x5x26\n25x29x12\n16x28x22\n26x26x15\n9x13x5\n10x29x7\n1x24x16\n22x2x2\n6x16x13\n3x12x28\n4x12x13\n14x27x21\n14x23x26\n7x5x18\n8x30x27\n15x9x18\n26x16x5\n3x29x17\n19x7x18\n16x18x1\n26x15x30\n24x30x21\n13x20x7\n4x12x10\n27x20x11\n28x29x21\n20x14x30\n28x12x3\n19x1x8\n4x8x6\n21x14x2\n27x19x21\n17x24x14\n15x18x11\n18x7x26\n25x28x29\n27x26x9\n18x12x17\n24x28x25\n13x24x14\n26x9x28\n9x3x30\n9x2x9\n8x1x29\n18x30x10\n18x14x5\n26x8x30\n12x1x1\n30x5x28\n26x17x21\n10x10x10\n20x7x27\n13x17x6\n21x13x17\n2x16x8\n7x9x9\n15x26x4\n11x28x25\n10x6x19\n21x6x29\n15x5x6\n28x9x16\n14x3x10\n12x29x5\n22x19x19\n25x15x22\n30x6x28\n11x23x13\n20x25x14\n26x1x13\n6x14x15\n16x25x17\n28x4x13\n10x24x25\n4x13x10\n9x15x16\n15x24x6\n22x9x19\n11x11x8\n4x19x12\n24x5x4\n27x12x13\n7x27x16\n2x6x9\n29x27x15\n18x26x23\n19x16x15\n14x5x25\n9x16x30\n4x6x4\n13x10x10\n1x8x29\n23x5x17\n19x20x20\n11x27x24\n27x15x5\n15x11x12\n21x11x3\n1x13x22\n17x8x8\n13x14x14\n17x22x7\n9x5x8\n2x6x3\n25x9x15\n11x8x13\n9x25x12\n3x16x12\n12x16x8\n16x24x17\n4x6x26\n22x29x11\n14x17x19\n28x2x27\n24x22x19\n22x20x30\n23x28x4\n16x12x14\n22x24x22\n29x1x28\n26x29x16\n3x25x30\n27x3x13\n22x24x26\n25x3x2\n7x24x2\n10x5x3\n28x8x29\n25x6x4\n12x17x14\n24x3x5\n23x27x7\n26x23x30\n11x10x19\n23x7x11\n26x14x15\n14x3x25\n12x24x14\n2x14x12\n9x12x16\n9x2x28\n3x8x2\n22x6x9\n2x30x2\n25x1x9\n20x11x2\n14x11x12\n7x14x12\n24x8x26\n13x21x23\n18x17x23\n13x6x17\n20x20x19\n13x17x29\n7x24x24\n23x8x6\n19x10x28\n3x8x21\n15x20x18\n11x27x1\n11x24x28\n13x20x11\n18x19x22\n27x22x12\n28x3x2\n13x4x29\n26x5x6\n14x29x25\n7x4x7\n5x17x7\n2x8x1\n22x30x24\n22x21x28\n1x28x13\n11x20x4\n25x29x19\n9x23x4\n30x6x11\n25x18x10\n28x10x24\n3x5x20\n19x28x10\n27x19x2\n26x20x4\n19x21x6\n2x12x30\n8x26x27\n11x27x10\n14x13x17\n4x3x21\n2x20x21\n22x30x3\n2x23x2\n3x16x12\n22x28x22\n3x23x29\n8x25x15\n9x30x4\n10x11x1\n24x8x20\n10x7x27\n7x22x4\n27x13x17\n5x28x5\n30x15x13\n10x8x17\n8x21x5\n8x17x26\n25x16x4\n9x7x25\n13x11x20\n6x30x9\n15x14x12\n30x1x23\n5x20x24\n22x7x6\n26x11x23\n29x7x5\n13x24x28\n22x20x10\n18x3x1\n15x19x23\n28x28x20\n7x26x2\n9x12x20\n15x4x6\n1x17x21\n3x22x17\n9x4x20\n25x19x5\n9x11x22\n14x1x17\n14x5x16\n30x5x18\n19x6x12\n28x16x22\n13x4x25\n29x23x18\n1x27x3\n12x14x4\n10x25x19\n15x19x30\n11x30x4\n11x22x26\n13x25x2\n17x13x27\n11x30x24\n15x1x14\n17x18x4\n26x11x3\n16x22x28\n13x20x9\n1x18x3\n25x11x12\n20x21x1\n22x27x4\n8x28x23\n7x13x27\n17x9x26\n27x27x20\n11x20x12\n26x21x11\n29x14x12\n27x25x1\n28x29x25\n21x23x28\n5x18x18\n19x5x4\n7x6x30\n27x8x11\n12x24x12\n16x25x22\n26x11x29\n25x22x17\n15x23x23\n17x9x6\n30x10x16\n21x3x5\n18x27x2\n28x21x14\n16x18x17\n4x18x2\n9x1x14\n9x1x9\n5x27x12\n8x16x30\n3x19x19\n16x26x24\n1x6x9\n15x14x3\n11x7x19\n8x19x3\n17x26x26\n6x18x11\n19x12x4\n29x20x16\n20x17x23\n6x6x5\n20x30x19\n18x25x18\n2x26x2\n3x1x1\n14x25x18\n3x1x6\n11x14x18\n17x23x27\n25x29x9\n6x25x20\n20x10x9\n17x5x18\n29x14x8\n14x25x26\n10x15x29\n23x19x11\n22x2x2\n4x5x5\n13x23x25\n19x13x19\n20x18x6\n30x7x28\n26x18x17\n29x18x10\n30x29x1\n12x26x24\n18x17x26\n29x28x15\n3x12x20\n24x10x8\n30x15x6\n28x23x15\n14x28x11\n10x27x19\n14x8x21\n24x1x23\n1x3x27\n6x15x6\n8x25x26\n13x10x25\n6x9x8\n10x29x29\n26x23x5\n14x24x1\n25x6x22\n17x11x18\n1x27x26\n18x25x23\n20x15x6\n2x21x28\n2x10x13\n12x25x14\n2x14x23\n30x5x23\n29x19x21\n29x10x25\n14x22x16\n17x11x26\n12x17x30\n8x17x7\n20x25x28\n20x11x30\n15x1x12\n13x3x24\n16x23x23\n27x3x3\n26x3x27\n18x5x12\n12x26x7\n19x27x12\n20x10x28\n30x12x25\n3x14x10\n21x26x1\n24x26x26\n7x21x30\n3x29x12\n29x28x5\n5x20x7\n27x11x2\n15x20x4\n16x15x15\n19x13x7\n7x17x15\n27x24x15\n9x17x28\n20x21x14\n14x29x29\n23x26x13\n27x23x21\n18x13x6\n26x16x21\n18x26x27\n9x3x12\n30x18x24\n12x11x29\n5x15x1\n1x16x3\n14x28x11\n2x18x1\n19x18x19\n18x28x21\n2x3x14\n22x16x5\n28x18x28\n24x16x18\n7x4x10\n19x26x19\n24x17x7\n25x9x6\n25x17x7\n20x22x20\n3x3x7\n23x19x15\n21x27x21\n1x23x11\n9x19x4\n22x4x18\n6x15x5\n15x25x2\n23x11x20\n27x16x6\n27x8x5\n10x10x19\n22x14x1\n7x1x29\n8x11x17\n27x9x27\n28x9x24\n17x7x3\n26x23x8\n7x6x30\n25x28x2\n1x30x25\n3x18x18\n28x27x15\n14x14x1\n10x25x29\n18x12x9\n20x28x16\n26x27x22\n8x26x1\n21x2x12\n25x16x14\n21x19x5\n12x9x22\n16x5x4\n5x4x16\n25x29x3\n4x29x13\n15x16x29\n8x11x24\n30x11x20\n17x21x14\n12x24x10\n10x12x6\n3x26x30\n15x14x25\n20x12x21\n13x11x16\n15x13x3\n5x17x29\n6x3x23\n9x26x11\n30x1x8\n14x10x30\n18x30x10\n13x19x19\n16x19x17\n28x7x10\n28x29x4\n3x21x10\n4x28x24\n7x28x9\n2x4x9\n25x27x13\n6x12x15\n4x18x20\n20x1x16\n5x13x24\n11x11x10\n12x9x23\n1x9x30\n17x28x24\n9x5x27\n21x15x16\n17x4x14\n8x14x4\n13x10x7\n17x12x14\n9x19x19\n2x7x21\n8x24x23\n19x5x12\n11x23x21\n13x3x1\n5x27x15\n12x25x25\n13x21x16\n9x17x11\n1x15x21\n4x26x17\n11x5x15\n23x10x15\n12x17x21\n27x15x1\n4x29x14\n5x24x25\n10x10x12\n18x12x9\n11x24x23\n24x23x3\n28x12x15\n29x9x14\n11x25x8\n5x12x2\n26x26x29\n9x21x2\n8x8x25\n1x16x30\n17x29x20\n9x22x13\n7x18x16\n3x3x23\n26x25x30\n15x23x24\n20x23x5\n20x16x10\n23x7x8\n20x18x26\n8x27x6\n30x23x23\n7x7x24\n21x11x15\n1x30x25\n26x27x22\n30x28x13\n20x13x13\n3x1x15\n16x7x1\n7x25x15\n12x7x18\n16x9x23\n16x12x18\n29x5x2\n17x7x7\n21x17x5\n9x9x17\n26x16x10\n29x29x23\n17x26x10\n5x19x17\n1x10x1\n14x21x20\n13x6x4\n13x13x3\n23x4x18\n4x16x3\n16x30x11\n2x11x2\n15x30x15\n20x30x22\n18x12x16\n23x5x16\n6x14x15\n9x4x11\n30x23x21\n20x7x12\n7x18x6\n15x6x5\n18x22x19\n16x10x22\n26x20x25\n9x25x25\n29x21x10\n9x21x24\n7x18x21\n14x3x15\n18x19x19\n4x29x17\n14x10x9\n2x26x14\n13x3x24\n4x4x17\n6x27x24\n2x18x3\n14x25x2\n30x14x17\n11x6x14\n4x10x18\n15x4x2\n27x7x10\n13x24x1\n7x12x6\n25x22x26\n19x2x18\n23x29x2\n2x15x4\n12x6x9\n16x14x29\n9x17x3\n21x9x12\n23x18x22\n10x8x4\n29x2x7\n19x27x15\n4x24x27\n25x20x14\n8x23x19\n1x24x19\n6x20x10\n15x8x5\n18x28x5\n17x23x22\n9x16x13\n30x24x4\n26x3x13\n12x22x18\n29x17x29\n26x4x16\n15x7x20\n9x15x30\n12x7x18\n28x19x18\n11x23x23\n24x20x1\n20x3x24\n1x26x1\n14x10x6\n5x27x24\n13x21x12\n20x20x5\n6x28x9\n11x26x11\n26x29x12\n21x4x11\n20x11x17\n22x27x20\n19x11x21\n2x11x11\n13x5x7\n12x10x25\n21x28x1\n15x30x17\n28x19x1\n4x19x12\n11x4x12\n4x10x30\n11x18x5\n22x20x12\n3x7x27\n20x26x4\n13x27x26\n23x14x13\n4x19x7\n26x27x16\n20x5x20\n18x5x8\n19x21x1\n22x8x1\n29x4x1\n24x10x15\n24x9x20\n10x3x8\n29x30x3\n2x8x24\n16x7x18\n2x11x23\n23x15x16\n21x12x6\n24x28x9\n6x1x13\n14x29x20\n27x24x13\n16x26x8\n5x6x17\n21x8x1\n28x19x21\n1x14x16\n18x2x9\n29x28x10\n22x26x27\n18x26x23\n22x24x2\n28x26x1\n27x29x12\n30x13x11\n1x25x5\n13x30x18\n3x13x22\n22x10x11\n2x7x7\n18x17x8\n9x22x26\n30x18x16\n10x2x3\n7x27x13\n3x20x16\n9x21x16\n1x18x15\n21x30x30\n4x25x23\n3x11x7\n5x6x12\n27x1x20\n13x15x24\n23x29x2\n13x5x24\n22x16x15\n28x14x3\n29x24x9\n2x20x4\n30x10x4\n23x7x20\n22x12x21\n3x19x11\n4x28x28\n5x4x7\n28x12x25\n2x16x26\n23x20x7\n5x21x29\n9x21x16\n9x6x10\n9x6x4\n24x14x29\n28x11x6\n10x22x1\n21x30x20\n13x17x8\n2x25x24\n19x21x3\n28x8x14\n6x29x28\n27x10x28\n30x11x12\n17x2x10\n14x19x17\n2x11x4\n26x1x2\n13x4x4\n23x20x18\n2x17x21\n28x7x15\n3x3x27\n24x17x30\n28x28x20\n21x5x29\n13x12x19\n24x29x29\n19x10x6\n19x12x14\n21x4x17\n27x16x1\n4x17x30\n23x23x18\n23x15x27\n26x2x11\n12x8x8\n15x23x26\n30x17x15\n17x17x15\n24x4x30\n9x9x10\n14x25x20\n25x11x19\n20x7x1\n9x21x3\n7x19x9\n10x6x19\n26x12x30\n21x9x20\n15x11x6\n30x21x9\n10x18x17\n22x9x8\n8x30x26\n28x12x27\n17x17x7\n11x13x8\n5x3x21\n24x1x29\n1x28x2\n18x28x10\n8x29x14\n26x26x27\n17x10x25\n22x30x3\n27x9x13\n21x21x4\n30x29x16\n22x7x20\n24x10x2\n16x29x17\n28x15x17\n19x19x22\n9x8x6\n26x23x24\n25x4x27\n16x12x2\n11x6x18\n19x14x8\n9x29x13\n23x30x19\n10x16x1\n4x21x28\n23x25x25\n19x9x16\n30x11x12\n24x3x9\n28x19x4\n18x12x9\n7x1x25\n28x7x1\n24x3x12\n30x24x22\n27x24x26\n9x30x30\n29x10x8\n4x6x18\n10x1x15\n10x4x26\n23x20x16\n6x3x14\n30x8x16\n25x14x20\n11x9x3\n15x23x25\n8x30x22\n22x19x18\n25x1x12\n27x25x7\n25x23x3\n13x20x8\n5x30x7\n18x19x27\n20x23x3\n1x17x21\n21x21x27\n13x1x24\n7x30x20\n21x9x18\n23x26x6\n22x9x29\n17x6x21\n28x28x29\n19x25x26\n9x27x21\n5x26x8\n11x19x1\n10x1x18\n29x4x8\n21x2x22\n14x12x8"
}