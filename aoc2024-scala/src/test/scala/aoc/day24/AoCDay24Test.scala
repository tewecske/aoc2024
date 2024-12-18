package aoc.day24

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay24Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day24Part1" should "be valid" in {
    val result = day24part1(input)
    assert(result == 0)
  }

  "Day24Part2" should "be valid" in {
    val result = day24part2(input)
    assert(result == 2432424)
  }

}

