package aoc.day8

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay8Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day8Part1" should "be valid" in {
    val result = day8part1(input)
    assert(result == 3849)
  }

  "Day8Part2" should "be valid" in {
    val result = day8part2(input)
    assert(result == 11388)
  }

}

