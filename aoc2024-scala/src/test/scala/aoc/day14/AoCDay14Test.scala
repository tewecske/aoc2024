package aoc.day14

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay14Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day14Part1" should "be valid" in {
    val result = day14part1(input)
    assert(result == 0)
  }

  "Day14Part2" should "be valid" in {
    val result = day14part2(input)
    assert(result == 1431414)
  }

}

