package aoc.day16

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay16Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day16Part1" should "be valid" in {
    val result = day16part1(input)
    assert(result == 0)
  }

  "Day16Part2" should "be valid" in {
    val result = day16part2(input)
    assert(result == 1631616)
  }

}

