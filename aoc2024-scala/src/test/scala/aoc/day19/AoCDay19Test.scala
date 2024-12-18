package aoc.day19

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay19Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day19Part1" should "be valid" in {
    val result = day19part1(input)
    assert(result == 0)
  }

  "Day19Part2" should "be valid" in {
    val result = day19part2(input)
    assert(result == 1931919)
  }

}

