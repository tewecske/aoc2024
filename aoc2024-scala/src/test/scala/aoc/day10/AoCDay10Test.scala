package aoc.day10

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay10Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day10Part1" should "be valid" in {
    val result = day10part1(input)
    assert(result == 0)
  }

  "Day10Part2" should "be valid" in {
    val result = day10part2(input)
    assert(result == 1131010)
  }

}

