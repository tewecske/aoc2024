package aoc.day21

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay21Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day21Part1" should "be valid" in {
    val result = day21part1(input)
    assert(result == 0)
  }

  "Day21Part2" should "be valid" in {
    val result = day21part2(input)
    assert(result == 2132121)
  }

}

