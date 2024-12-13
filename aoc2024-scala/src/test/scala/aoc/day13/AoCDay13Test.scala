package aoc.day13

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay13Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day13Part1" should "be valid" in {
    val result = day13part1(input)
    assert(result == 0)
  }

  "Day13Part2" should "be valid" in {
    val result = day13part2(input)
    assert(result == 1331313)
  }

}

