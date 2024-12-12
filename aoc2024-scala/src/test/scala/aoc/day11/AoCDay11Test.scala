package aoc.day11

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay11Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day11Part1" should "be valid" in {
    val result = day11part1(input)
    assert(result == 0)
  }

  "Day11Part2" should "be valid" in {
    val result = day11part2(input)
    assert(result == 1131111)
  }

}

