package aoc.day15

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay15Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day15Part1" should "be valid" in {
    val result = day15part1(input)
    assert(result == 0)
  }

  "Day15Part2" should "be valid" in {
    val result = day15part2(input)
    assert(result == 1531515)
  }

}

