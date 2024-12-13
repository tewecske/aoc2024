package aoc.day12

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay12Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day12Part1" should "be valid" in {
    val result = day12part1(input)
    assert(result == 0)
  }

  "Day12Part2" should "be valid" in {
    val result = day12part2(input)
    assert(result == 1231212)
  }

}

