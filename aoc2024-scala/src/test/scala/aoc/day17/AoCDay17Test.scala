package aoc.day17

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay17Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day17Part1" should "be valid" in {
    val result = day17part1(input)
    assert(result == 0)
  }

  "Day17Part2" should "be valid" in {
    val result = day17part2(input)
    assert(result == 1731717)
  }

}

