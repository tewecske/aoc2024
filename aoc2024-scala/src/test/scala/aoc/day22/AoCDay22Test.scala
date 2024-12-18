package aoc.day22

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay22Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day22Part1" should "be valid" in {
    val result = day22part1(input)
    assert(result == 0)
  }

  "Day22Part2" should "be valid" in {
    val result = day22part2(input)
    assert(result == 2232222)
  }

}

