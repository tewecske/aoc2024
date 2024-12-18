package aoc.day18

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay18Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day18Part1" should "be valid" in {
    val result = day18part1(input)
    assert(result == 0)
  }

  "Day18Part2" should "be valid" in {
    val result = day18part2(input)
    assert(result == 1831818)
  }

}

