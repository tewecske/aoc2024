package aoc.day25

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay25Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day25Part1" should "be valid" in {
    val result = day25part1(input)
    assert(result == 0)
  }

  "Day25Part2" should "be valid" in {
    val result = day25part2(input)
    assert(result == 2532525)
  }

}

