package aoc.day20

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay20Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day20Part1" should "be valid" in {
    val result = day20part1(input)
    assert(result == 0)
  }

  "Day20Part2" should "be valid" in {
    val result = day20part2(input)
    assert(result == 2032020)
  }

}

