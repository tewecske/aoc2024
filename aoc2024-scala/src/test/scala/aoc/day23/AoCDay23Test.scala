package aoc.day23

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay23Test extends AnyFlatSpec {

    val input =
"""""".split("[\n\r]+").toList

  "Day23Part1" should "be valid" in {
    val result = day23part1(input)
    assert(result == 0)
  }

  "Day23Part2" should "be valid" in {
    val result = day23part2(input)
    assert(result == 2332323)
  }

}

