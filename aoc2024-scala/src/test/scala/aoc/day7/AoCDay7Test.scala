package aoc.day7

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay7Test extends AnyFlatSpec {

    val input =
"""190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20""".split("[\n\r]+").toList

  "Day7Part1" should "be valid" in {
    val result = day7part1(input)
    assert(result == 3749)
  }

  "Day7Part2" should "be valid" in {
    val result = day7part2(input)
    assert(result == 11387)
  }

}

