package aoc.day5

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay5Test extends AnyFlatSpec {

    val input =
"""47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47""".split("[\n\r]+").toList

  "Day5Part1" should "be valid" in {
    val result = day5part1(input)
    assert(result == 143)
  }

  "Day5Part2" should "be valid" in {
    val result = day5part2(input)
    assert(result == 123)
  }

}

