package aoc.day6

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay6Test extends AnyFlatSpec {

    val input =
"""....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...""".split("[\n\r]+").toList

  "Day6Part1" should "be valid" in {
    val result = day6part1(input)
    assert(result == 143)
  }

  "Day6Part2" should "be valid" in {
    val result = day6part2(input)
    assert(result == 123)
  }

}

