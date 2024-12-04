package aoc.day4

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay4Test extends AnyFlatSpec {

  "XMAS" should "be valid" in {
    val wordblock =
"""MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX""".split("[\n\r]+").toList
    val found = findXmas(wordblock)
    assert(found == 18)
  }

  "X-MAS" should "be valid" in {
    val wordblock =
""".M.S......
..A..MSMS.
.M.S.MAA..
..A.ASMSM.
.M.S.M....
..........
S.S.S.S.S.
.A.A.A.A..
M.M.M.M.M.
..........""".split("[\n\r]+").toList
    val found = findX_mas(wordblock)
    assert(found == 9)
  }

}

