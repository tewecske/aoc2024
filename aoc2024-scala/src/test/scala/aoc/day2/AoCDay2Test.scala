package aoc.day2

import org.scalatest.flatspec.AnyFlatSpec
import aoc.day2.Direction.*

class AoCDay2Test extends AnyFlatSpec {

  "Levels" should "be valid" in {
    assert(check2(List(7, 6, 4, 2, 1), 0, 0, Unknown) != 0)
    assert(check2(List(1, 2, 7, 8, 9), 0, 0, Unknown) == 0)
    assert(check2(List(9, 7, 6, 2, 1), 0, 0, Unknown) == 0)
    assert(check2(List(1, 3, 2, 4, 5), 0, 0, Unknown) != 0)
    assert(check2(List(8, 6, 4, 4, 1), 0, 0, Unknown) != 0)
    assert(check2(List(1, 3, 6, 7, 9), 0, 0, Unknown) != 0)
    assert(check2(List(1, 1, 2, 3, 4), 0, 0, Unknown) != 0)
    assert(check2(List(1, 2, 3, 4, 4), 0, 0, Unknown) != 0)
    assert(check2(List(1, 2, 3, 3, 4), 0, 0, Unknown) != 0)
    assert(check2(List(8, 1, 2, 3, 4), 0, 0, Unknown) != 0)
    assert(check2(List(5, 6, 7, 8, 1), 0, 0, Unknown) != 0)
    assert(check2(List(1, 2, 3, 8, 4), 0, 0, Unknown) != 0)
    assert(check2(List(8, 7, 6, 1, 5), 0, 0, Unknown) != 0)
    assert(check2(List(8, 8, 4, 3, 2), 0, 0, Unknown) == 0)
    assert(check2(List(4, 3, 2, 8, 8), 0, 0, Unknown) == 0)
    assert(check2(List(4, 3, 2, 2, 8), 0, 0, Unknown) == 0)
    assert(check2(List(75, 77, 72, 70, 69), 0, 0, Unknown) != 0)
    assert(check2(List(28, 28, 27, 26, 23), 0, 0, Unknown) != 0)
    assert(check2(List(51, 50, 47, 45, 42, 41, 34), 0, 0, Unknown) != 0)
    assert(check2(List(8, 10, 9, 10, 9, 8, 5, 3), 0, 0, Unknown) == 0)
    assert(check2(List(76, 79, 78, 78, 80), 0, 0, Unknown) == 0)
    assert(check2(List(21, 21, 20, 19, 17, 15, 13, 6), 0, 0, Unknown) == 0)
    assert(check2(List(70, 70, 68, 65, 61), 0, 0, Unknown) == 0)
    assert(check2(List(7, 15, 6, 3, 1), 0, 0, Unknown) != 0)
    assert(check2(List(83, 76, 73, 72, 71, 71, 72), 0, 0, Unknown) == 0)
    assert(check2(List(1, 2, 3, 4, 5, 9), 0, 0, Unknown) != 0)

    assert(check2(List(8, 8, 7, 7, 6, 2), 0, 0, Unknown) == 0)
    assert(check2(List(4, 4, 3, 3, 3), 0, 0, Unknown) == 0)
    assert(check2(List(1, 5, 4, 3, 2, 1), 0, 0, Unknown) != 0)
    assert(check2(List(5, 3, 4, 5, 6), 0, 0, Unknown) != 0)
    assert(check2(List(5, 4, 3, 2, 1, 1), 0, 0, Unknown) != 0)
    assert(check2(List(9, 8, 7, 6, 7), 0, 0, Unknown) != 0)
    assert(check2(List(9, 8, 7, 7, 6), 0, 0, Unknown) != 0)
    assert(check2(List(9, 5, 5, 4, 3), 0, 0, Unknown) == 0)
    assert(check2(List(9, 5, 5, 4, 3), 0, 0, Unknown) == 0)
    assert(check2(List(9, 6, 3, 3, 1), 0, 0, Unknown) != 0)
    assert(check2(List(9, 8, 7, 2, 1), 0, 0, Unknown) == 0)
    assert(check2(List(1, 2, 3, 8, 9), 0, 0, Unknown) == 0)

    assert(check2(List(74, 68, 65, 63 ,59 ,56 ,52), 0, 0, Unknown) == 0)
    assert(check2(List(41, 35, 32, 30, 26, 19), 0, 0, Unknown) == 0)
    assert(check2(List(42, 35, 34, 32, 25, 23, 20), 0, 0, Unknown) == 0)
    assert(check2(List(45, 40, 39, 38, 36, 35, 29, 31), 0, 0, Unknown) == 0)
    assert(check2(List(62, 55, 52, 51, 49, 48, 43, 43), 0, 0, Unknown) == 0)
    assert(check2(List(61, 56, 55, 49, 47, 45, 41), 0, 0, Unknown) == 0)
    assert(check2(List(58, 51, 49, 47, 41, 38, 32), 0, 0, Unknown) == 0)
    assert(check2(List(57, 56, 54, 54, 53, 50, 53), 0, 0, Unknown) == 0)
    assert(check2(List(62, 61, 64, 61, 63, 64, 68), 0, 0, Unknown) == 0)
    assert(check2(List(43, 38, 38, 35, 32, 30, 24), 0, 0, Unknown) == 0)

    assert(check2(List(10, 10, 11, 12, 13, 17), 0, 0, Unknown) == 0)
    assert(check2(List(35, 40, 41, 38, 40, 42, 49), 0, 0, Unknown) == 0)
    assert(check2(List(52, 52, 49, 50, 49), 0, 0, Unknown) == 0)

    assert(check2(List(10, 6, 8, 7, 5), 0, 0, Unknown) != 0)
    assert(check2(List(12, 10, 13, 13, 14, 16, 19), 0, 0, Unknown) == 0)
    
    assert(check2(List(14, 17, 14, 11, 10, 6), 0, 0, Unknown) == 0)

    assert(check2(List(70, 73, 74, 71, 69, 68), 0, 0, Unknown) == 0)
    


  }
}

