package aoc.day3

import org.scalatest.flatspec.AnyFlatSpec

class AoCDay3Test extends AnyFlatSpec {

  "Commands" should "be valid" in {
    // xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
    val muls = mul("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

    assert(muls == List("mul(2,4)", "mul(5,5)", "mul(11,8)", "mul(8,5)"))
    
    val result = process(muls)

    assert(result == 161)

  }

  "Commands do don't" should "be valid" in {
    //xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
    val muls = mulDo("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

    assert(muls == List("mul(2,4)", "mul(8,5)"))
    
    val result = process(muls)

    assert(result == 48)

  }
}

