package aoc.day3

import os.*
import scala.annotation.tailrec

@main def aoc2024day3(): Unit = {
  println("Day3")
  day3()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day3(): Unit = {
  val lines = os.read.lines.stream(os.resource / "aoc/day3/day3_input")

  val line = lines.toList.mkString("")
  val muls = mul(line)
  val result = process(muls)
  println(s"Result: $result")

  val muls2 = mulDo(line)
  val result2 =process(muls2)
  println(s"Result2: $result2")
}

val mulRegex = "mul\\(\\d+,\\d+\\)|do\\(\\)|don't\\(\\)".r

def mul(data: String): List[String] = {
  mulRegex.findAllIn(data).toList
}

def mulDo(data: String): List[String] = {
  var on = true
  mulRegex.findAllIn(data).toList.map { cmd =>
    if (on == true) {
      if (cmd == "don't()") {
        on = false
        None
      } else if (cmd == "do()") {
        None
      } else {
        Some(cmd)
      }
    } else {
      if (cmd == "do()") {
        on = true
      }
      None
    }
  }.flatten
}

val processRegex = "mul\\((\\d+),(\\d+)\\)".r
def process(muls: List[String]): Int = {
  muls.map { mul =>
    mul match {
      case processRegex(x, y) => x.toInt * y.toInt
      case _ => 0
    }
  }.sum
}


