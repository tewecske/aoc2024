package aoc.day2

import os.*
import scala.annotation.tailrec

@main def aoc2024day2(): Unit = {
  println("Day2")
  day2()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day2(): Unit = {
  val levels = os.read.lines.stream(os.resource / "aoc/day2/day2_input")
  // val levels = os.read.lines.stream(os.resource / "aoc/day2/day2_input_sample")

  // levels.foreach(println)


  // val valid = levels.fold(0) { (acc, line) =>
  //   val list = line.split(" ").toList.map(_.toInt)
  //   val res = check(list, 0, Unknown) > 0
  //   // println(s"Levels: $line = $res")
  //   if (res) acc + 1 else acc
  // }

  // println(s"Valid: $valid")

  val valid2 = levels.fold(0) { (acc, line) =>
    val list = line.split(" ").toList.map(_.toInt)
    val res = check2(list, 0, 0, Unknown) > 0
    debug(s"Levels: $line = $res")
    if (res) println(s"$line")
    if (res) acc + 1 else acc
  }

  println(s"Valid2: $valid2")
}

enum Direction {
  case Inc, Dec, IncSafe, DecSafe, Unknown, Almost
}

import Direction.*

@tailrec def check(list: List[Int], prev: Int, dir: Direction): Int = {
  list match {
    case Nil =>
      debug(s"Nil Nil $prev $dir")
      0
    case head :: Nil => 
      debug(s"Last $head :: Nil $prev $dir")
      val diff = head - prev
      if (dir == Inc && diff >= 1 && diff <= 3) {
        head
      } else if (dir == Dec && diff <= -1 && diff >= -3) {
        head
      } else {
        debug("FAIL SAFE head")
        0
      }
    case head :: tail =>
      debug(s"Full $head :: $tail $prev $dir")
      val diff = head - prev
      if (dir == Unknown || ((dir == Almost || dir == Inc) && (diff >= 1 && diff <= 3))) {
        check(tail, head, if (prev == 0) Almost else Inc)
      } else if (dir == Unknown || ((dir == Almost || dir == Dec) && (diff <= -1 && diff >= -3))) {
        check(tail, head, if (prev == 0) Almost else Dec)
      } else {
        debug("FAIL SAFE head :: tail")
        0
      }

  }
}

def check2(list: List[Int], prev1: Int, prev2: Int, dir: Direction): Int = {
  list match {
    case Nil =>
      debug(s"Nil Nil $prev1 $prev2 $dir")
      0
    case head :: Nil => 
      debug(s"Last2 $head :: Nil $prev1 $prev2 $dir")
      val diff1 = head - prev1
      val diff1Inc = diff1 >= 1 && diff1 <= 3
      val diff1Dec = diff1 <= -1 && diff1 >= -3
      val diff2 = if (prev2 == 0) 9 else head - prev2
      val diff2Inc = diff2 >= 1 && diff2 <= 3
      val diff2Dec = diff2 <= -1 && diff2 >= -3
      val prevDiff = prev1 - prev2
      val prevDiffInc = prevDiff >= 1 && prevDiff <= 3
      val prevDiffDec = prevDiff <= -1 && prevDiff >= -3
      if (dir == Inc && ((diff1Inc) || (diff2Inc))) {
        head
      } else if (dir == Dec && ((diff1Dec) || (diff2Dec))) {
        head
      } else {
        debug("LAST")
        head
      }
    case head :: tail =>
      val diff1 = head - prev1
      val diff1Inc = diff1 >= 1 && diff1 <= 3
      val diff1Dec = diff1 <= -1 && diff1 >= -3
      val diff2 = if (prev2 == 0) 9 else head - prev2
      val diff2Inc = diff2 >= 1 && diff2 <= 3
      val diff2Dec = diff2 <= -1 && diff2 >= -3
      val prevDiff = prev1 - prev2
      val prevDiffInc = prevDiff >= 1 && prevDiff <= 3
      val prevDiffDec = prevDiff <= -1 && prevDiff >= -3
      debug(s"Full2 $prev2 $prev1 $head :: $tail $dir diff1Inc $diff1Inc diff1Dec $diff1Dec diff2Inc $diff2Inc diff2Dec $diff2Dec prevDiffInc $prevDiffInc prevDiffDec $prevDiffDec")
      if (dir == Unknown || ((dir == Almost || dir == Inc) && diff1Inc)) {
        if (prev1 == 0 || prev2 == 0) {
          check2(tail, head, prev1, Almost)
        } else if (prevDiffInc) {
          check2(tail, head, prev1, Inc)
        } else {
          val r1 = check(tail, head, if (dir == Inc) Inc else Dec)
          val r2 = check(tail, head, if (dir == Dec) Dec else Inc)
          if (r1 != 0) r1
          else if (r2 != 0) r2
          else 0
        }
      } else if (dir == Unknown || ((dir == Almost || dir == Dec) && (diff1Dec))) {
        if (prev1 == 0 || prev2 == 0) {
          check2(tail, head, prev1, Almost)
        } else if (prevDiffDec) {
          check2(tail, head, prev1, Dec)
        } else {
          val r1 = check(tail, head, if (dir == Inc) Inc else Dec)
          val r2 = check(tail, head, if (dir == Dec) Dec else Inc)
          if (r1 != 0) r1
          else if (r2 != 0) r2
          else 0
        }
      // } else if (dir == Unknown || ((dir == Almost || dir == Inc) && (diff1 == 0 || (diff2Inc)))) {
      //   check(tail, head, if (prev1 == 0) Almost else Inc)
      } else if (dir == Unknown || ((dir == Almost || dir == Inc))) { // && (diff1 == 0 || (diff2Inc)))) {
        if (diff2Dec) {
          if (prevDiffInc && dir == Inc) {
            val r1 = check(head :: tail, prev1, Inc)
            val r2 = check(head :: tail, prev2, Inc)
            if (r1 != 0) r1
            else if (r2 != 0) r2
            else 0
          } else {
            val r1 = check(head :: tail, prev1, Dec)
            val r2 = check(head :: tail, prev2, Dec)
            if (r1 != 0) r1
            else if (r2 != 0) r2
            else 0
          }
        } else if (diff2Inc) {
          check(head :: tail, prev2, Inc)
        } else if (diff1Dec) {
          if (prevDiffInc && dir == Inc) {
            val r1 = check(head :: tail, prev1, Inc)
            val r2 = check(head :: tail, prev2, Inc)
            if (r1 != 0) r1
            else if (r2 != 0) r2
            else 0
          } else {
            val r1 = check(head :: tail, prev1, Dec)
            val r2 = check(head :: tail, prev2, Dec)
            if (r1 != 0) r1
            else if (r2 != 0) r2
            else 0
          }
        } else if (diff1Inc) {
          check(head :: tail, prev1, Inc)
        } else if (prev2 == 0) {
          val r1 = check(tail, head, dir)
          val r2 = check(tail, prev1, dir)
          if (r1 != 0) r1
          else if (r2 != 0) r2
          else 0
        } else {
          var r1 = check(head :: tail, prev1, dir)
          val r2 = check(head :: tail, prev2, dir)
          var r3 = check(prev1 :: tail, prev2, dir)
          if (r1 != 0) r1
          else if (r2 != 0) r2
          else if (r3 != 0) r3
          else 0
        }
      // } else if (dir == Unknown || ((dir == Almost || dir == Dec) && (diff1 == 0 || (diff2Dec)))) {
      //   check(tail, head, if (prev1 == 0) Almost else Dec)
      } else if (dir == Unknown || ((dir == Almost || dir == Dec))) { // && (diff1 == 0 || (diff2Dec)))) {
        if (diff2Inc) {
          if (prevDiffDec && dir == Dec) {
            val r1 = check(head :: tail, prev1, Dec)
            val r2 = check(head :: tail, prev2, Dec)
            if (r1 != 0) r1
            else if (r2 != 0) r2
            else 0
          } else {
            val r1 = check(head :: tail, prev1, Inc)
            val r2 = check(head :: tail, prev2, Inc)
            if (r1 != 0) r1
            else if (r2 != 0) r2
            else 0
          }
        } else if (diff2Dec) {
          check(head :: tail, prev2, Dec)
        } else if (diff1Inc) {
          if (prevDiffDec && dir == Dec) {
            val r1 = check(head :: tail, prev1, Dec)
            val r2 = check(head :: tail, prev2, Dec)
            if (r1 != 0) r1
            else if (r2 != 0) r2
            else 0
          } else {
            val r1 = check(head :: tail, prev1, Inc)
            val r2 = check(head :: tail, prev2, Inc)
            if (r1 != 0) r1
            else if (r2 != 0) r2
            else 0
          }
        } else if (diff1Inc) {
          check(head :: tail, prev1, Inc)
        } else if (prev2 == 0) {
          val r1 = check(tail, head, dir)
          val r2 = check(tail, prev1, dir)
          if (r1 != 0) r1
          else if (r2 != 0) r2
          else 0
        } else {
          var r1 = check(head :: tail, prev1, dir)
          val r2 = check(head :: tail, prev2, dir)
          var r3 = check(prev1 :: tail, prev2, dir)
          if (r1 != 0) r1
          else if (r2 != 0) r2
          else if (r3 != 0) r3
          else 0
        }
      } else {
        debug("FAIL")
        0
      }

  }
}

