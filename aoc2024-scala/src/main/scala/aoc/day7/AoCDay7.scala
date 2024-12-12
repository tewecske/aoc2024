package aoc.day7

import os.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*


@main def aoc2024day7(): Unit = {
  println("Day7")
  day7()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day7(): Unit = {
  val lines = os.read.lines.stream(os.resource / "aoc/day7/day7_input")

    val lines2 =
"""""".split("[\n\r]+").toList
  val start = System.currentTimeMillis()
  // println(day7part1(lines.toList))
  // println(day7part1(lines2))
  // println(day7part2(lines.toList))
  // println(day7part2(lines2))
  println(day7part2inspect(lines.toList))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

def day7part1(lines: List[String]): Long = {
  val inputs = lines.map(_.split(":")).map { arr => (arr(0).toLong, arr(1).trim.split(" ").toList.map(_.toLong)) }
  println(s"inputs: $inputs")


  val ops = List("+", "*")

  // Claude AI :)
  @tailrec
  def combinations(
    remainingSlots: Long, 
    currentCombos: List[List[String]] = List(List.empty)
  ): List[List[String]] = {
    if (remainingSlots == 0) currentCombos
    else {
      val newCombos = for {
        combo <- currentCombos
        op <- ops
      } yield op +: combo

      combinations(remainingSlots - 1, newCombos)
    }
  }


  def compute(ss: List[String | Long], acc: Long, o: String): Long = {
    ss match {
      case Nil => acc
      case p :: Nil => 
        p match {
          case (p: String) => acc
          case (p: Long) => o match {
            case "+" => acc + p
            case "*" => acc * p
            case _ => acc
          }
        }

      case p :: tail =>
        p match {
          case (p: Long) => o match {
            case "+" => compute(tail, acc + p, "")
            case "*" => compute(tail, acc * p, "")
            case "" => compute(tail, p, "")
          }
          case (p: String) => p match {
            case "+" => compute(tail, acc, "+")
            case "*" => compute(tail, acc, "*")
          }
        }
    }
  }

  def possibleOps(req: Long, elems: List[Long], allOps: List[List[String]]): Long = {

    val ex = allOps.exists { cOps =>
      val ee = elems.zip(cOps).map { case (i, o) =>
        List(i, o)
      }
      // println(ee.flatten)
      val rr = compute(ee.flatten, 0, "")
      // println(s"$req == $rr")
      req == rr
    }
    if (ex) req else 0
  }


  inputs.map { input =>
    possibleOps(input._1, input._2, combinations(input._2.size))
  }.sum

}

def day7part2(lines: List[String]): Long = {
  val inputs = lines.map(_.split(":")).map { arr => (arr(0).toLong, arr(1).trim.split(" ").toList.map(_.toLong)) }
  println(s"inputs: $inputs")


  val ops = List("+", "*", "||")

  @tailrec
  def combinations(
    remSlots: Long, 
    currents: List[List[String]] = List(List.empty)
  ): List[List[String]] = {
    if (remSlots == 0) currents
    else {
      val newCombos = for {
        combo <- currents
        op <- ops
      } yield op +: combo

      combinations(remSlots - 1, newCombos)
    }
  }

  def compute(ss: List[String | Long], acc: Long, o: String): Long = {
    ss match {
      case Nil => acc
      case p :: Nil => 
        p match {
          case (p: String) => acc
          case (p: Long) => o match {
            case "+" => acc + p
            case "*" => acc * p
            case "||" => s"$acc$p".toLong
            case _ => acc
          }
        }

      case p :: tail =>
        p match {
          case (p: Long) => o match {
            case "+" => compute(tail, acc + p, "")
            case "*" => compute(tail, acc * p, "")
            case "||" => compute(tail, s"$acc$p".toLong, "")
            case "" => compute(tail, p, "")
          }
          case (p: String) => p match {
            case "+" => compute(tail, acc, "+")
            case "*" => compute(tail, acc, "*")
            case "||" => compute(tail, acc, "||")
          }
        }
    }
  }

  def possibleOps(req: Long, elems: List[Long], allOps: List[List[String]]): Long = {

    val ex = allOps.exists { cOps =>
      val ee = elems.zip(cOps).map { case (i, o) =>
        List(i, o)
      }
      // println(ee.flatten)
      val rr = compute(ee.flatten, 0, "")
      // println(s"$req == $rr")
      req == rr
    }
    if (ex) req else 0
  }


  inputs.par.map { input =>
    possibleOps(input._1, input._2, combinations(input._2.size))
  }.sum

}

// https://github.com/scarf005/aoc-scala/blob/main/2024/day07.scala
def day7part2inspect(lines: List[String]): Long = {
  def digits(x: Long) = math.log10(x.toDouble).toInt + 1

  extension (a: Long) {
    inline def ||(b: Long) = (a * math.pow(10, digits(b))).toLong + b
  }

  type Op = Long => Long => List[Long]

  def getCombo(xs: List[Long], next: Op) = {
    @tailrec def go(xs: List[Long], acc: List[Long] = List()): List[Long] =
      (xs, acc) match {
        case (Nil, _)       => acc
        case (y :: ys, Nil) => go(ys, List(y))
        case (y :: ys, _)   => go(ys, acc.flatMap(next(y)))
      }
    go(xs)
  }

def solve(xss: Vector[(Long, List[Long])], op: Op) =
  xss.filter((target, xs) => getCombo(xs, op).exists(_ == target)).map(_._1).sum

  val input = lines.collect { case s"$target: $xs" =>
    (target.toLong, xs.split(" ").map(_.toLong).toList)
  }.toVector

  // println(solve(input, (x) => (a) => List(a + x, a * x)))
  // println(solve(input, (x) => (a) => List(a + x, a * x, a || x)))
  solve(input, (x) => (a) => List(a + x, a * x, a || x))
}
