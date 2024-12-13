package aoc.day13

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq


@main def aoc2024day13(): Unit = {
  println("Day13")
  day13()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

case class ClawMachine(ax: Long, ay: Long, bx: Long, by: Long, px: Long, py: Long) {
  override def toString() = s"ax: $ax, ay: $ay, bx: $bx, by: $by, px: $px, py: $py"
}

def day13(): Unit = {
  println(s"Started...")
  val lines = os.read.lines.stream(os.resource / "aoc/day13/day13_input")

    val lines2 = """Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279"""
  val start = System.currentTimeMillis()

  def parse(input: String): List[ClawMachine] = input.split("\n\n").map(_.linesIterator.toSeq match {
    case Seq(
          s"Button A: X+$ax, Y+$ay",
          s"Button B: X+$bx, Y+$by",
          s"Prize: X=$px, Y=$py",
        ) => (ClawMachine(ax.toLong, ay.toLong, bx.toLong, by.toLong, px.toLong, py.toLong))
  }
  ).toList

  def parse2(input: String): List[ClawMachine] = input.split("\n\n").map(_.linesIterator.toSeq match {
    case Seq(
          s"Button A: X+$ax, Y+$ay",
          s"Button B: X+$bx, Y+$by",
          s"Prize: X=$px, Y=$py",
        ) => (ClawMachine(ax.toLong, ay.toLong, bx.toLong, by.toLong, px.toLong + 10000000000000L, py.toLong + 10000000000000L))
  }
  ).toList

  // println(day13part1(parse(lines.mkString("\n")))) // 39748
  println(day13part1(parse(lines2)))
  // println(day13part1(parse2(lines2)))
  // println(day13part2(lines.mkString))
  // println(day13part2(lines2))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

def day13part1(list: List[ClawMachine]): Long = {

  def findPossibleRecur(aMax: Long, bMax: Long, a: Long, b: Long, p: Long, acc: Seq[(Long, Long, Long)] = Seq.empty): Seq[(Long, Long, Long)] = {
    @tailrec def findA(aMax: Long, bMax: Long, a: Long, b: Long, p: Long, acc: Seq[(Long, Long, Long)] = Seq.empty): Seq[(Long, Long, Long)] = {
      val ra = aMax * a
      val rb = bMax * b
      val r = ra + rb
      println(s"ra: $ra rb: $rb r: $r")
      if (aMax == 1) acc
      else {
        val newAcc = if (r == p) acc :+ (aMax, bMax, r) else acc
        findA(aMax - 1, bMax, a, b, p, newAcc ++ findB(aMax - 1, bMax, a, b, p))
      }
    }
    @tailrec def findB(aMax: Long, bMax: Long, a: Long, b: Long, p: Long, acc: Seq[(Long, Long, Long)] = Seq.empty): Seq[(Long, Long, Long)] = {
      val ra = aMax * a
      val rb = bMax * b
      val r = ra + rb
      println(s"ra: $ra rb: $rb r: $r")
      if (bMax == 1) acc
      else {
        if (r == p) acc :+ (aMax, bMax, r)
        else findB(aMax, bMax - 1, a, b, p, acc)
      }
    }
    
    findA(aMax, bMax, a, b, p)
  }

  val r1 = list.map { cm =>
    println(s"Claw Machine: ${cm}")
    val axMax = cm.px/cm.ax
    val bxMax = cm.px/cm.bx
    val ayMax = cm.py/cm.ay
    val byMax = cm.py/cm.by

    println(s"axMax: $axMax, bxMax: $bxMax")
    val possibleX = findPossibleRecur(axMax, bxMax, cm.ax, cm.bx, cm.px)
    println(s"possibleX: ${possibleX.mkString(",")}")


    println(s"ayMax: $ayMax, byMax: $byMax")
    val possibleY = findPossibleRecur(ayMax, byMax, cm.ay, cm.by, cm.py)
    println(s"possibleY: ${possibleY.mkString(",")}")

    if (possibleX.isEmpty || possibleY.isEmpty) {
      // println("ERROR")
      0
    } else {
      val possibles = for {
        (ax, bx, _) <- possibleX
        (ay, by, _) <- possibleY
        if ax == ay && bx == by
      } yield (ax, bx)

      // println(s"possibles: ${possibles.mkString}")

      possibles.take(1).map(_ * 3 + _ * 1).sum
    }
  }.sum



  // val r1 = 0
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def day13part2(line: List[String]): Long = {
  0
}

