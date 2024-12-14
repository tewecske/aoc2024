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
  // println(day13part1(parse(lines2)))
  println(day13part1(parse2(lines.mkString("\n")))) // 74478585072604
  // println(day13part1(parse2(lines2)))
  // println(day13part2(lines.mkString))
  // println(day13part2(lines2))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}
// part 2 hint: https://github.com/scarf005/aoc-scala/blob/main/2024/day13.scala
def day13part1(list: List[ClawMachine]): Long = {

  val r1 = list.map { cm =>
    println(s"Claw Machine: ${cm}")
    // a * ax + b * bx = px , a * ay + b * by = py
    // a = (px - b * bx) / ax , a = (py - b * by) / ay
    // (px - b * bx) / ax = (py - b * by) / ay
    // (px - b * bx) * ay = (py - b * by) * ax
    // px * ay - b * bx * ay = py * ax - b * by * ax
    // px * ay - py * ax = b * bx * ay - b * by * ax
    // px * ay - py * ax = b * (bx * ay - by * ax)
    // b = (px * ay - py * ax) / (bx * ay - by * ax)
    // a = (px - b * bx) / ax
      
      val b1 = cm.px * cm.ay - cm.py * cm.ax
      val b2 = cm.bx * cm.ay - cm.by * cm.ax
      if (b2 != 0 && b1 % b2 == 0) {
        val b = b1 / b2
        val a1 = cm.px - b * cm.bx
        val a2 = cm.ax
        if (a2 != 0 && a1 % a2 == 0) {
          val a = a1 / a2
          a * 3 + b
        } else 0
      } else 0
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

