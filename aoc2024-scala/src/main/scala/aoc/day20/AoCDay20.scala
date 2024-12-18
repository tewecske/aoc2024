package aoc.day20

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq
import scala.util.Using


@main def aoc2024day20(): Unit = {
  println("Day20")
  day20()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day20(): Unit = {
  println(s"Started...")
  val lines = os.read(os.resource / "aoc/day20/day20_input")
  // Using.resource(scala.io.Source.fromResource(name))(source => source.mkString)

    val lines2 = """###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############"""

  val start = System.currentTimeMillis()

  def parse(input: String): Grid[Char] = {
    Grid(input.split("\n").map(_.toCharArray))
  }


  println(day20part1(parse(lines2)))
  // println(day20part1(parse(lines))) // 1472026
  // println(day20part2(parse(lines2)))
  // println(day20part2(parse(lines))) // 
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}
// part 2 hint: https://github.com/scarf005/aoc-scala/blob/main/2024/day20.scala
def day20part1(grid: Grid[Char]): Long = {

  grid.draw()

  val S = grid.find(_ == 'S')(0)
  val E = grid.find(_ == 'S')(0)
  val rein = S







  // Thread.sleep(500)
  // print(s"\u001b[${g.height + 1}A")
  // debug(s"robot: $robot -> ${res._2} dir: $d")
  // g.draw()

  // grid.draw()

  val r1 = 0
  val r2 = 0
  debug(s"Result1: ${r1}")
  debug(s"Result2: $r2")

  0
}

def day20part2(grid: Grid[Char]): Long = {

  grid.draw()
  val r1 = 0
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}
