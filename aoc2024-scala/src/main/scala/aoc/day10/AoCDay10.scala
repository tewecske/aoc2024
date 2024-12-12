package aoc.day10

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq


@main def aoc2024day10(): Unit = {
  println("Day10")
  day10()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day10(): Unit = {
  println(s"Started...")
  val lines = os.read.lines.stream(os.resource / "aoc/day10/day10_input")

    val lines2 = """
89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732""".split("[\n\r]+").toList.tail
  val start = System.currentTimeMillis()
  println(day10part1(lines.toList))
  // println(day10part1(lines2))
  // println(day10part1lemon(lines.mkString))
  // println(day10part2(lines.mkString))
  // println(day10part2(lines2))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

def day10part1(line: List[String]): Long = {
  val grid = Grid(line.map(_.toCharArray().map(_.asDigit)).toArray)
  val startCoords = grid.find(_ == 0)
  println(startCoords.mkString)

  def single(coord: Coord, visited: Set[Coord] = Set.empty, trail: List[Coord] = List.empty): List[Coord] = {
    val v = grid.at(coord)
    val visit = coord.allCross.filter { crd =>
      val next = grid.at(crd) 
      !visited(crd) && v.exists(vv => next.exists(_ == vv + 1))
    }
    grid.at(coord) match {
      case Some(9) => coord +: trail
      case None => trail
      case _ => visit.flatMap(v => single(v, visited + coord, trail))
    }
  }

  val trails = startCoords.map(single(_))
  val r1 = trails.map(_.distinct.size).sum
  val r2 = trails.map(_.size).sum

  println(s"Result1: $r1")
  println(s"Result2: $r2")

  // grid.draw()
  0
}

def day10part2(line: List[String]): Long = {
  0
}

