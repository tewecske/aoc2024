package aoc.day8

import os.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*


@main def aoc2024day8(): Unit = {
  println("Day8")
  day8()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day8(): Unit = {
  val lines = os.read.lines.stream(os.resource / "aoc/day8/day8_input")

    val lines2 =
"""............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............""".split("[\n\r]+").toList
  val start = System.currentTimeMillis()
  // println(day8part1(lines.toList))
  // println(day8part1(lines2))
  println(day8part2(lines.toList))
  // println(day8part2(lines2))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
  // Iterator.iterate
  // Vector.unfold
  //
  // case class Context(size: Size, antennas: Map[Char, Vector[Pos]]):
  //   def solve(fn: ((Pos, Pos)) => IterableOnce[Pos]) =
  //     antennas.values.flatMap(_.combinationsN(2).flatMap(fn)).toSet
  //   lazy val part1 =
  //     solve((a, b) => Vector(a + (a - b), b + (b - a))).filter(size(_)).size
  //   lazy val part2 = solve((a, b) =>
  //     (a deltas b).takeWhile(size(_)) ++ (b deltas a).takeWhile(size(_)),
  //   ).size
}

case class Size(height: Int, width: Int)
case class Grid(grid: Array[Array[Char]]) {
  val size = Size(grid.length, grid.head.length)
  val lookup = (for { 
    x <- 0 until size.height
    y <- 0 until size.width
    c = at(x, y)
    if c != '.'
  } yield (c, (x, y))).groupMap { case (c, (x, y)) => c } { case (c, xy) => xy }


  def inside(x: Int, y: Int): Boolean = !(x < 0 || y < 0 || x >= size.height || y >= size.width)
  def at(x: Int, y: Int): Char = grid(x)(y)

  def drawWith(xys: List[(Int, Int)]): Unit = {
    for { 
      x <- 0 until size.height
      y <- 0 until size.width
    } {
      if (xys.contains((x, y))) print('#')
      else print(at(x, y))
      if (y + 1 == size.width) println
    }
  }

}
def day8part1(lines: List[String]): Long = {
  val inputs = lines.map(_.toCharArray).toArray
  val grid = Grid(inputs)
  println(s"inputs: \n${inputs.map(_.mkString).mkString("\n")}")
  println(s"grid.lookup: \n${grid.lookup.mkString("\n")}")

  val grouped = grid.lookup.map((k, v) => (k, v.toList))

  extension (xy: (Int, Int)) {
    def minus(other: (Int, Int)): (Int, Int) = (xy._1 - other._1, xy._2 - other._2)
    def plus(other: (Int, Int)): (Int, Int) = (xy._1 + other._1, xy._2 + other._2)
    def distance(other: (Int, Int)): (Int, Int) = (0, 0)
  }

  def proj(xy1: (Int, Int), xy2: (Int, Int)): List[(Int, Int)] = {
    val d = xy1.minus(xy2)
    println(s"xy1: $xy1, xy2: $xy2, d: $d")
    val res = if (d == (0, 0)) Nil
    else {
      var r = List.empty[(Int, Int)]
      val xy1d = xy1.plus(d)
      val xy2d = xy2.minus(d)
      val xy1din = grid.inside.tupled(xy1d)
      val xy1dat = if (xy1din) grid.at.tupled(xy1d) else ""
      val xy2din = grid.inside.tupled(xy2d)
      val xy2dat = if (xy2din) grid.at.tupled(xy2d) else ""
      println(s"xy1d: $xy1d, in: $xy1din, at: $xy1dat xy2d: $xy2d, in: $xy2din, at: $xy2dat")
      // if (grid.inside.tupled(xy1d) && grid.at.tupled(xy1d) == '.') r = r :+ xy1d
      // if (grid.inside.tupled(xy2d) && grid.at.tupled(xy2d) == '.') r = r :+ xy2d
      if (grid.inside.tupled(xy1d)) r = r :+ xy1d
      if (grid.inside.tupled(xy2d)) r = r :+ xy2d
      r
    }
    println(s"res: $res")
    res
  }
  def check(c: Char, xys: List[(Int, Int)]): List[(Int, Int)] = {
    @tailrec def loop(xy: (Int, Int), rem: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
      rem match {
        case Nil => acc
        case head :: Nil => acc
        case head :: tail => loop(xy, tail, acc ++ proj(xy, head))
      }
    }
    xys.flatMap(xy => loop(xy, xys, List.empty))
  }
  val result = grouped.map { kv => 
    check.tupled(kv)
  }.flatten
  // grid.drawWith(Nil)
  // println
  // grid.drawWith(result.toList)
  result.toSet.size
}

def day8part2(lines: List[String]): Long = {
  val inputs = lines.map(_.toCharArray).toArray
  val grid = Grid(inputs)
  println(s"inputs: \n${inputs.map(_.mkString).mkString("\n")}")
  println(s"grid.lookup: \n${grid.lookup.mkString("\n")}")

  val grouped = grid.lookup.map((k, v) => (k, v.toList))

  extension (xy: (Int, Int)) {
    def minus(other: (Int, Int)): (Int, Int) = (xy._1 - other._1, xy._2 - other._2)
    def plus(other: (Int, Int)): (Int, Int) = (xy._1 + other._1, xy._2 + other._2)
    def distance(other: (Int, Int)): (Int, Int) = (0, 0)
  }

  def proj(xy1: (Int, Int), xy2: (Int, Int)): List[(Int, Int)] = {
    val d = xy1.minus(xy2)
    println(s"xy1: $xy1, xy2: $xy2, d: $d")

    def checkMulti1(xy1: (Int, Int), d: (Int, Int)): List[(Int, Int)] = {
      val xy1d = xy1.plus(d)
      val xy1din = grid.inside.tupled(xy1d)
      val xy1dat = if (xy1din) grid.at.tupled(xy1d) else ""
      println(s"xy1d: $xy1d, in: $xy1din, at: $xy1dat")
      if (grid.inside.tupled(xy1d)) xy1d :: checkMulti1(xy1d, d)
      else Nil
    }
    def checkMulti2(xy2: (Int, Int), d: (Int, Int)): List[(Int, Int)] = {
      val xy2d = xy2.minus(d)
      val xy2din = grid.inside.tupled(xy2d)
      val xy2dat = if (xy2din) grid.at.tupled(xy2d) else ""
      println(s"xy2d: $xy2d, in: $xy2din, at: $xy2dat")
      if (grid.inside.tupled(xy2d)) xy2d :: checkMulti2(xy2d, d)
      else Nil
    }

    val res = if (d == (0, 0)) Nil
    else {
      xy1 :: checkMulti1(xy1, d) ++ checkMulti2(xy1, d)
    }
    println(s"res: $res")
    res
  }
  def check(c: Char, xys: List[(Int, Int)]): List[(Int, Int)] = {
    @tailrec def loop(xy: (Int, Int), rem: List[(Int, Int)], acc: List[(Int, Int)]): List[(Int, Int)] = {
      rem match {
        case Nil => acc
        case head :: Nil => acc
        case head :: tail => loop(xy, tail, acc ++ proj(xy, head))
      }
    }
    xys.flatMap(xy => loop(xy, xys, List.empty))
  }
  val result = grouped.map { kv => 
    check.tupled(kv)
  }.flatten
  // grid.drawWith(Nil)
  // println
  // grid.drawWith(result.toList)
  result.toSet.size
}

