package aoc.day14

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq


@main def aoc2024day14(): Unit = {
  println("Day14")
  day14()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

case class Space(width: Int, height: Int)
case class Robot(x: Int, y: Int, vx: Int, vy: Int)
case class Quadrant(x: Int, y: Int, xx: Int, yy: Int)

def day14(): Unit = {
  println(s"Started...")
  val lines = os.read.lines.stream(os.resource / "aoc/day14/day14_input")

    val lines2 = """p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3"""
  val start = System.currentTimeMillis()

  def parse(input: String): List[Robot] = input.split("\n").map(_.linesIterator.toSeq match {
    case Seq(s"p=$x,$y v=$vx,$vy") =>
      (Robot(x.toInt, y.toInt, vx.toInt, vy.toInt))
  }
  ).toList


  println(day14part1(Space(101, 103), parse(lines.mkString("\n")))) // 39748
  // println(day14part1(Space(11, 7), parse(lines2)))
  // println(day14part1(parse2(lines.mkString("\n")))) // 74478585072604
  // println(day14part1(parse2(lines2)))
  // println(day14part2(lines.mkString))
  // println(day14part2(lines2))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}
// part 2 hint: https://github.com/scarf005/aoc-scala/blob/main/2024/day14.scala
def day14part1(space: Space, robots: List[Robot]): Long = {
  println(s"Space: $space")
  val iRobots = robots.zipWithIndex
  println(s"Robots: \n${iRobots.sortBy((r, _) => (r.y, r.x)).mkString("\n")}")

  def draw(robots: List[(Robot, Int)]) = {
    println("#" * space.width)
    val org = robots.map((r, _) => (r.y, r.x)).sorted.groupBy(identity).view.mapValues(_.size).toMap
    for {
      y <- 0 until space.height
      x <- 0 until space.width
    } {
      print(org.getOrElse((y, x), ".").toString)
      if (x + 1 == space.width) println()
    }
  }

  draw(iRobots)
  @tailrec def loop(times: Int, iRobots: List[(Robot, Int)]): List[(Robot, Int)] = {
    if (times <= 0) iRobots
    else {
      val newRobots = iRobots.map { case (robot, i) =>
        val newX = robot.x + robot.vx
        val newY = robot.y + robot.vy
        val endX = if (newX > space.width - 1) newX - space.width
        else if (newX < 0) newX + space.width
        else newX
        val endY = if (newY > space.height - 1) newY - space.height
        else if (newY < 0) newY + space.height
        else newY
        (Robot(endX, endY, robot.vx, robot.vy), i)
      }
      
      // val test = newRobots.groupMapReduce((r, _) => r.x)(_ => 1)(_ + _)
      // val lot = test.toList.filter(_._2 > 70).size
      val test = newRobots.groupBy((r, _) => r.x).view.mapValues(_.groupBy((r, _) => r.y).toList.sortBy(_._1)).toList
      val lot = test.filter { (_, l) =>
        l.map(_._1).foldLeft((0, 0)) { case ((acc, prev), i) => 
          if (acc > 20) (acc, i)
          else if (prev + 1 == i) (acc + 1, i)
          else (0, i)
        }._1 > 20
      }.size
      if (lot > 1) {
        println(times)
        draw(newRobots)
      }
      loop(times - 1, newRobots)
    }
  }

  val endRobots = loop(10000000, iRobots)
  draw(endRobots)
  

  // println(s"End robots: \n${endRobots.sortBy((r, _) => (r.y, r.x)).mkString("\n")}")

  val q1 = Quadrant(0, 0, (space.width / 2 - 1), (space.height / 2 - 1))
  val q2 = Quadrant((space.width / 2) + 1, 0, (space.width) - 1, (space.height / 2) - 1)
  val q3 = Quadrant(0, (space.height / 2) + 1, (space.width / 2) - 1, (space.height) - 1)
  val q4 = Quadrant((space.width / 2) + 1, (space.height / 2) + 1, (space.width) - 1, (space.height) - 1)
  println(s"q1: $q1, q2: $q2, q3: $q3, q4: $q4") 

  val r1 = Seq(q1, q2, q3, q4).map(q =>
    endRobots.filter((r, _) => r.x >= q.x && r.x <= q.xx && r.y >= q.y && r.y <= q.yy).size
    ).reduce(_ * _)

  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def day14part2(line: List[String]): Long = {
  0
}

