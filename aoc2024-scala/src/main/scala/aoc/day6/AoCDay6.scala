package aoc.day6

import os.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn

@main def aoc2024day6(): Unit = {
  println("Day6")
  day6()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day6(): Unit = {
  val lines = os.read.lines.stream(os.resource / "aoc/day6/day6_input")

    val lines2 =
"""....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...""".split("[\n\r]+").toList
  // println(day6part1(lines.toList))
  // println(day6part1(lines2))
  println(day6part2(lines.toList))
  // println(day6part2(lines2))
  // println(day6test(lines.toList))
}

def day6test(lines: List[String]): Int = {
  val input = lines.map(_.split("").map(_.toInt).toList)
  val m = input.size
  val n = input(0).size
  println(s"m: $m n: $n")
  // First set of lines
  println("AAA")
  println("BBB")
  println("CCC")

  StdIn.readLine()
  // Move cursor back 3 lines 
  print("\u001b[3A")

  // Overwrite with new lines
  println("XXX")
  println("XXX")
  println("XXX")

  0
}

def day6part1(lines: List[String]): Int = {
  val input = lines.map(_.split("").toList)
  val m = input.size
  val n = input(0).size

  val dirs = List("^", ">", "v", "<")
  var dir = "^"
  var initialState = input.to(ListBuffer).map(_.to(ListBuffer))
  var state = initialState
  var pos = (0, 0)
  var run = true
  var counter = mutable.Set.empty[(Int, Int)]
  def changeDir() = {
    val newDir = dirs.indexOf(dir) + 1
    if (newDir >= dirs.size) {
      println(s"changeDir ${dirs(0)}")
      dir = dirs(0)
    } else {
      println(s"changeDir ${dirs(newDir)}")
      dir = dirs(newDir)
    }
  }

  def printState(): Unit = 
    println(s"m: $m n: $n counter: ${counter.size} dir: $dir pos: $pos run: $run")

  def draw(): Unit = {
    printState()
    for {
      x <- 0 to m-1
    } {
      println(state(x).mkString(""))
    }
    Thread.sleep(100)

    (dir, pos) match {
      case ("^", (x, y)) if x-1 < 0 =>
      case (">", (x, y)) if y+1 >= state(x).size =>
      case ("v", (x, y)) if x+1 >= state.size =>
      case ("<", (x, y)) if x-1 < 0 =>
      case _ => print(s"\u001b[${m+1}A")
    }
  }

  def endGame(): Unit = {
    run = false
    draw()
    Thread.sleep(1000)
    // printState()
    println("THE END")
  }

  def mark(x: Int, y: Int): Unit = {
    state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer("X"), 1)), 1)
    counter.add((x, y))
  }

  def move(): Unit = {
    (dir, pos) match {
      case ("^", (x, y)) if x-1 < 0 =>
        println("^ oopsie")
        mark(x, y)
        endGame()
      case ("^", (x, y)) if state(x-1)(y) != "#" =>
        // println("UP")
        mark(x, y)
        state.patchInPlace(x-1, List(state(x-1).patchInPlace(y, ListBuffer(dir), 1)), 1)
        pos = (x-1, y)
      case ("^", (x, y)) =>
        changeDir()
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer(dir), 1)), 1)

      case (">", (x, y)) if y+1 >= state(x).size =>
        println("> oopsie")
        mark(x, y)
        endGame()
      case (">", (x, y)) if state(x)(y+1) != "#" =>
        // println("RIGHT")
        mark(x, y)
        state.patchInPlace(x, List(state(x).patchInPlace(y+1, ListBuffer(dir), 1)), 1)
        pos = (x, y+1)
      case (">", (x, y)) =>
        changeDir()
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer(dir), 1)), 1)

      case ("v", (x, y)) if x+1 >= state.size =>
        println("v oopsie")
        mark(x, y)
        endGame()
      case ("v", (x, y)) if state(x+1)(y) != "#" =>
        // println("DOWN")
        mark(x, y)
        state.patchInPlace(x+1, List(state(x+1).patchInPlace(y, ListBuffer(dir), 1)), 1)
        pos = (x+1, y)
      case ("v", (x, y)) =>
        changeDir()
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer(dir), 1)), 1)

      case ("<", (x, y)) if y-1 < 0 =>
        println("< oopsie")
        mark(x, y)
        endGame()
      case ("<", (x, y)) if state(x)(y-1) != "#" =>
        // println("LEFT")
        mark(x, y)
        state.patchInPlace(x, List(state(x).patchInPlace(y-1, ListBuffer(dir), 1)), 1)
        pos = (x, y-1)
      case ("<", (x, y)) =>
        changeDir()
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer(dir), 1)), 1)
      case (_, (_, _)) => 
        println("ERROR")
        endGame()
    }
  }

  def loop(): Unit = {
    while (run) {
      // state.map(_.mkString("")).foreach(println)
      draw()
      move()
    }
  }


  def process(): Unit = {
    for {
      x <- 0 to m-1
      y <- 0 to n-1
    } {
      val px = state(x)(y)
      if (dirs.contains(px)) pos = (x, y)
    }
    loop()
  }


  process()

  0
}

def day6part2(lines: List[String]): Int = {
  val input = lines.map(_.split("").toList)
  val m = input.size
  val n = input(0).size

  val dirs = List("^", ">", "v", "<")
  var dir = "^"
  var initialState = input.to(ListBuffer).map(_.to(ListBuffer))
  var state = input.to(ListBuffer).map(_.to(ListBuffer))
  var pos = (0, 0)
  var run = true
  var counter = mutable.Set.empty[(Int, Int, String)]
  var stuck = 0
  def changeDir() = {
    val newDir = dirs.indexOf(dir) + 1
    if (newDir >= dirs.size) {
      // println(s"changeDir ${dirs(0)}")
      dir = dirs(0)
    } else {
      // println(s"changeDir ${dirs(newDir)}")
      dir = dirs(newDir)
    }
  }

  def printState(): Unit = 
    println(s"m: $m n: $n stuck: $stuck counter: ${counter.size} dir: $dir pos: $pos run: $run")

  def draw(): Unit = {
    printState()
    for {
      x <- 0 to m-1
    } {
      println(state(x).mkString(""))
    }
    Thread.sleep(10)

    (dir, pos) match {
      case ("^", (x, y)) if x-1 < 0 =>
      case (">", (x, y)) if y+1 >= state(x).size =>
      case ("v", (x, y)) if x+1 >= state.size =>
      case ("<", (x, y)) if x-1 < 0 =>
      case _ => print(s"\u001b[${m+1}A")
    }
  }

  def endGame(): Unit = {
    run = false
    printState()
    // draw()
    // Thread.sleep(1000)
    // printState()
    // println("THE END")
  }

  def mark(x: Int, y: Int): Unit = {
    state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer("X"), 1)), 1)
  }
  def count(x: Int, y: Int, dir: String): Unit = {
    val cs = counter.size
    counter.add((x, y, dir))
    if (cs == counter.size) {
      stuck = stuck + 1
      run = false
    }
  }

  def move(): Unit = {
    (dir, pos) match {
      case ("^", (x, y)) if x-1 < 0 =>
        println("^ oopsie")
        mark(x, y)
        count(x, y, dir)
        endGame()
      case ("^", (x, y)) if !state(x-1)(y).matches("[#O]") =>
        // println("UP")
        mark(x, y)
        count(x, y, dir)
        state.patchInPlace(x-1, List(state(x-1).patchInPlace(y, ListBuffer(dir), 1)), 1)
        pos = (x-1, y)
      case ("^", (x, y)) =>
        changeDir()
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer(dir), 1)), 1)

      case (">", (x, y)) if y+1 >= state(x).size =>
        println("> oopsie")
        mark(x, y)
        count(x, y, dir)
        endGame()
      case (">", (x, y)) if !state(x)(y+1).matches("[#O]") =>
        // println("RIGHT")
        mark(x, y)
        count(x, y, dir)
        state.patchInPlace(x, List(state(x).patchInPlace(y+1, ListBuffer(dir), 1)), 1)
        pos = (x, y+1)
      case (">", (x, y)) =>
        changeDir()
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer(dir), 1)), 1)

      case ("v", (x, y)) if x+1 >= state.size =>
        println("v oopsie")
        mark(x, y)
        count(x, y, dir)
        endGame()
      case ("v", (x, y)) if !state(x+1)(y).matches("[#O]") =>
        // println("DOWN")
        mark(x, y)
        count(x, y, dir)
        state.patchInPlace(x+1, List(state(x+1).patchInPlace(y, ListBuffer(dir), 1)), 1)
        pos = (x+1, y)
      case ("v", (x, y)) =>
        changeDir()
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer(dir), 1)), 1)

      case ("<", (x, y)) if y-1 < 0 =>
        println("< oopsie")
        mark(x, y)
        count(x, y, dir)
        endGame()
      case ("<", (x, y)) if !state(x)(y-1).matches("[#O]") =>
        // println("LEFT")
        mark(x, y)
        count(x, y, dir)
        state.patchInPlace(x, List(state(x).patchInPlace(y-1, ListBuffer(dir), 1)), 1)
        pos = (x, y-1)
      case ("<", (x, y)) =>
        changeDir()
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer(dir), 1)), 1)
      case (_, (_, _)) => 
        println("ERROR")
        endGame()
    }
  }

  def loop(): Unit = {
    while (run) {
      // state.map(_.mkString("")).foreach(println)
      // draw()
      move()
    }
  }


  def process(): Unit = {
    for {
      x <- 0 to m-1
      y <- 0 to n-1
    } {
      val px = state(x)(y)
      if (dirs.contains(px)) pos = (x, y)
    }
    loop()
  }


  for {
    x <- 0 to m-1
    y <- 0 to n-1
  } {
    dir = "^"
    state = input.to(ListBuffer).map(_.to(ListBuffer))
    run = true
    counter.clear()
    (x, y) match {
      case (x, y) if state(x)(y) == "#" =>
      case (x, y) if dirs.contains(state(x)(y)) =>
      case (x, y) => 
        state.patchInPlace(x, List(state(x).patchInPlace(y, ListBuffer("O"), 1)), 1)
    }
    process()
  }

  0
}

