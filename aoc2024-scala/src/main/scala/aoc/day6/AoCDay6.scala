package aoc.day6

import os.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.io.BufferedSource

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
  val start = System.currentTimeMillis()
  // println(day6part1(lines.toList))
  // println(day6part1(lines2))
  // println(day6part2(lines.toList))
  // println(day6part2(lines2))
  // println(day6part2inspect(lines.toList))
  println(day6part2mam(lines.toList))
  // println(day6test(lines.toList))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
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

// https://github.com/scarf005/aoc-scala/blob/main/2024/day06.scala
def day6part2inspect(lines: List[String]): Unit = {

  extension (a: Boolean) inline def toInt = if a then 1 else 0

  extension [A](self: A) {
    inline def tap[B](f: A => B): A = { f(self); self }

    inline def pipe[B](f: A => B): B = f(self)

    inline def |>[B](f: A => B): B = f(self)
  }

  extension (n: Int) {
    inline infix def divmod(d: Int) = (n / d, n % d)
    inline infix def moddiv(d: Int) = (n % d, n / d)
  }


  final case class Size(width: Int, height: Int) {
    inline def contains(p: Pos) =
      0 <= p.x && p.x < width && 0 <= p.y && p.y < height
    inline def apply(p: Pos) = contains(p)
  }

  object Size {
    def apply[A](xs: Seq[Seq[A]]): Size = Size(xs(0).size, xs.size)
    def apply[A](xs: Array[Array[A]]): Size = Size(xs(0).size, xs.size)
  }

  final case class Pos(x: Int, y: Int) {
    def +(p: Pos) = Pos(x + p.x, y + p.y)
  }

  enum Dir {
    case Up, Down, Left, Right

    def delta = this match {
      case Up    => Pos(0, -1)
      case Down  => Pos(0, 1)
      case Left  => Pos(-1, 0)
      case Right => Pos(1, 0)
    }
  }

  extension (d: Dir) {
    def turnRight = d match {
      case Dir.Up    => Dir.Right
      case Dir.Down  => Dir.Left
      case Dir.Left  => Dir.Up
      case Dir.Right => Dir.Down
    }
  }

  case class Context(size: Size, pos: Pos)(walls: Set[Pos]) {

    def at(pos: Pos) = Option.when(size(pos))(walls(pos))

    def walk = {
      val visited = collection.mutable.Set.empty[Pos]

      @tailrec def loop(pos: Pos, dir: Dir): Unit = {
        visited.add(pos)
        val next = pos + dir.delta
        at(next) match {
          case None       => {}
          case Some(true) => loop(pos, dir.turnRight)
          case Some(_)    => loop(next, dir)
        }
      }

      loop(pos, Dir.Up); visited
    }

    def loops = {
      val corners = collection.mutable.Set.empty[(Pos, Dir)]

      @tailrec def loop(pos: Pos, dir: Dir): Boolean = {
        val next = pos + dir.delta
        at(next) match {
          case None => false
          case Some(true) =>
            (if corners.add(next -> dir) then loop(pos, dir.turnRight) else true)
          case Some(_) => loop(next, dir)
        }
      }

      loop(pos, Dir.Up)
    }
  }

  def solve(input: String) = {
    val grid = input.linesIterator.map(_.toCharArray).toArray
    val size = Size(grid)
    val pos = Pos.apply tupled (input.indexOf('^') moddiv (size.width + 1))
    val walls = (for {
      y <- grid.indices; x <- grid(0).indices
      if grid(y)(x) == '#'
    } yield Pos(x, y)).toSet

    val ctx = Context(size, pos)
    val visited = ctx(walls).walk
    val loops = visited.toArray.par.map { p => ctx(walls + p).loops.toInt }.sum
    visited.size -> loops
  }

  
  BufferedSource(os.read.inputStream(os.resource / "aoc/day6/day6_input")).mkString |> solve |> println
}


// https://github.com/makingthematrix/AdventOfCode2024/blob/main/src/main/scala/io/github/makingthematrix/AdventofCode2024/DaySix.scala
def day6part2mam(lines: List[String]): Unit = {

  inline def getChar(x: Int, y: Int)(using arr: Array[Char], len: Int): Option[Char] =
    if x < 0 || y < 0 || x >= len || y >= len then None else Some(arr(x * len + y))

  @tailrec def getPath(x: Int, y: Int, dir: (Int, Int) = (-1, 0), uniques: Set[(Int, Int)] = Set.empty)(using arr: Array[Char], len: Int): Set[(Int, Int)] = {
    val updated = if getChar(x, y).contains('.') then uniques + ((x, y)) else uniques
    getChar(x + dir._1, y + dir._2) match {
      case None      => updated
      case Some('.') => getPath(x + dir._1, y + dir._2, dir, updated)
      case Some('#') => getPath(x, y, (dir._2, -dir._1), updated)
    }
  }

  val Marks = Map('#' -> '1', '1' -> '2', '2' -> '3', '3' -> '4')

  @tailrec def isInfiniteLoop(x: Int, y: Int, dir: (Int, Int) = (-1, 0))(using arr: Array[Char], len: Int): Boolean = {
    getChar(x + dir._1, y + dir._2) match {
      case None      => false
      case Some('.') => isInfiniteLoop(x + dir._1, y + dir._2, dir)
      case Some('4') => true
      case Some(c)   => arr.update((x + dir._1) * len + y + dir._2, Marks(c)); isInfiniteLoop(x, y, (dir._2, -dir._1))
    }
  }

  val (array, len) = lines match { case lines => (lines.mkString.toCharArray, lines.head.length) }
  val start        = array.indexOf('^')
  val t@(x, y)     = (start / len, start % len)
  array.update(start, '.')
  // Part 1
  val path = getPath(x, y)(using array, len)
  println(s"Part 1: ${path.size}") // 5329
  // Part 2
  val res2 = (path - t).toArray.par
    .map { (a, b) => array.updated(a * len + b, '#') }
    .count { arr => isInfiniteLoop(x, y)(using arr, len) }
  println(s"Part 2: $res2") // 2162
}

