package aoc.day21

import os.*
import common.*
import aoc.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq
import scala.util.Using


@main def aoc2024day21(): Unit = {
  println("Day21")
  day21()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

// +---+---+---+
// | 7 | 8 | 9 |
// +---+---+---+
// | 4 | 5 | 6 |
// +---+---+---+
// | 1 | 2 | 3 |
// +---+---+---+
//     | 0 | A |
//     +---+---+
val numeric = Map(
  '7' -> Loc(0, 0),
  '8' -> Loc(1, 0),
  '9' -> Loc(2, 0),
  '6' -> Loc(0, 1),
  '5' -> Loc(1, 1),
  '4' -> Loc(2, 1),
  '3' -> Loc(0, 2),
  '2' -> Loc(1, 2),
  '1' -> Loc(2, 2),
  '0' -> Loc(1, 3),
  'A' -> Loc(2, 3),
  )

val numericBoard = numeric.map((k, v) => (v, k))

//     +---+---+
//     | ^ | A |
// +---+---+---+
// | < | v | > |
// +---+---+---+

val directional = Map(
  '^' -> Loc(1, 0),
  'A' -> Loc(2, 0),
  '<' -> Loc(0, 1),
  'v' -> Loc(1, 1),
  '>' -> Loc(2, 1),
  )

val directionalBoard = directional.map((k, v) => (v, k))

val DirectionSymbols = CardinalDirections.map {
  case Dir.N => (Dir.N, '^')
  case Dir.E => (Dir.E, '>')
  case Dir.W => (Dir.W, '<')
  case Dir.S => (Dir.S, 'v')
}

def day21(): Unit = {
  println(s"Started...")
  val lines = os.read(os.resource / "aoc/day21/day21_input")
  // Using.resource(scala.io.Source.fromResource(name))(source => source.mkString)

    val lines2 = """029A
980A
179A
456A
379A"""

  val start = System.currentTimeMillis()

  def parse(input: String): Vector[(String, Vector[Loc])] = {
    input.linesIterator.map(code => (code, code.map(numeric(_)).toVector)).toVector
  }


  // println(day21part1(parse(lines2)))
  println(day21part1article(lines)) // 
  // println(day21part1(parse(lines))) // 1472126
  // println(day21part2(parse(lines2)))
  // println(day21part2(parse(lines))) // 
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}
// part 2 hint: https://github.com/scarf005/aoc-scala/blob/main/2124/day21.scala
def day21part1(codes: Vector[(String, Vector[Loc])]): Long = {

  // codes.head.map

  case class State(loc: Loc, path: Vector[(Loc, Char)]):
    def merge(other: State) =
      copy(loc = other.loc, path = path.appendedAll(other.path))

  given Priority[Int, State] = _.path.size

  def memoize[A1, A2, A3, B](f: (A1, A2, A3) => B): (A1, A2, A3) => B =
    val cache = mutable.Map.empty[(A1, A2, A3), B]
    (a1, a2, a3) => cache.getOrElseUpdate((a1, a2, a3), f(a1, a2, a3))

  case class Search(keypad: Map[Loc, Char]):
    def search(start: Loc, finish: Loc, stage: Int): Set[State] = memoize(inner)(start, finish, stage)

    def inner(start: Loc, finish: Loc, stage: Int): Set[State] =
      val visited = mutable.Map(start -> 0)
      val paths = mutable.Set.empty[State]
      def loop(queue: PriorityQueue[Int, State]): Set[State] =
        // println(s"QUEUE: ${queue.headOption}")
        if (queue.isEmpty) paths.toSet
        else
          val (current, rest) = queue.dequeue
          if (current.loc == finish)
            // println(s"FINISH: $current")
            paths.addOne(current)
            loop(rest)
          else
            val adjs = DirectionSymbols.map((d, s) => (current.loc + d, s, current.path.size + 1))
              .filter((loc, _, dist) => keypad.get(loc).isDefined && (visited.get(loc).isEmpty || dist <= visited(loc))).map((loc, s, _) => State(loc, current.path :+ (loc, s)))
              visited.addAll(adjs.map(s => s.loc -> s.path.size))
            loop(rest.enqueueAll(adjs))
      loop(PriorityQueue(State(start, Vector.empty)))

  val numericSearch = Search(numericBoard)
  val directionalSearch = Search(directionalBoard)

  def numericMoves(code: Vector[Loc]): Set[State] =
    val resultRobot = code.foldLeft(Set(State(numeric('A'), Vector.empty))) { (lastStates, nextLoc) =>
      lastStates.flatMap { lastState =>
        // println(s"robot1 lastState: $lastState, nextLoc: $nextLoc")
        val endStates = numericSearch.search(lastState.loc, nextLoc, 0)
        // println(s"numericMoves endStates: ${endStates.size}")
        val finalStates = endStates.map(endState => endState.copy(path = endState.path.appended((endState.path.last._1, 'A'))))
        finalStates.map(finalState => lastState.merge(finalState))
      }
    }
    println(s"Numeric result: ${resultRobot.view.flatMap(_.path.map(_._2)).mkString}")
    resultRobot

  def robot(prevResults: Set[State], stage: Int): Set[State] =
    val inputsRobot = prevResults.map(prevResult => prevResult.path.map(p => directional(p._2)))
    val resultsRobot = inputsRobot.flatMap { inputRobot => 
      inputRobot.foldLeft(Set(State(directional('A'), Vector.empty))) { (lastStates, nextLoc) =>
        // println(s"robot2 lastState: $lastState, nextLoc: $nextLoc")
        lastStates.flatMap { lastState =>
          val endStates = directionalSearch.search(lastState.loc, nextLoc, stage)
          val finalStates = endStates.map(endState => endState.copy(path = endState.path.appended((endState.path.lastOption.map(_._1).getOrElse(lastState.loc), 'A'))))
          finalStates.map(finalState => lastState.merge(finalState))
        }
      }
    }
    println(s"Directional result: ${resultsRobot.view.flatMap(_.path.map(_._2)).mkString}")
    resultsRobot

  // val r1 = codes.take(1).map((code, codeLocs) =>
  //   (code, numericMoves(codeLocs))
  //   ).map { case (code, moves) =>
  //     val codeNum = code.replaceAll("^0|A", "")
  //     val movesLength = moves.map(_.path.size).min
  //     println(s"codeNum: $codeNum, movesLength: $movesLength")
  //     codeNum.toInt * movesLength
  //   }.sum

  // val r1 = codes.map((code, codeLocs) =>
  //   (code, robot(robot(numericMoves(codeLocs), 1), 2))
  //   ).map { case (code, moves) =>
  //     val codeNum = code.replaceAll("^0|A", "")
  //     val movesLength = moves.map(_.path.size).min
  //     println(s"codeNum: $codeNum, movesLength: $movesLength")
  //     codeNum.toInt * movesLength
  //   }.sum

  // v<<A>>^A<A>AvA<^AA>A<vAAA>^A
  //
  // v<<A>^>A<A>A<AA>vA^Av<AAA^>A

  // Thread.sleep(500)
  // print(s"\u001b[${g.height + 1}A")
  // debug(s"robot: $robot -> ${res._2} dir: $d")

  val r1 = 0
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def day21part1article(input: String) = {
  case class Pos(x: Int, y: Int):
    def +(other: Pos) = Pos(x + other.x, y + other.y)
    def -(other: Pos) = Pos(x - other.x, y - other.y)
    def projX = Pos(x, 0)
    def projY = Pos(0, y)

  val numericalKeypad = Map(
    '7' -> Pos(0, 0), '8' -> Pos(1, 0), '9' -> Pos(2, 0),
    '4' -> Pos(0, 1), '5' -> Pos(1, 1), '6' -> Pos(2, 1),
    '1' -> Pos(0, 2), '2' -> Pos(1, 2), '3' -> Pos(2, 2),
                      '0' -> Pos(1, 3), 'A' -> Pos(2, 3),
  )
  val numericalKeypadPositions = numericalKeypad.values.toSet

  val directionalKeypad = Map(
                      '^' -> Pos(1, 0), 'A' -> Pos(2, 0),
    '<' -> Pos(0, 1), 'v' -> Pos(1, 1), '>' -> Pos(2, 1),
  )
  val directionalKeypadPositions = directionalKeypad.values.toSet

  def minPathStep(from: Pos, to: Pos, positions: Set[Pos]): String =
    val shift = to - from
    val h = (if shift.x > 0 then ">" else "<") * shift.x.abs
    val v = (if shift.y > 0 then "v" else "^") * shift.y.abs
    val reverse = !positions(from + shift.projX) || (positions(from + shift.projY) && shift.x > 0)
    if reverse then v + h + 'A' else h + v + 'A'

  def minPath(input: String, isNumerical: Boolean = false): String =
    val keypad = if isNumerical then numericalKeypad else directionalKeypad
    val positions = if isNumerical then numericalKeypadPositions else directionalKeypadPositions
    (s"A$input").map(keypad).sliding(2).map(p => minPathStep(p(0), p(1), positions)).mkString

  def part1(input: String): Long =
    input
      .linesIterator
      .filter(_.nonEmpty)
      .map: line => // 029A
        val path1 = minPath(line, isNumerical = true) // <A^A^^>AvvvA
        val path2 = minPath(path1) // v<<A>>^A<A>A<AAv>A^A<vAAA^>A
        val path3 = minPath(path2) // <vA<AA>>^AvAA<^A>Av<<A>>^AvA^Av<<A>>^AA<vA>A^A<A>Av<<A>A^>AAA<Av>A^A
        val num = line.init.toLong // 29
        val len = path3.length() // 68
        len * num // 211930
      .sum

  println(s"Part1: ${part1(input)}")

  val cache = collection.mutable.Map.empty[(Pos, Pos, Int, Int), Long]
  def minPathStepCost(from: Pos, to: Pos, level: Int, maxLevel: Int): Long =
    cache.getOrElseUpdate((from, to, level, maxLevel), {
      val positions = if level == 0 then numericalKeypadPositions else directionalKeypadPositions
      val shift = to - from
      val h = (if shift.x > 0 then ">" else "<") * shift.x.abs
      val v = (if shift.y > 0 then "v" else "^") * shift.y.abs
      val reverse = !positions(from + shift.projX) || (positions(from + shift.projY) && shift.x > 0)
      val res = if reverse then v + h + 'A' else h + v + 'A'
      if level == maxLevel then res.length() else minPathCost(res, level + 1, maxLevel)
    })

  def minPathCost(input: String, level: Int, maxLevel: Int): Long =
    val keypad = if level == 0 then numericalKeypad else directionalKeypad
    (s"A$input").map(keypad).sliding(2).map(p => minPathStepCost(p(0), p(1), level, maxLevel)).sum

  def part2(input: String): Long =
    input
      .linesIterator
      .filter(_.nonEmpty)
      .map(line => minPathCost(line, 0, 25) * line.init.toLong)
      .sum

  println(s"Part2: ${part2(input)}")
}

def day21part2(codes: Vector[Vector[Loc]]): Long = {

  val r1 = 0
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

