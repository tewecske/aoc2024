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


  println(day21part1(parse(lines2)))
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

  def memoize[A1, A2, B](f: (A1, A2) => B): (A1, A2) => B =
    val cache = mutable.Map.empty[(A1, A2), B]
    (a1, a2) => cache.getOrElseUpdate((a1, a2), f(a1, a2))

  case class Search(keypad: Map[Loc, Char]):
    def search(start: Loc, finish: Loc): Option[State] = memoize(inner)(start, finish)

    def inner(start: Loc, finish: Loc): Option[State] =
      val visited = mutable.Set(start)
      def loop(queue: PriorityQueue[Int, State]): Option[State] =
        // println(s"QUEUE: ${queue.headOption}")
        if (queue.isEmpty) None
        else if (queue.firstValue.loc == finish) queue.firstValues.headOption
        else
          val (current, rest) = queue.dequeue
          val (adjacent1, adjacent2) = DirectionSymbols.partition((dir, _) => dir == Dir.NW)
          val adj1 = adjacent1.map((d, s) => (current.loc + d, s))
            .filter((loc, _) => keypad.get(loc).isDefined && !visited(loc)).map((loc, s) => State(loc, current.path :+ (loc, s)))
          val adj2 = adjacent2.map((d, s) => (current.loc + d, s))
            .filter((loc, _) => keypad.get(loc).isDefined && !visited(loc)).map((loc, s) => State(loc, current.path :+ (loc, s)))
          loop(rest.enqueueAll(adj2).enqueueAll(adj1))
      loop(PriorityQueue(State(start, Vector.empty))) // Not valid dir :)

  val numericSearch = Search(numericBoard)
  val directionalSearch = Search(directionalBoard)

  def numericMoves(code: Vector[Loc]): State =
    val resultRobot = code.foldLeft(State(numeric('A'), Vector.empty)) { (lastState, nextLoc) =>
      // println(s"robot1 lastState: $lastState, nextLoc: $nextLoc")
      val endState = numericSearch.search(lastState.loc, nextLoc).get
      val finalState = endState.copy(path = endState.path.appended((endState.path.last._1, 'A')))
      lastState.merge(finalState)
    }
    println(resultRobot.path.map(_._2).mkString)
    resultRobot

  def robot(prevResult: State): State =
    val inputRobot = prevResult.path.map(p => directional(p._2))
    val resultRobot = inputRobot.foldLeft(State(directional('A'), Vector.empty)) { (lastState, nextLoc) =>
      // println(s"robot2 lastState: $lastState, nextLoc: $nextLoc")
      val endState = directionalSearch.search(lastState.loc, nextLoc).get
      val finalState = endState.copy(path = endState.path.appended((endState.path.lastOption.map(_._1).getOrElse(lastState.loc), 'A')))
      lastState.merge(finalState)
    }
    println(resultRobot.path.map(_._2).mkString)
    resultRobot

// my: <A^A^^>AvvvA
// ex: <A^A>^^AvvvA
// my: v<<A>^>A<A>A<AA>vA^Av<AAA^>A
// ex: v<<A>>^A<A>AvA<^AA>A<vAAA>^A
// my: v<A<AA>^>AvA^<A>vA^Av<<A>^>AvA^Av<<A>^>AAvA<A^>A<A>Av<A<A>^>AAA<A>vA^A
// ex: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A


  // 029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
  //       v<A<AA>^>AvA^<A>vA^Av<<A>^>AvA^Av<<A>^>AAvA<A^>A<A>Av<A<A>^>AAA<A>vA^A
  // 980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
  // 179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
  // 456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
  // 379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A

  val r1 = codes.map((code, codeLocs) =>
    (code, robot(robot(numericMoves(codeLocs))))
    ).map { case (code, moves) =>
      val codeNum = code.replaceAll("^0|A", "")
      val movesLength = moves.path.size
      println(s"codeNum: $codeNum, movesLength: $movesLength")
      codeNum.toInt * movesLength
    }.sum

  // v<<A>>^A<A>AvA<^AA>A<vAAA>^A
  //
  // v<<A>^>A<A>A<AA>vA^Av<AAA^>A

  // Thread.sleep(500)
  // print(s"\u001b[${g.height + 1}A")
  // debug(s"robot: $robot -> ${res._2} dir: $d")

  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def day21part2(codes: Vector[Vector[Loc]]): Long = {

  val r1 = 0
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

