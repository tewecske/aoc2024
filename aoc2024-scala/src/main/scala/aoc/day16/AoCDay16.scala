package aoc.day16

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq
import scala.util.Using
import scala.collection.immutable.TreeMap


@main def aoc2024day16(): Unit = {
  println("Day16")
  day16()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day16(): Unit = {
  println(s"Started...")
  val lines = os.read(os.resource / "aoc/day16/day16_input").toString
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


  // println(day16part1(lines2))
  // println(day16part1(lines)) // 160624
  // println(day16part2(parse(lines2)))
  println(day16part2(lines)) // 
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

// https://scalacenter.github.io/scala-advent-of-code/2024/puzzles/day16
// https://github.com/merlinorg/aoc2024/blob/main/src/main/scala/Day16.scala

type Signum = -1 | 0 | 1
extension (signum: Signum)
  def reverse: Signum = if signum == 1 then -1 else if signum == -1 then 1 else 0

type Direction = (Signum, Signum)

val East: Direction = (1, 0)

extension (direction: Direction)
  def cw: Direction  = (direction(1).reverse, direction(0))
  def ccw: Direction = (direction(1), direction(0).reverse)

type Position = (Int, Int)

extension (position: Position)
  def +(direction: Direction): Position = (position(0) + direction(0), position(1) + direction(1))


type Maze = Array[String]

extension (maze: Maze)
  def findPosition(char: Char): Option[Position] =
    maze.zipWithIndex.collectFirst:
      case (row, y) if row.contains(char) => row.indexOf(char) -> y

  def apply(position: Position): Char = maze(position(1))(position(0))


type PriorityQueue[K, V] = TreeMap[K, Vector[V]]

trait Priority[K, V]:
  def priority(value: V): K

object PriorityQueue:
  def apply[K: Ordering, V](value: V)(using P: Priority[K, V]): PriorityQueue[K, V] =
    TreeMap(P.priority(value) -> Vector(value))

extension [K, V](queue: PriorityQueue[K, V])
  def enqueue(value: V)(using P: Priority[K, V]): PriorityQueue[K, V] =
    queue.updatedWith(P.priority(value)):
      case Some(values) => Some(values :+ value)
      case None         => Some(Vector(value))

  def enqueueAll(values: Iterable[V])(using P: Priority[K, V]): PriorityQueue[K, V] =
    values.foldLeft(queue)(_.enqueue(_))

  def dequeue: (V, PriorityQueue[K, V]) =
    val (priority, values) = queue.head
    if values.size == 1 then (values.head, queue - priority)
    else (values.head, queue + (priority -> values.tail))

  def firstValue: V = firstValues.head

  def firstValues: Vector[V] = queue.valuesIterator.next()
end extension

// part 1
// case class Reindeer(score: Int, pos: Position, dir: Direction):
//   def neighbours: Vector[Reindeer] = Vector(
//     Reindeer(score + 1, pos + dir, dir),
//     Reindeer(score + 1000, pos, dir.cw),
//     Reindeer(score + 1000, pos, dir.ccw)
//   )
// part 2
case class Reindeer(score: Int, pos: Position, dir: Direction, path: Vector[Position]):
  def neighbours: Vector[Reindeer] = Vector(
    Reindeer(score + 1, pos + dir, dir, path :+ (pos + dir)),
    Reindeer(score + 1000, pos, dir.cw, path),
    Reindeer(score + 1000, pos, dir.ccw, path)
  )
given Priority[Int, Reindeer] = _.score

case class ReindeerState(
  maze: Maze,
  end: Position,
  queue: PriorityQueue[Int, Reindeer],
  visited: Set[(Position, Direction)],
):
  def nextState: ReindeerState =
    val (reindeer, rest) = queue.dequeue

    val neighbours = reindeer.neighbours.filter: next =>
      maze(next.pos) != '#' && !visited(next.pos -> next.dir)

    ReindeerState(
      maze,
      end,
      rest.enqueueAll(neighbours),
      visited + (reindeer.pos -> reindeer.dir),
    )

  def solution1: Option[Int] =
    Option(queue.firstValue).filter(_.pos == end).map(_.score)

  def solution2: Option[Int] =
    Option.when(queue.firstValue.pos == end):
      queue.firstValues.filter(_.pos == end).flatMap(_.path).distinct.size

object ReindeerState:
  def apply(input: String): ReindeerState =
    val maze     = input.split("\n")
    val start    = maze.findPosition('S').get
    val end      = maze.findPosition('E').get
    val reindeer = Reindeer(0, start, East, Vector(start))
    new ReindeerState(maze, end, PriorityQueue(reindeer), Set.empty)


def day16part1(input: String): Int = {

  Iterator
    .iterate(ReindeerState(input)): state =>
      state.nextState
    .flatMap: state =>
      state.solution1
    .next()

}

def day16part2(input: String): Int = {

  Iterator
    .iterate(ReindeerState(input)): state =>
      state.nextState
    .flatMap: state =>
      state.solution2
    .next()

}


