package aoc.day18

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


@main def aoc2024day18(): Unit = {
  println("Day18")
  day18()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day18(): Unit = {
  println(s"Started...")
  val lines = os.read(os.resource / "aoc/day18/day18_input")
  // Using.resource(scala.io.Source.fromResource(name))(source => source.mkString)

    val lines2 = """5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0"""

  def parse(input: String): (Board, List[Loc]) =
    (Vector.fill(71)("." * 71),
    input.linesIterator.collect {
      case s"$x,$y" => Loc(x.toLong, y.toLong)
    }.toList)

  val start = System.currentTimeMillis()

  // println(day18part1.tupled(parse(lines2)))
  println(day18part1.tupled(parse(lines))) // 1471826
  // println(day18part2(parse(lines2)))
  // println(day18part2(parse(lines))) // 
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}
// part 2 hint: https://github.com/scarf005/aoc-scala/blob/main/2024/day18.scala
def day18part1(in: Board, walls: List[Loc]): Long = {

  extension (b: Board)
    def draw() = 
      b.locations.foreach { l =>
       print(b(l))
       if (l.x > 0 && l.x % (b.width - 1) == 0) println()
     }

  val take = 1024
  val board = walls.take(take).foldLeft(in)((b, l) => b.update(l, '#'))
  board.draw()

  case class Route(loc: Loc, path: Vector[Loc]):
    def neighbours: Vector[Route] = 
      loc.adjacents.map(l => Route(l, path :+ l))

  given Priority[Int, Route] = _.path.size
  
  case class State(
    board: Board,
    take: Int,
    end: Loc,
    queue: PriorityQueue[Int, Route],
    visited: Set[Loc],
  ):
    def nextState: State =
      val (route, rest) = queue.dequeue

      val neighbours = route.neighbours.filter: next =>
        board.get(next.loc).exists(_ == '.') && !visited(next.loc)

      State(
        board,
        take,
        end,
        rest.enqueueAll(neighbours),
        visited ++ neighbours.map(_.loc),
      )

    def solution1: Option[Int] =
      Option(queue.firstValue).filter(_.loc == end).map{ r =>
        println(r.path.mkString(":"))
        val endBoard = r.path.foldLeft(board)((b, l) => b.update(l, 'O'))
        endBoard.draw()
        r.path.size
      }

    // merlinorg
  def part2: Loc =
    Iterator.iterate(take + 1 -> walls.size): (t, ws) =>
      if solve(State(walls.take((t + ws) / 2).foldLeft(in)((b, l) => b.update(l, '#')), t), solution2)
        then t -> ((t + ws) / 2) else ((t + ws) / 2) + 1 -> ws
    .flatMap: (t, ws) =>
      // (t == ws).option(walls(t - 1))
      Option.when(t == ws)(walls(t - 1))
    .next()

  def solution2(state: State): Option[Boolean] =
    Option.when(state.queue.isEmpty || state.queue.firstValue.loc == state.end)(state.queue.isEmpty)

  object State:
    def apply(board: Board, take: Int): State =
      val route = Route(board.nw, Vector(board.nw))
      new State(board, take, board.se, PriorityQueue(route), Set(board.nw))


  def solve[A](state: State, solution: State => Option[A]): A =
    Iterator
    .iterate(state)(_.nextState)
    .flatMap(solution)
    .next()

  // val r1 = Iterator
  //   .iterate(State()): state =>
  //     state.nextState
  //   .flatMap: state =>
  //     state.solution1
  //   .next()
  // Thread.sleep(500)
  // print(s"\u001b[${g.height + 1}A")


  val r2 = part2
  // println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def day18part2(grid: Grid[Char]): Long = {

  grid.draw()
  val r1 = 0
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

