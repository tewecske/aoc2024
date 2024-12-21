package aoc.day20

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable
import scala.util.Using
import aoc.*
import scala.collection.immutable.Queue


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
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############"""

  val start = System.currentTimeMillis()

  def parse(input: String): Board = {
    input.linesIterator.toVector
  }


  // println(day20part1(parse(lines2)))
  // println(day20part1(parse(lines))) // 1452
  // println(day20part2(parse(lines2)))
  println(day20part2(parse(lines))) // 
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}
// part 2 hint: https://github.com/scarf005/aoc-scala/blob/main/2024/day20.scala
def day20part1(in: Board): Long = {


  in.draw()
  val start = in.find('S')
  val finish = in.find('E')
  val board = in.update(start, '.').update(finish, '.')
  println(start)
  println(finish)

  val possible = board.locations
    // makes collect safe
    .filter(l => l.x >=< (1, board.width - 1) && l.y >=< (1, board.height - 1))
    .collect { 
      case l if l.horizontals.count(board(_) == '.') >= 2 => (l, HorizontalDirections)
      case l if l.verticals.count(board(_) == '.') >= 2 => (l, VerticalDirections)
    }

  case class State(loc: Loc, path: Vector[Loc]):
    def adjacents = loc.adjacents.map(l => State(l, path :+ l))

  case class Search(board: Board):
    def search(start: Loc, finish: Loc) =
      val startState = State(start, Vector(start))
      dijkstra(start, finish, Queue(startState), mutable.Set(start))

    @tailrec private def dijkstra(start: Loc, finish: Loc, queue: Queue[State], visited: mutable.Set[Loc]): Option[State] = 
      if (queue.isEmpty || queue.head.loc == finish) queue.headOption
      else
        val (head, tail) = queue.dequeue
        val adjacents = head.adjacents
          .filter(next => board.get(next.loc).exists(c => c == '.' || c == 'E') && !visited(next.loc))
        dijkstra(start, finish, tail.enqueueAll(adjacents), visited ++ adjacents.map(_.loc))


  val search = Search(board)
  val finishState = search.search(start, finish)
  val path = finishState.toVector.flatMap(_.path)


  // board.locations.foreach { l =>
  //   print(path.collectFirst { case p if p == l => 'O' }.getOrElse(board(l)))
  //   if (l.x == board.width - 1) println
  // }

  // board.locations.foreach { l =>
  //   print(possible.collectFirst { case (p, _) if p == l => 'X' }.getOrElse(board(l)))
  //   if (l.x == board.width - 1) println
  // }

  val pathIndex = path.zipWithIndex.toMap
  val distances = possible.map { case (pl, adjs) =>
    (pl, adjs.map(dir => pathIndex(pl + dir)).reduce((a, b) => Math.abs(b - a) - 2))
  }

  // println(distances.groupBy(_._2).view.mapValues(_.size).toVector.sorted.mkString("\n"))
  val r1 = distances.groupBy(_._2).view.mapValues(_.size).toVector.filter(_._1 >= 100).map(_._2).sum


  // Thread.sleep(500)
  // print(s"\u001b[${g.height + 1}A")
  // debug(s"robot: $robot -> ${res._2} dir: $d")

  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def day20part2(in: Board): Long = {


  in.draw()
  val start = in.find('S')
  val finish = in.find('E')
  val clean = in.update(start, '.').update(finish, '.')
  val board = clean.locations
    .filter(l => l.x == 0 || l.y == 0 || l.x == clean.width - 1 || l.y == clean.height - 1)
    .foldLeft(clean)((clean, l) => clean.update(l, 'X'))
  board.draw()

  println(start)
  println(finish)

  val possible = board.locations
    // makes collect safe
    .filter(l => l.x >=< (1, board.width - 1) && l.y >=< (1, board.height - 1))
    .collect { 
      case l => CardinalDirections.map(d => (l, d)).filter((l, d) => board(l + d) == '#')
    }.flatten

  case class State(loc: Loc, path: Vector[Loc]):
    def adjacents = loc.adjacents.map(l => State(l, path :+ l))

  case class Search(board: Board):
    def search(start: Loc, finish: Loc) =
      val startState = State(start, Vector(start))
      dijkstra(start, finish, Queue(startState), mutable.Set(start))

    @tailrec private def dijkstra(start: Loc, finish: Loc, queue: Queue[State], visited: mutable.Set[Loc]): Option[State] = 
      if (queue.isEmpty || queue.head.loc == finish) queue.headOption
      else
        val (head, tail) = queue.dequeue
        val adjacents = head.adjacents
          .filter(next => board.get(next.loc).exists(c => c == '.' || c == 'E') && !visited(next.loc))
        dijkstra(start, finish, tail.enqueueAll(adjacents), visited ++ adjacents.map(_.loc))


  val search = Search(board)
  val finishState = search.search(start, finish)
  val path = finishState.toVector.flatMap(_.path).zipWithIndex.map((l, i) => (i, l)).toMap
  // println(path)
  println(path.size)
  val cheats = 20
  val picos = 100

  val results = for {
    curr <- 0 to path.size
    dest <- curr + picos to path.size
    if curr < dest
    currLoc <- path.get(curr)
    destLoc <- path.get(dest)
    distance = currLoc.manhattan(destLoc)
    if (distance <= cheats) && (dest - curr - distance >= picos)
  } yield {
    // println(s"curr: $currLoc [$curr], dest: $destLoc [$dest] diff: ${dest-curr} dist: $distance")
    dest - curr
    // distance
  }

  // val r2 = results.count(_ == true)
  val rr = results.groupBy(identity).view.mapValues(_.size).toVector.sorted
  // println(rr.mkString("\n"))
  
  val r3 = results.size
  println(r3)


  // board.locations.foreach { l =>
  //   print(path.collectFirst { case p if p == l => 'O' }.getOrElse(board(l)))
  //   if (l.x == board.width - 1) println
  // }

  // board.locations.foreach { l =>
  //   print(possible.collectFirst { case (p, _) if p == l => 'X' }.getOrElse(board(l)))
  //   if (l.x == board.width - 1) println
  // }

  // val pathIndex = path.zipWithIndex.toMap
  // val distances = possible.map { case (pl, adjs) =>
  //   (pl, adjs.map(dir => pathIndex(pl + dir)).reduce((a, b) => Math.abs(b - a) - 2))
  // }

  // println(distances.groupBy(_._2).view.mapValues(_.size).toVector.sorted.mkString("\n"))
  // val r1 = distances.groupBy(_._2).view.mapValues(_.size).toVector.filter(_._1 >= 100).map(_._2).sum


  // Thread.sleep(500)
  // print(s"\u001b[${g.height + 1}A")
  // debug(s"robot: $robot -> ${res._2} dir: $d")

  val r2 = part2merlinorg(in)
  val r1 = 0
  // val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def part2merlinorg(maze: Vector[String]) = {

  val start = maze.find('S')
  val picos = 100
  val cheat = 20

  val path = Vector((start, 0)) ++ Iterator.unfold((start, start, 1)): (prev, cur, steps) =>
    cur.adjacents
      .find(loc => !maze.is(loc, '#') && loc != prev)
      .map(loc => ((loc, steps), (cur, loc, steps + 1)))

  path.tails.foldCollect:
    case (loc0: Loc, dst0: Int) +: tail =>
      tail.drop(picos).count: (loc1, dst1) =>
        val dist = loc0.manhattan(loc1)
        (dist <= cheat) && (dst1 - dst0 - dist >= picos)

}
