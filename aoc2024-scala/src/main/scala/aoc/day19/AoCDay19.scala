package aoc.day19

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq
import scala.util.Using
import scala.collection.mutable.TreeSet
import scala.collection.concurrent.TrieMap


@main def aoc2024day19(): Unit = {
  println("Day19")
  day19()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}


def day19(): Unit = {
  println(s"Started...")
  val lines = os.read(os.resource / "aoc/day19/day19_input")
  // Using.resource(scala.io.Source.fromResource(name))(source => source.mkString)

    val lines2 = """r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb"""

  val start = System.currentTimeMillis()

  def parse(input: String) = {
    val parts = input.split("\n\n")
    val avail = parts(0).split(", ").toSeq
    val reqs = parts(1).linesIterator.toSeq
    (avail, reqs)
  }


  // println(day19part1 tupled (parse(lines2)))
  println(day19part1 tupled (parse(lines))) // 1471926
  // println(day19part2(parse(lines2)))
  // println(day19part2(parse(lines))) // 
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

def day19part1(avail: Seq[String], reqs: Seq[String]): Long = {


  println("Order: w, u, b, r, g")

  
  val memo = TrieMap[String, Long]("" -> 1)

  def countAll(req: String): Long = {
    if (req.size == 0)
      1L
    else
      avail
        .flatMap(t =>
          if (req.startsWith(t)) {
            Some(req.slice(t.length, req.length))
          } else None
        )
        .map(r => memo.getOrElseUpdate(r, countAll(r))).sum
  }
  val p = reqs.map(countAll)
  val r1 = p.count(_ > 0) // 322
  val r2 = p.sum // 715514563508258

  // Thread.sleep(500)
  // print(s"\u001b[${g.height + 1}A")
  // debug(s"robot: $robot -> ${res._2} dir: $d")

  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def day19part2(grid: Grid[Char]): Long = {

  grid.draw()
  val r1 = 0
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

