package aoc.day11

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq


@main def aoc2024day11(): Unit = {
  println("Day11")
  day11()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day11(): Unit = {
  println(s"Started...")
  val lines = os.read.lines.stream(os.resource / "aoc/day11/day11_input")

    val lines2 = """125 17"""
  val start = System.currentTimeMillis()
  println(day11part1(lines.toList.head.split(" ").map(_.toLong)))
  // println(day11part1(lines2.split(" ").map(_.toLong)))
  // println(day11part2(lines.mkString))
  // println(day11part2(lines2))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

def day11part1(line: Array[Long]): Long = {

  def memoize[A, B](f: A => B): A => B = {
    val cache = mutable.Map.empty[A, B]
    (a: A) => cache.getOrElseUpdate(a, f(a))
  }

  def mutate(stone: Long): Vector[Long] = {
    (stone, stone.digits) match {
      case (0L, _) => Vector(1L)
      case (i, d) if d % 2 == 0 => 
        val s = i.split
        Vector(s._1, s._2)
      case (i, _) => Vector(i * 2024)
    }
  }

  def fix(v: Vector[Long], m: mutable.Map[Long, Long]): mutable.Map[Long, Long] = {
    val mImm = m.toMap
    v.foldLeft(m){ case (acc, l) =>
      // println(s"mImm($l): ${mImm.get(l)} acc($l) ${acc.getOrElse(l, 0L)}")
      acc.update(l, acc.getOrElse(l, 0L) + mImm.getOrElse(l, 1L))
      // acc.update(l, acc.getOrElse(l, 0L) + 1L)
      acc 
    }
    m
  }

  def fixMap(v: mutable.Map[Long, Long], m: mutable.Map[Long, Long]): mutable.Map[Long, Long] = {
    val mImm = m.toMap
    v.foldLeft(m){ case (acc, (l, size)) => 
      // println(s"mImm($l): ${mImm.get(l)} acc($l) ${acc.getOrElse(l, 0L)} size: $size")
      acc.update(l, mImm.getOrElse(l, 0L) + size)
      acc
    }
    m
  }

  def blink(stones: Vector[Long]) = {
    stones.flatMap(mutate)
  }

  def blinkMap(stones: mutable.Map[Long, Long]) = {
    stones.keys.foreach(k => fix(mutate(k), stones))
    stones
  }

  def times[A](n: Int, f: A => A, value: A): A = {
    if (n <= 0) value
    else times(n - 1, f, f(value))
  }
  // memoize(
  val memo = memoize(blink)
  println(s"line: ${line.mkString(",")}")
  val r1 = line.toVector.flatMap (s => times(25, memo, Vector(s)))


  val allMap = mutable.Map.empty[Long, Long]
  fix(line.toVector, allMap)

  (1 to 75).foreach { ii =>
    println(s"ii: $ii")
    val tempMaps = mutable.ListBuffer.empty[mutable.Map[Long, Long]]
    val allMapImm = allMap.toMap
    for {
      (mapK, mapV) <- allMapImm
      } {
        val tempMap = mutable.Map.empty[Long, Long]
        fix(memo(Vector(mapK)), tempMap)
        tempMap.mapValuesInPlace((k ,v) => v * mapV)
        // println(s"$mapK -> tempMap: ${tempMap.toList.sorted.mkString(",")}")
        tempMaps.addOne(tempMap)
      }
    allMap.clear()
    tempMaps.foreach { tempMap =>
      fixMap(tempMap, allMap)
    }
    // println(s"allMap: ${allMap.toList.sorted.mkString(",")}")
  }


  // val rMap = r1.foldLeft(mutable.Map.empty[Long, Long]){ case (acc, l) => acc.update(l, acc.getOrElse(l, 0L) + 1L); acc }
  println(allMap.size)
  // println(rMap.mkString(","))
  // val r1 = r0.map ( s => times(25, memo, Vector(s)).length.toLong).sum
  // val r1b = rMap.map { case (s, t) => times(5, memo, Vector(s)).length.toLong * t }.sum

  // val r1 = times(25, blink, line.toVector)
  val r2 = allMap.values.sum





  // println(s"Result1: ${r1.mkString(" ")}")
  // println(s"Result1: ${r1.length}")
  println(s"Result1: ${r1.length}")
  // println(s"Result1b: ${r1b}")
  println(s"Result2: $r2") // low: 13412874

  // grid.draw()
  0
}

def day11part2(line: List[String]): Long = {
  0
}

