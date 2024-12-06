package aoc.day5

import os.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

@main def aoc2024day5(): Unit = {
  println("Day5")
  day5()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day5(): Unit = {
  val lines = os.read.lines.stream(os.resource / "aoc/day5/day5_input")
  println(day5part1(lines.toList))
  println(day5part2(lines.toList))
  // println(day5test(lines.toList))
}

def day5test(lines: List[String]): Int = {
  val config = lines.filter(_.contains("|"))
  val inputs = lines.filterNot(l => l.contains("|") || l == "").map(_.split(",").map(_.toInt).toList)

  // val c = config.sorted.map { pp =>
  //   val p = pp.split("\\|")
  //   (p(0).toInt,p(1).toInt)
  // }.groupMap(_._1)(_._2)

  val c = lines.takeWhile(_.nonEmpty).map(_.split("\\|")).collect { case Array(a, b) => (a.toInt, b.toInt) }
  given ruleSet: Set[(Int, Int)] = c.toSet

  val zipped = inputs(0).zip(inputs(0).tail)
  println(s"inputs(0): ${inputs(0)}")
  println(s"zipped: $zipped")
  val (xxx, yyy) = inputs.partition(input => input.view.zip(input.tail).forall(ruleSet))
  // println(s"xxx: $xxx")
  println(xxx.map(l => l(l.size / 2)).sum)
  0
}

def day5part1(lines: List[String]): Int = {
  val config = lines.filter(_.contains("|"))
  val inputs = lines.filterNot(l => l.contains("|") || l == "")

  // println(input)
  val c = config.sorted.map { pp =>
    val p = pp.split("\\|")
    (p(0).toInt,p(1).toInt)
  }.groupMap(_._1)(_._2)
  println(c.mkString(","))

  val valids = inputs.map(_.split(",").map(_.toInt).toList).filter { input =>
    val f = input.zipWithIndex.forall { (v, i) =>
      val r = c.get(v).fold(true)(_.forall(check => !input.slice(0, i).contains(check)))
      println(s"v: $v in ${c.get(v)} is $r")
      r
    }
    if (f) println(s"line: $input OK") else println(s"line: $input NOT")
    f
  }
  valids.map(l => l(l.size / 2)).sum
}

def day5part2(lines: List[String]): Int = {
  val config = lines.filter(_.contains("|"))
  val inputs = lines.filterNot(l => l.contains("|") || l == "")
  // println(input)
  val c = config.sorted.map { pp =>
    val p = pp.split("\\|")
    (p(0).toInt,p(1).toInt)
  }.groupMap(_._1)(_._2)
  println(c.mkString(","))

  val invalids = inputs.map(_.split(",").map(_.toInt).toList).filterNot { input =>
    val f = input.zipWithIndex.forall { (v, i) =>
      val r = c.get(v).fold(true)(_.forall(check => !input.slice(0, i).contains(check)))
      println(s"v: $v in ${c.get(v)} is $r")
      r
    }
    if (f) println(s"line: $input OK") else println(s"line: $input NOT")
    f
  }

  def checkOrder(v: Int, in: List[Int], i: Int): Boolean = {
    c.get(v).fold(true)(_.forall(check => !in.slice(0, i).contains(check)))
  }

  val reordered = invalids.map { in =>
    println(s"PART2 in: $in")
    val f = in.zipWithIndex.foldLeft(List[Int]()) { case (acc, (v, i)) =>
      val r = checkOrder(v, in, i)
      println(s"v: $v in: $in i: $i r: $r acc: $acc")
      if (r) (acc :+ v)
      else {
        val r2 = (acc.size to 0 by -1).filter { j =>
          println(s"v: $v j: $j slice: ${acc.slice(0, j)}")
          checkOrder(v, acc.slice(0, j) :+ v, j)
        }.head
        (acc.slice(0, r2) :+ v) ++ acc.slice(r2, acc.size)
      }
      // println(s"v: $v in ${c.get(v)} is $r")
    }
    f
  }

  reordered.map(l => l(l.size / 2)).sum
}

