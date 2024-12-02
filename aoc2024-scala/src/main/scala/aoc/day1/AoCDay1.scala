package aoc.day1

import os.*

@main def aoc2024(): Unit = {
  println("Hello Day1!")
  day1()
}

def day1(): Unit = {
  // first 50 places
  // location ID

  val locations = os.read.lines.stream(os.resource / "aoc/day1/day1_input")

  val lists = locations.map(_.split(" +").toList.map(_.toInt)).fold((List.empty[Int], List.empty[Int])) { (acc, list) =>
    (acc._1 :+ list(0), acc._2 :+ list(1))
  }

  val sortedList1 = lists._1.sorted
  val sortedList2 = lists._2.sorted
  val sortedLists = sortedList1.zip(sortedList2)

  val result = sortedLists.map { (first, second) =>
    val diff = Math.abs(first - second)
    println(s"$first $second $diff")
    diff
  }.sum
  
  val result2 = sortedList1.map(i => i * sortedList2.filter(_ == i).length).sum


  println(s"Sum: $result")
  println(s"Sim: $result2")

}
