package aoc.day4

import os.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

@main def aoc2024day4(): Unit = {
  println("Day4")
  day4()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day4(): Unit = {
  val lines = os.read.lines.stream(os.resource / "aoc/day4/day4_input")
  val wordblock = lines.toList
  val result1 = findXmas(wordblock)
  println(s"Result 1: $result1")
  val result2 = findX_mas(wordblock)
  println(s"Result 2: $result2")
}

def findXmas(wordblock: List[String]): Int = {
  val wordmap = wordblock.map(_.toList)
  val horizontal = wordblock.map(findXmasLine).sum
  val horizontalReverse = wordblock.map(line => findXmasLine(line.reverse)).sum


  val verticalBlock = ListBuffer[ListBuffer[Char]]()
  for (j <- 0 to wordblock.head.toList.size - 1) { println(s"pre $j"); verticalBlock.addOne(ListBuffer[Char]()) }
  for {
    j <- 0 to wordblock.head.toList.size - 1
    i <- 0 to wordblock.size - 1
  } {
    println(s"pro $i $j")
    verticalBlock(j).addOne(wordmap(i)(j))
  }
  
  val vertical = verticalBlock.map(vline => findXmasLine(vline.mkString)).sum
  val verticalReverse = verticalBlock.map(vline => findXmasLine(vline.mkString.reverse)).sum

  var diagonal1 = 0
  var diagonal2 = 0
  var diagonal3 = 0
  var diagonal4 = 0
  for {
    j <- 0 to wordblock.head.toList.size - 1
    i <- 0 to wordblock.size - 1
  } {
    println(s"pro $i $j")
    if (i <= wordmap.size - 4 && j <= wordmap(i).size - 4 &&
      wordmap(i)(j) == 'X' &&
    wordmap(i+1)(j+1) == 'M' &&
    wordmap(i+2)(j+2) == 'A' &&
    wordmap(i+3)(j+3) == 'S') diagonal1 = diagonal1 + 1
    if (i >= 3 && j >= 3 &&
      wordmap(i)(j) == 'X' &&
    wordmap(i-1)(j-1) == 'M' &&
    wordmap(i-2)(j-2) == 'A' &&
    wordmap(i-3)(j-3) == 'S') diagonal2 = diagonal2 + 1
    if (i >= 3 && j <= wordmap(i).size - 4 &&
      wordmap(i)(j) == 'X' &&
    wordmap(i-1)(j+1) == 'M' &&
    wordmap(i-2)(j+2) == 'A' &&
    wordmap(i-3)(j+3) == 'S') diagonal3 = diagonal3 + 1
    if (i <= wordmap.size - 4 && j >= 3 &&
      wordmap(i)(j) == 'X' &&
    wordmap(i+1)(j-1) == 'M' &&
    wordmap(i+2)(j-2) == 'A' &&
    wordmap(i+3)(j-3) == 'S') diagonal4 = diagonal4 + 1
  }
  
  horizontal + horizontalReverse + vertical + verticalReverse + diagonal1
 + diagonal2 + diagonal3 + diagonal4
}

def findX_mas(wordblock: List[String]): Int = {
  val wordmap = wordblock.map(_.toList)
  val horizontal = wordblock.map(findXmasLine).sum
  val horizontalReverse = wordblock.map(line => findXmasLine(line.reverse)).sum


  var diagonal1 = 0
  var diagonal2 = 0
  var diagonal3 = 0
  var diagonal4 = 0
  for {
    j <- 0 to wordblock.head.toList.size - 1
    i <- 0 to wordblock.size - 1
  } {
    if (i <= wordmap.size - 2 && j <= wordmap(i).size - 2 &&
    i >= 1 && j >= 1 &&
      wordmap(i)(j) == 'A' &&
    (
      (wordmap(i+1)(j+1) == 'M' &&
    wordmap(i-1)(j-1) == 'S' &&
    wordmap(i-1)(j+1) == 'M' &&
    wordmap(i+1)(j-1) == 'S') ||
      (wordmap(i+1)(j+1) == 'S' &&
    wordmap(i-1)(j-1) == 'M' &&
    wordmap(i-1)(j+1) == 'S' &&
    wordmap(i+1)(j-1) == 'M') ||
    //   (wordmap(i+1)(j+1) == 'S' &&
    // wordmap(i-1)(j-1) == 'S' &&
    // wordmap(i-1)(j+1) == 'M' &&
    // wordmap(i+1)(j-1) == 'M') ||
    //   (wordmap(i+1)(j+1) == 'M' &&
    // wordmap(i-1)(j-1) == 'M' &&
    // wordmap(i-1)(j+1) == 'S' &&
    // wordmap(i+1)(j-1) == 'S') ||
      (wordmap(i+1)(j+1) == 'M' &&
    wordmap(i-1)(j-1) == 'S' &&
    wordmap(i-1)(j+1) == 'S' &&
    wordmap(i+1)(j-1) == 'M') ||
      (wordmap(i+1)(j+1) == 'S' &&
    wordmap(i-1)(j-1) == 'M' &&
    wordmap(i-1)(j+1) == 'M' &&
    wordmap(i+1)(j-1) == 'S')
  )) diagonal1 = diagonal1 + 1
  }
  
  diagonal1 + diagonal2 + diagonal3 + diagonal4
}


val xmasRegex = "XMAS".r
def findXmasLine(line: String): Int = {
  val found = xmasRegex.findAllIn(line)
  found.toList.size
}


