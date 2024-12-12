package aoc.day9

import os.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq


@main def aoc2024day9(): Unit = {
  println("Day9")
  day9()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day9(): Unit = {
  println(s"Started...")
  val lines = os.read.lines.stream(os.resource / "aoc/day9/day9_input")

    val lines2 = """2333133121414131402"""
  val start = System.currentTimeMillis()
  // println(day9part1(lines.mkString))
  // println(day9part1(lines2))
  // println(day9part1lemon(lines.mkString))
  println(day9part2(lines.mkString))
  // println(day9part2(lines2))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

sealed trait Block { def size: Int }
case class File(idx: Int, size: Int) extends Block
case class Free(size: Int) extends Block

def parse(input: String) = input.zipWithIndex.map { case (c, i) =>
  if (i % 2 == 0) File(i / 2, c.asDigit) else Free(c.asDigit) }

def checksum(drive: Seq[Block]) = {
  val r1 = drive.flatMap {
    case File(idx, blk) => Vector.fill(blk)(idx.toLong)
    case Free(blk) => Vector.fill(blk)(0L)
  }
  val r2 = r1.zipWithIndex
  val r3 = r2.map { case (idx, blk) => blk * idx }
  val r = r3.sum
  r
}

def day9part1(line: String): Long = {
  def defragBlocks(blocks: Seq[Block]) = {
    @tailrec def defrag(drive: Seq[Block], files: Seq[File], defragmented: Seq[File]): Seq[File] = {
      (drive, files) match {
        case (File(idx, _) +: _, (f @ File(fileIdx, _)) +: _) if (idx >= fileIdx) => defragmented :+ f

        case ((f @ File(_, _)) +: drive, _) => defrag(drive, files, defragmented :+ f)

        case (Free(freeSize) +: drive, (f @ File(idx, size)) +: files) =>
          if (freeSize > size) defrag(Free(freeSize - size) +: drive, files, defragmented :+ f)
          else if (freeSize < size) defrag(drive, File(idx, size - freeSize) +: files, defragmented :+ File(idx, freeSize))
          else defrag(drive, files, defragmented :+ f)

      }
    }

    defrag(blocks, blocks.reverse.collect { case x @ File(_, _) => x }, Seq.empty[File] )
  }

  val blocks = parse(line)
  val defragged = defragBlocks(blocks)
  val result = checksum(defragged)
  println(result)
  0
}

def day9part1bad(lines: String): Long = {
  val fileBlocks = lines.toCharArray.foldLeft((true, 0, Vector.empty[(Int, Int)])) { case ((fs, idx, acc), arr) => 
    if (fs)
      (!fs, idx + 1, acc.appended((idx, arr.toInt - 48)))
    else
      (!fs, idx, acc.appended((-1, arr.toInt - 48)))
  }._3
  // println(fileBlocks)

  val reverseFilesOnly = fileBlocks.filter(_._1 >= 0).reverse
  
  val fileBlocksIterator = fileBlocks.iterator
  val reverseIterator = reverseFilesOnly.iterator
  val result = mutable.ListBuffer.empty[(Int, Int)]

  var remRevIdx = 0
  var remRevBlk = 0
  var remBlk = 0
  var stop = false
  while (fileBlocksIterator.hasNext) {
    val (idx, blk) = fileBlocksIterator.next()
    remBlk = blk

    if (idx >= 0) {
      println(s"idx >= 0 : ($idx, $blk)")
      result.addOne((idx, blk))
    }
    else {
      println(s"${remRevIdx == idx} && ${remRevBlk == 0}")
      if (remRevIdx == idx && remRevBlk == 0)
        result.addOne((-1, blk))
      else {
        while (remRevIdx != idx && remBlk > 0 && (reverseIterator.hasNext || remRevBlk > 0)) {
          val (rIdx, rBlk) = if (remRevBlk == 0) reverseIterator.next() else (remRevIdx, remRevBlk)
          remRevBlk = rBlk

          var blocks = 0
          if (remRevBlk > remBlk) {
            remRevBlk = remRevBlk - remBlk
            remRevIdx = rIdx
            blocks = remBlk
            remBlk = 0
          } else {
            remBlk = remBlk - remRevBlk
            // remRevIdx = 0
            blocks = remRevBlk
            remRevBlk = 0
          }
          println(s"rem ($rIdx, $blocks)")
          if (rIdx == idx) stop = true
          result.addOne(rIdx, blocks)
        }
      }
    }
  }

  println(result)

  val r1 = result.flatMap { case (idx, blk) => Vector.fill(blk)(idx) }
  println(r1)
  val r2 = r1.zipWithIndex
  println(r2)
  val r3 = r2.map { case (blk, idx) => blk * idx }
  println(r3)
  val r = r3.sum

  println(r)

  0
}

// https://github.com/twentylemon/advent-of-code/blob/main/src/main/scala/org/lemon/advent/year2024/Day09.scala
def day9part1lemon(lines: String): Long = {
  trait Segment
  case class Free(length: Int) extends Segment
  case class File(length: Int, id: Int) extends Segment

  def parse(input: String) = input
    .zipWithIndex
    .map((s, i) => if i % 2 == 0 then File(s.asDigit, i / 2) else Free(s.asDigit))

  def defragBlocks(segments: Seq[Segment]) = {
    @annotation.tailrec
    def defrag(segments: Seq[Segment], reverse: Seq[File], result: Seq[File] = Seq()): Seq[File] = {
      val toMove @ File(len, moveId) = reverse.head
      segments.head match {
        case File(_, id) if id >= moveId => result :+ toMove
        case Free(0) | File(0, _) => defrag(segments.tail, reverse, result)
        case Free(n) if len <= n => // move whole file; free space may remain
          defrag(Free(n - len) +: segments.tail, reverse.tail, result :+ toMove)
        case Free(n) if len > n => // move part of file; no free space left
          defrag(segments.tail, File(len - n, moveId) +: reverse.tail, result :+ File(n, moveId))
        case file: File => // a file; just copy it forward
          defrag(segments.tail, reverse, result :+ file)
      }
    }

    defrag(segments, segments.reverse.collect { case f: File => f })
  }

  def checksum(files: Seq[Segment]) =
    files.iterator
      .flatMap(_ match {
        case File(len, id) => Iterator.fill(len)(id.toLong)
        case Free(len) => Iterator.fill(len)(0L)
      })
      .zipWithIndex
      .map(_ * _)
      .sum


  val segments = parse(lines)
  val files = defragBlocks(segments)
  println(checksum(files))
  0
}

def day9part2(line: String): Long = {
  def defragBlocks(blocks: Seq[Block]) = {
    @tailrec def defrag(drive: Seq[Block], defragmented: Seq[Block]): Seq[Block] = {
      (drive) match {
        case Nil => defragmented

        case (f @ File(_, _)) +: driveTail => defrag(driveTail,  defragmented :+ f)

        case (free @ Free(freeSize)) +: driveTail =>
          driveTail.lastIndexWhere { case File(idx, size) if size <= freeSize => true; case _ => false } match {
            case -1 => defrag(driveTail, defragmented :+ free)
            case i => 
              val file = driveTail(i)
              if (freeSize > file.size) defrag(Free(freeSize - file.size) +: driveTail.updated(i, Free(file.size)), defragmented :+ file)
              else defrag(driveTail.updated(i, Free(file.size)), defragmented :+ file)
          }

      }
    }

    defrag(blocks, Seq.empty[File] )
  }

  val blocks = parse(line)
  val defragged = defragBlocks(blocks)
  println(defragged)
  val result = checksum(defragged)
  println(result)
  0
}

