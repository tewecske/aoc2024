package aoc.day12

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq


@main def aoc2024day12(): Unit = {
  println("Day12")
  day12()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day12(): Unit = {
  println(s"Started...")
  val lines = os.read.lines.stream(os.resource / "aoc/day12/day12_input")

    val lines2 = """
RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE""".split("[\n\r]+").toList.tail
  val start = System.currentTimeMillis()
  println(day12part1(lines.toList))
  // println(day12part1(lines2))
  // println(day12part2(lines.mkString))
  // println(day12part2(lines2))
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

def day12part1(line: List[String]): Long = {
  val grid = Grid(line.map(_.toCharArray()).toArray)

  def memoize[A, B](f: A => B): A => B = {
    val cache = mutable.Map.empty[A, B]
    (a: A) => cache.getOrElseUpdate(a, f(a))
  }


  val plantTypes = line.mkString.toSet
  // println(plantTypes.mkString(","))
  val plants = line.mkString.zipWithIndex
  // println(plants)
  val plantCoords = grid.find(_ == 'R')
  // println(plantCoords.mkString(","))

  extension [A](a: A) {
    inline def |>[B](f: A => B): B = f(a)
  }

  case class Plant(kind: Char, parent: Int, coord: Coord)

  val parents = mutable.Map.empty[Coord, Coord]
  val ranks = mutable.Map.empty[Coord, Int]


  def findRoot(coord: Coord): Coord = {
    parents.get(coord) match {
      case None => coord
      case Some(parent) => 
        val root = findRoot(parent)
        parents.update(coord, root)
        root
    }
  }

  // Tarjan's disjoint-set representation
  for {
    x <- 0 until grid.height
    y <- 0 until grid.width
    coord = Coord(x, y)
    } {
      coord.allCross.foreach { neighbor =>
        (grid.at(x, y), grid.at(neighbor)) match {
          case (currentPlant, Some(nextPlant)) if currentPlant == nextPlant =>
            // currentParent to -1 ???
            (findRoot(coord), findRoot(neighbor)) match {
              case (currentRoot, nextRoot) => 
                if (currentRoot != nextRoot) {
                  (ranks.getOrElse(currentRoot, 0), ranks.getOrElse(nextRoot, 0)) match {
                    case (currentRank, nextRank) =>
                      if (currentRank < nextRank) {
                        parents.update(currentRoot, nextRoot)
                      } else if (currentRank > nextRank) {
                        parents.update(nextRoot, currentRoot)
                      } else {
                        parents.update(nextRoot, currentRoot)
                        ranks.update(currentRoot, currentRank + 1)
                      }
                  }
                }
            }
          case (_, _) => ()
        }
      }
    }

  val components = mutable.Map.empty[Coord, List[Coord]]

  for {
    x <- 0 until grid.height
    y <- 0 until grid.width
    coord = Coord(x, y)
    } {
      val root = findRoot(coord)
      components.update(root, coord :: components.getOrElse(root, Nil))
    }

  // println(parents.mkString(","))
  // println(components.mkString(","))

  val data = components.map { case (root, group) =>
    val rootPlant = grid.at(root).get
    (rootPlant, group.map( c => c.allCross.filter(nc => grid.at(nc) match {
      case Some(adjPlant) => adjPlant != rootPlant
      case None => true
    }).size).sum, group.size)
  }
  // grid.draw() 

  val data2 = components.map { case (root, g) =>
    val group = g.toSet
    val rootPlant = grid.at(root).get

    val perimeterWithOutside = group.flatMap(node => 
        node.allCross.filter(!group.contains(_)).map(out => (node, out))
    )

    val sidesOnly = perimeterWithOutside.filterNot { case (p1, p2) =>
      perimeterWithOutside(p1.down, p2.down) || perimeterWithOutside(p1.right, p2.right)
    }.size
    

    val perimeter = group.filter(_.surrounding.exists(!group.contains(_)))

    val sides = perimeter.iterator.map { coord =>
      // coord is adjacent to the outside, each contributes 1 side
      // oo
      // o#
      val topLeft = !group(coord.up) && !group(coord.left)
      // oo
      // #o
      val topRight = !group(coord.up) && !group(coord.right)
      // o#
      // oo
      val botLeft = !group(coord.down) && !group(coord.left)
      // #o
      // oo
      val botRight = !group(coord.down) && !group(coord.right)

      // coord is diagonal to the outside, the shape has turned a corner
      // o#
      // ##
      val diagUpLeft = group(coord.up) && group(coord.left) && !group(coord.up.left)
      // #o
      // ##
      val diagUpRight = group(coord.up) && group(coord.right) && !group(coord.up.right)
      // ##
      // #o
      val diagBotLeft = group(coord.down) && group(coord.left) && !group(coord.down.left)
      // ##
      // o#
      val diagBotRight = group(coord.down) && group(coord.right) && !group(coord.down.right)

      // println(s"$rootPlant $coord $topLeft, $topRight, $botLeft, $botRight, $diagUpLeft, $diagUpRight, $diagBotLeft, $diagBotRight")
      Seq(topLeft, topRight, botLeft, botRight, diagUpLeft, diagUpRight, diagBotLeft, diagBotRight)
          .count(identity)
    }.sum

    // println(s"$rootPlant group: ${group.size} sides: ${sides}")
    // println(s"$rootPlant group: ${group.size} sides: ${sides} sidesOnly: $sidesOnly")
    (rootPlant, sidesOnly, group.size)
  }

  val r1 = data.map { case (plant, perimeter, area) =>
    val plantCost = area * perimeter
    // println(s"$plant $area * $perimeter = ${plantCost}")
    plantCost
  }.sum
  val r2 = data2.map { case (plant, sides, area) =>
    val plantCost = area * sides
    // println(s"$plant $area * $sides = ${plantCost}")
    plantCost
  }.sum
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

def day12part2(line: List[String]): Long = {
  0
}

