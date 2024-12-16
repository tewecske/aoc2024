package aoc.day15

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq
import scala.util.Using


@main def aoc2024day15(): Unit = {
  println("Day15")
  day15()
}
val isDebug = false
def debug(str: String): Unit = {
  if (isDebug) {
    println(str)
  }
}

def day15(): Unit = {
  println(s"Started...")
  val lines = os.read(os.resource / "aoc/day15/day15_input")
  // Using.resource(scala.io.Source.fromResource(name))(source => source.mkString)

    val lines2 = """########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<"""

    val lines3 = """#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^"""

    val linesTest = """##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"""

  val start = System.currentTimeMillis()

  def parse(input: String): (Grid[Char], Seq[Direction])= {
    val inputs = input.split("\n\n")
    (Grid(inputs(0).split("\n").map(_.toCharArray)), inputs(1).filterNot(_ == '\n').toSeq.map(Direction(_)))
  }


  // println(day15part1.tupled(parse(lines2)))
  // println(day15part1.tupled(parse(lines))) // 1471826
  // println(day15part2.tupled(parse(lines3)))
  // println(day15part2.tupled(parse(linesTest)))
  println(day15part2.tupled(parse(lines))) // 
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}
// part 2 hint: https://github.com/scarf005/aoc-scala/blob/main/2024/day15.scala
def day15part1(grid: Grid[Char], dirs: Seq[Direction]): Long = {

  grid.draw()

  val robot = grid.find(_ == '@')(0)

  def checkDir(grid: Grid[Char], r: Coord, d: Direction) = {
    val v = Vector.unfold(r.move(d)) { c =>
      // debug(s"Checking $c ${grid.at(c)}")
      c match {
        case c if !grid.inside(c) => None
        case c if grid.at(c) == Some('O') => Some(('O', c.move(d)))
        case c if grid.at(c) == Some('#') => None
        case c if grid.at(c) == Some('.') => Some(('.', c.move(d)))
      }
    }
    val i = v.indexOf('.')
    // debug(s"v: $v i: $i")
    if (i == -1) Vector.empty
    else v.take(i + 1)
  }

  def moveRobot(grid: Grid[Char], robot: Coord, d: Direction, boxes: Vector[Char]) = {
    boxes.filter(_ == '.').foldLeft((grid, robot)) { case ((grid, r), box) =>
      grid.update(r, '.')
      val newRobot = r.move(d)
      grid.update(newRobot, '@')
      (grid, newRobot)
    }
  }

  def pushBoxes(grid: Grid[Char], robot: Coord, d: Direction, boxes: Vector[Char]) = {
    boxes.filter(_ == 'O').foldLeft((grid, robot)) { case ((grid, boxPos), box) =>
      val newPos = boxPos.move(d)
      grid.update(newPos, 'O')
      (grid, newPos)
    }
  }

  grid.draw()
  val (endGrid, _) = dirs.foldLeft((grid, robot)) { case ((g, robot), d) =>
    val checks = checkDir(g, robot, d)
    // debug(checks)
    val res = if (!checks.contains('.')) (g, robot)
    else {
      val (g1, r1) = moveRobot(g, robot, d, checks)
      val (g2, _) = pushBoxes(g1, r1, d, checks)
      (g2, r1)
    }
    // Thread.sleep(500)
    // print(s"\u001b[${g.height + 1}A")
    // debug(s"robot: $robot -> ${res._2} dir: $d")
    // g.draw()
    res
  }

  val r1 = endGrid.find(_ == 'O').map(c => 100 * c.x + c.y).sum
  // grid.draw()

  // val r1 = 0
  val r2 = 0
  debug(s"Result1: ${r1}")
  debug(s"Result2: $r2")

  0
}

def day15part2(grid: Grid[Char], dirs: Seq[Direction]): Long = {

  grid.draw()
  val enlargedGrid = grid.enlargeY(Map('@' -> Seq('@', '.'), 'O' -> Seq('[', ']')))
  enlargedGrid.draw()

  val robot = enlargedGrid.find(_ == '@')(0)

  // TODO: remove 2nd param
  def checkCollision(c1: Coord, c2: Coord, updates: Set[(Coord, Char)])(using grid: Grid[Char], r: Coord, d: Direction): Set[(Coord, Char)] = {
    import Direction.*
    debug(s"c1: $c1 ${grid.at(c1)}, c2: $c2 ${grid.at(c2)}, d: $d")
    val basic = Set((c2, '@'))
    (grid.at(c1), grid.at(c2)) match {
      case (Some('@'), Some('.')) => debug(s"@ . $d $c2 @"); basic ++ updates
      case (Some('@'), Some('[')) => d match {
        case N |  S => debug(s"@ [ $d $c2 @"); checkCollision(c2, c2.move(d), basic ++ updates)
        case E => debug(s"@ [ $d $c2 @"); checkCollision(c2, c2.move(d), basic ++ updates)
        case W => debug(s"@ [ $d ERROR"); updates
      }
      case (Some('@'), Some(']')) => d match {
        // Same as before ('[') if I extract the char and check with if
        case N | S => debug(s"@ ] $d $c2 @"); checkCollision(c2, c2.move(d), basic ++ updates)
        case E => debug(s"@ ] $d updates"); updates
        case W => debug(s"@ ] $d $c2 @"); checkCollision(c2, c2.move(d), basic ++ updates)
      }
      case (Some('['), Some(nxt)) if Set('[', ']').contains(nxt) => d match {
        case N | S =>  debug(s"[ $nxt $d $c2 ["); if (updates.contains((c2, '['))) updates else {
          checkCollision(c1.move(E), c1.move(E).move(d), checkCollision(c2, c2.move(d), updates ++ Set((c2, '['))))
        }
        case E | W => debug(s"[ ] $d $c2 ["); checkCollision(c2, c2.move(d), Set((c2, '[')) ++ updates)
      }
      case (Some(']'), Some(nxt)) if Set('[', ']').contains(nxt) => d match {
        case N | S => debug(s"] $nxt $d $c2 ]"); if (updates.contains((c2, ']'))) updates else {
          checkCollision(c1.move(W), c1.move(W).move(d), checkCollision(c2, c2.move(d), Set((c2, ']')) ++ updates))
        }
        case E | W => debug(s"] [ $d $c2 ]"); checkCollision(c2, c2.move(d), Set((c2, ']')) ++ updates)
      }
      case (Some('['), Some('.')) => d match {
        case N | S => debug(s"[ . $d $c2 ["); if (updates.contains((c2, '['))) updates
          else checkCollision(c1.move(E), c1.move(E).move(d), Set((c2, '[')) ++ updates)
        case W => debug(s"[ . $d ["); updates ++ Set((c2, '['))
        case E => debug(s"[ . $d updates"); updates
      }
      case (Some(']'), Some('.')) => d match {
        case N | S => debug(s"] . $d $c2 ]"); if (updates.contains((c2, ']'))) updates
          else checkCollision(c1.move(W), c1.move(W).move(d), Set((c2, ']')) ++ updates)
        case E => debug(s"] . $d ]"); updates ++ Set((c2, ']'))
        case W => debug(s"] . $d updates"); updates
      }
      case (Some('.'), Some('.')) => debug(s". . $d updates"); updates
      case (Some(_), Some('#')) => debug(s"_ # $d $c2 #"); Set((c2, '#')) ++ updates // should block all
      case (_, _) => debug("ERROR"); updates

    }
  }

  val endGrid = dirs.foldLeft((enlargedGrid, robot)) { case ((g, r), d) =>
    val updates = checkCollision(r, r.move(d), Set.empty)(using g, r, d)
    if (updates.filter(_._2 == '#').nonEmpty) (g, r)
    else {
      updates.foreach( (uc, _) => g.update(uc.move(d.opposite), '.') )
      updates.foreach( (uc, _) => g.update(uc, '.') )
      updates.foreach( (uc, uv) => g.update(uc, uv) )
      // g.draw()
      (g, g.find(_ == '@').head)
    }
  }

  endGrid._1.draw()
  val height = enlargedGrid.height - 1
  val width = enlargedGrid.width - 1
  val r1 = endGrid._1.find(_ == '[').map(c => 100 * c.x + c.y).sum
  // val r1 = endGrid._1.find(_ == '[').toSeq.map { c => 
  //   println(s"${height} ${width} c: $c ${Math.min(height - c.x, c.x)} ${Math.min(width - c.y - 1, c.y)}")
  //   100 * Math.min(height - c.x, c.x) + Math.min(width - c.y - 1, c.y)
  // }.sum
  // val r1 = 0
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")

  0
}

