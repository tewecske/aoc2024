package common

import scala.reflect.ClassTag


extension (a: Long) {
  inline def digits = math.log10(a.toDouble).toInt + 1
  inline def ||(b: Long) = (a * math.pow(10, b.digits)).toLong + b
  inline def split = {
    val s = math.pow(10, a.digits / 2).toLong
    ((a / s).toLong, (a % s))
  }
}

enum Direction(val x: Int, val y: Int) {
  case N extends Direction(-1, 0)
  case E extends Direction(0, 1)
  case S extends Direction(1, 0)
  case W extends Direction(0, -1)

  def all = Seq(N, E, S, W)
  def cw = this match {
    case N => E
    case E => S
    case S => W
    case W => N
  }
  def ccw = this match {
    case N => W
    case E => N
    case S => E
    case W => S
  }
  def opposite: Direction = this match {
    case N => S
    case E => W
    case S => N
    case W => E
  }
}
object Direction {
  def apply(c: Char) = c match {
    case '^' => N
    case '>' => E
    case 'v' => S
    case '<' => W
  }
}

case class Coord(x: Int, y: Int) {
  def move(dir: Direction): Coord = Coord(x + dir.x, y + dir.y)
  def up: Coord = copy(y = y - 1)
  def right: Coord = copy(x = x + 1)
  def down: Coord = copy(y = y + 1)
  def left: Coord = copy(x = x - 1)
  def allCross: List[Coord] = List(up, right, down, left)
  def surrounding: List[Coord] = List(up, right, down, left, up.right, right.down, down.left, left.up)
}
case class Size(height: Int, width: Int)
case class Grid[A: ClassTag](grid: Array[Array[A]]) {
  val height = grid.length
  val width = grid.head.length
  val size = Size(height, width)
  val lookup = (for { 
    x <- 0 until size.height
    y <- 0 until size.width
    c = at(x, y)
    if c != '.'
  } yield (c, (x, y))).groupMap { case (c, (x, y)) => c } { case (c, xy) => xy }

  def find(f: A => Boolean): Array[Coord] =
    for {
      (row, x) <- grid.zipWithIndex
      (cell, y) <- row.zipWithIndex
      if f(cell)
    } yield Coord(x, y)

  def inside(x: Int, y: Int): Boolean = !(x < 0 || y < 0 || x >= size.height || y >= size.width)
  def inside(c: Coord): Boolean = inside(c.x, c.y)

  def at(x: Int, y: Int): A = grid(x)(y)
  def at(c: Coord): Option[A] = if (inside(c)) Some(grid(c.x)(c.y)) else None

  def draw(): Unit = {
    for { 
      x <- 0 until size.height
      y <- 0 until size.width
    } {
      print(at(x, y))
      if (y + 1 == size.width) println
    }
  }

  def update(c: Coord, a: A) = {
    grid(c.x).update(c.y, a)
  }

  def drawWith(xys: List[(Int, Int)]): Unit = {
    for { 
      x <- 0 until size.height
      y <- 0 until size.width
    } {
      if (xys.contains((x, y))) print('#')
      else print(at(x, y))
      if (y + 1 == size.width) println
    }
  }

  def enlargeY(replace: Map[A, Seq[A]]): Grid[A] = {
      Grid(grid.map { r =>
        (for { 
          y <- 0 until size.width
        } yield {
          replace.getOrElse(r(y), Seq(r(y), r(y)))
        }).flatten.toArray
      })
  }

}
