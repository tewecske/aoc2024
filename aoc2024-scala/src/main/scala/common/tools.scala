package common

enum Direction(val x: Int, val y: Int) {
  case N extends Direction(-1, 0)
  case E extends Direction(0, 1)
  case S extends Direction(1, 0)
  case W extends Direction(0, -1)
}


case class Coord(x: Int, y: Int) {
  def move(dir: Direction): Coord = Coord(x + dir.x, y + dir.y)
  def up: Coord = copy(y = y - 1)
  def right: Coord = copy(x = x + 1)
  def down: Coord = copy(y = y + 1)
  def left: Coord = copy(x = x - 1)
  def allCross: List[Coord] = List(up, right, down, left)
}
case class Size(height: Int, width: Int)
case class Grid[A](grid: Array[Array[A]]) {
  val size = Size(grid.length, grid.head.length)
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

}
