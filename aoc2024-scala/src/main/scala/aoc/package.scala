package aoc
import scala.language.implicitConversions
import scala.collection.immutable.TreeMap

extension (self: Long)
  inline def >=<(n: Long): Boolean       = self >= 0 && self < n
  inline def >=<(n: (Int, Int)): Boolean = self >= n._1 && self < n._2
  inline def %?(n: Long): Boolean        = self % n == 0
  inline def %%(n: Long): Long           =
    val mod = self % n
    if mod < 0 then mod + n else mod
  inline def |-|(n: Long): Long          = (self - n).abs
  inline def /%(n: Long): (Long, Long)   = (self / n, self % n)
  inline def **(n: Long): Long           = math.pow(self.doubleValue, n.doubleValue).longValue
  inline def digits: Int                 = 1 + math.log10(self.doubleValue).intValue

type Board = Vector[String]

extension (self: Board)
  def width                            = self.head.length
  def height                           = self.length
  def nw: Loc                          = Origin
  def se: Loc                          = Loc(width - 1, height - 1)
  def apply(loc: Loc): Char            = self(loc.y.toInt)(loc.x.toInt)
  def get(loc: Loc): Option[Char]      = Option.when(loc >=< self)(self(loc))
  def is(loc: Loc, c: Int): Boolean    = loc >=< self && self(loc) == c
  def is(a: Loc, b: Loc): Boolean      = get(a) == get(b)
  def find(char: Char): Loc            = locations.find(apply(_) == char).get
  def findAll(char: Char): Vector[Loc] = locations.filter(apply(_) == char)

  def locations: Vector[Loc] =
    self.indices.toVector.flatMap(y => self.head.indices.map(x => Loc(x, y)))

  def update(loc: Loc, c: Char): Vector[String] =
    self.updated(loc.y.toInt, self(loc.y.toInt).updated(loc.x.toInt, c))

enum Dir(val dx: Long, val dy: Long):
  def cw: Dir             = Dir.fromOrdinal((ordinal + 2) % Dir.values.length)
  def ccw: Dir            = Dir.fromOrdinal((ordinal + Dir.values.length - 2) % Dir.values.length)
  def ccw2: Dir           = Dir.fromOrdinal((ordinal + Dir.values.length - 1) % Dir.values.length)
  def reverse: Dir        = Dir.fromOrdinal((ordinal + 4) % Dir.values.length)
  def horizontal: Boolean = dy == 0
  def vertical: Boolean   = dx == 0

  inline def *(length: Long): Vec = Vec(this, length)

  case N  extends Dir(0, -1)
  case NE extends Dir(1, -1)
  case E  extends Dir(1, 0)
  case SE extends Dir(1, 1)
  case S  extends Dir(0, 1)
  case SW extends Dir(-1, 1)
  case W  extends Dir(-1, 0)
  case NW extends Dir(-1, -1)

object Dir:
  given Ordering[Dir] = Ordering.by(_.ordinal)

  implicit def toVec(dir: Dir): Vec = dir * 1

val CardinalDirections: Vector[Dir] = Vector(Dir.N, Dir.E, Dir.S, Dir.W)

val OrdinalDirections: Vector[Dir] = Vector(Dir.NE, Dir.SE, Dir.SW, Dir.NW)

// a location in space

final case class Loc(x: Long, y: Long):
  inline def +(addend: Vec): Loc = Loc(x + addend.dx, y + addend.dy)

  inline def -(subtrahend: Vec): Loc = Loc(x - subtrahend.dx, y - subtrahend.dy)

  inline def -(subtrahend: Loc): Loc = Loc(x - subtrahend.x, y - subtrahend.y)

  inline def +(addend: Loc): Loc = Loc(x + addend.x, y + addend.y)

  inline def >=<(board: Board): Boolean = this >=< (board.head.length, board.length)

  inline def <>=(board: Board): Boolean = !(this >=< board)

  inline def >=<(size: Long): Boolean = this >=< (size, size)

  inline def >=<(w: Long, h: Long): Boolean = x >=< w && y >=< h

  def adjacents: Vector[Loc] = CardinalDirections.map(this + _)

  def manhattan(l: Loc): Long = (l.x - x).abs + (l.y - y).abs

  override def toString: String = s"$x,$y"

given Ordering[Loc] = Ordering.by(Tuple.fromProductTyped)

val Origin: Loc = Loc(0, 0)


final case class Vec(direction: Dir, magnitude: Long):
  def dx: Long = direction.dx * magnitude
  def dy: Long = direction.dy * magnitude

  inline def +(addend: Long): Vec     = copy(magnitude = magnitude + addend)
  inline def -(subtrahend: Long): Vec = copy(magnitude = magnitude - subtrahend)
  inline def *(multiplier: Long): Vec = copy(magnitude = magnitude * multiplier)


type PriorityQueue[K, V] = TreeMap[K, Vector[V]]

trait Priority[K, V]:
  def priority(value: V): K

object PriorityQueue:
  def apply[K: Ordering, V](value: V)(using P: Priority[K, V]): PriorityQueue[K, V] =
    TreeMap(P.priority(value) -> Vector(value))

extension [K, V](queue: PriorityQueue[K, V])
  def enqueue(value: V)(using P: Priority[K, V]): PriorityQueue[K, V] =
    queue.updatedWith(P.priority(value)):
      case Some(values) => Some(values :+ value)
      case None         => Some(Vector(value))

  def enqueueAll(values: Iterable[V])(using P: Priority[K, V]): PriorityQueue[K, V] =
    values.foldLeft(queue)(_.enqueue(_))

  def dequeue: (V, PriorityQueue[K, V]) =
    val (priority, values) = queue.head
    if values.size == 1 then (values.head, queue - priority)
    else (values.head, queue + (priority -> values.tail))

  def firstValue: V = firstValues.head

  def firstValues: Vector[V] = queue.valuesIterator.next()
