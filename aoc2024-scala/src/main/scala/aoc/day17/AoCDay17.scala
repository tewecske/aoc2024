package aoc.day17

import os.*
import common.*
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.io.StdIn
import scala.collection.parallel.CollectionConverters.*
import scala.collection.mutable.ArraySeq
import scala.util.Using


@main def aoc2024day17(): Unit = {
  println("Day17")
  day17()
}
val isDebug = false
def debug[A](a: A): Unit = {
  if (isDebug) {
    println(a)
  }
}

// Combo operands 0 through 3 represent literal values 0 through 3.
// Combo operand 4 represents the value of register A.
// Combo operand 5 represents the value of register B.
// Combo operand 6 represents the value of register C.
// Combo operand 7 is reserved and will not appear in valid programs.
sealed trait Combo
case class Literal(value: Long) extends Combo
case object RegA extends Combo
case object RegB extends Combo
case object RegC extends Combo
case object Invalid extends Combo

object Combo:
  def apply(operand: Long) =
    operand match
      case 0 => Literal(0)
      case 1 => Literal(1)
      case 2 => Literal(2)
      case 3 => Literal(3)
      case 4 => RegA
      case 5 => RegB
      case 6 => RegC
      case _ => Invalid

sealed trait Opcode(opcode: Int)
case class Adv(combo: Combo) extends Opcode(0)
case class Bxl(literal: Literal) extends Opcode(1)
case class Bst(combo: Combo) extends Opcode(2)
case class Jnz(literal: Literal) extends Opcode(3)
case object Bxc extends Opcode(4)
case class Out(combo: Combo) extends Opcode(5)
case class Bdv(combo: Combo) extends Opcode(6)
case class Cdv(combo: Combo) extends Opcode(7)

def show(opcode: Opcode) = opcode match
  case Adv(combo) => s"Adv($combo) a = a / 2 ^ $combo"
  case Bxl(literal) => s"Bxl($literal) b = b xor $literal"
  case Bst(combo) => s"Bst($combo) b = $combo % 8"
  case Jnz(literal) => s"Jnz($literal) pointer = $literal / 2"
  case Bxc => s"Bxc b = b ^ c"
  case Out(combo) => s"Out($combo) output = $combo % 8"
  case Bdv(combo) => s"Bdv($combo) b = a / 2 ^ $combo"
  case Cdv(combo) => s"Cdv($combo) c = a / 2 ^ $combo"

def parseOpcode(opcode: Long, operand: Long) =
  opcode match
    case 0 => Adv(Combo(operand))
    case 1 => Bxl(Literal(operand))
    case 2 => Bst(Combo(operand))
    case 3 => Jnz(Literal(operand))
    case 4 => Bxc
    case 5 => Out(Combo(operand))
    case 6 => Bdv(Combo(operand))
    case 7 => Cdv(Combo(operand))


def parseInstructions(ins: List[Long]) =
  ins.sliding(2, 2).map { l =>
    val opcode = l(0)
    val operand = l(1)
    parseOpcode(opcode, operand)
  }.toList


case class Registers(a: Long, b: Long, c: Long)
case class ProgramState(registers: Registers, instructions: List[Opcode], raw: List[Long], pointer: Int = 0)


def day17(): Unit = {
  println(s"Started...")
  val lines = os.read(os.resource / "aoc/day17/day17_input")
  // Using.resource(scala.io.Source.fromResource(name))(source => source.mkString)

    val lines2 = """Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""

    val test1 = """Register A: 0
Register B: 0
Register C: 9

Program: 2,6"""

    val test2 = """Register A: 10
Register B: 0
Register C: 0

Program: 5,0,5,1,5,4"""

    val test3 = """Register A: 2024
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0"""

    val part2Test = """Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0"""

    val part2Check1 = s"""Register A: ${108107574778365L}
Register B: 0
Register C: 0

Program: 2,4,1,3,7,5,4,0,1,3,0,3,5,5,3,0"""

  val start = System.currentTimeMillis()

  def parse(input: String) = {
    input.split("\n").toSeq match {
      case Seq(
        s"Register A: $a",
        s"Register B: $b",
        s"Register C: $c",
        s"",
        s"Program: $instructions") => 
        val ins = instructions.split(",").toList.map(_.toLong)
        val parsedInstructions = parseInstructions(ins)
        println(instructions.split(",").toList.sliding(2, 2).zip(parsedInstructions.map(show)).mkString("\n"))
        ProgramState(Registers(a.toLong, b.toLong, c.toLong), parsedInstructions, ins)
    }
  }


  // println(day17part1(parse(lines2)))
  // println(day17part1(parse(lines))) // 
  // println(day17part1(parse(test1)))
  // println(day17part1(parse(test2)))
  // println(day17part1(parse(test3)))
  // println(day17part2(parse(part2Test)))
  println(day17part2(parse(lines))) // 
  println(day17part1(parse(part2Check1))) // 
  val endTime = System.currentTimeMillis()
  println(s"Solved in: ${endTime - start} ms")
}

@tailrec def loop(programState: ProgramState, output: List[Long] = List.empty[Long]): List[Long] = 
  debug("--------------")
  def evalCombo(combo: Combo) =
    combo match
      case Literal(value) => value
      case RegA => programState.registers.a
      case RegB => programState.registers.b
      case RegC => programState.registers.c
      case Invalid => 0

  def printOp(opcode: Opcode) = opcode match
      case Adv(combo) => debug(s"Adv($combo) a = a / 2 ^ ${evalCombo(combo)}")
      case Bxl(literal) => debug(s"Bxl($literal) b = b xor ${literal.value}")
      case Bst(combo) => debug(s"Bst($combo) b = ${evalCombo(combo)} % 8")
      case Jnz(literal) if programState.registers.a == 0 => debug(s"Jnz($literal) noop")
      case Jnz(literal) => debug(s"Jnz($literal) pointer = ${literal.value / 2}")
      case Bxc => debug(s"Bxc b = b ^ c")
      case Out(combo) => debug(s"Out($combo) output = ${evalCombo(combo)} % 8")
      case Bdv(combo) => debug(s"Bdv($combo) b = a / 2 ^ ${evalCombo(combo)}")
      case Cdv(combo) => debug(s"Cdv($combo) c = a / 2 ^ ${evalCombo(combo)}")

  if (programState.pointer >= programState.instructions.size)
    output
  else {
    val regs = programState.registers
    val ptr = programState.pointer
    val current = programState.instructions(programState.pointer)
    printOp(current)
    val (nextState, nextOutput) = current match
      case Adv(combo) => (programState.copy(pointer = ptr + 1, registers = regs.copy(a = regs.a / Math.pow(2.toDouble, evalCombo(combo).toDouble).toLong)), output)
      case Bxl(literal) => (programState.copy(pointer = ptr + 1, registers = regs.copy(b = regs.b ^ literal.value)), output)
      case Bst(combo) => (programState.copy(pointer = ptr + 1, registers = regs.copy(b = evalCombo(combo) % 8)), output)
      case Jnz(literal) if regs.a == 0 => (programState.copy(pointer = ptr + 1), output)
      case Jnz(literal) => debug("==================="); (programState.copy(pointer = 0/*literal.value / 2*/), output)
      case Bxc => (programState.copy(pointer = ptr + 1, registers = regs.copy(b = regs.b ^ regs.c)), output)
      case Out(combo) => (programState.copy(pointer = ptr + 1), output :+ (evalCombo(combo) % 8))
      case Bdv(combo) => (programState.copy(pointer = ptr + 1, registers = regs.copy(b = regs.a / Math.pow(2.toDouble, evalCombo(combo).toDouble).toLong)), output)
      case Cdv(combo) => /*println(s"Cdv(combo) $combo ${evalCombo(combo)}");*/ (programState.copy(pointer = ptr + 1, registers = regs.copy(c = regs.a / Math.pow(2.0d, evalCombo(combo).toDouble).toLong)), output)
  
    debug(nextState.registers)
    debug(nextOutput)
    loop(nextState, nextOutput)
  }
end loop

// part 2 hint: https://github.com/scarf005/aoc-scala/blob/main/2024/day17.scala
def day17part1(programState: ProgramState): Long = {

  println(programState)

  val output = loop(programState)

  val r1 = output.mkString(",")
  val r2 = 0
  println(s"Result1: ${r1}")
  println(s"Result2: $r2")
  println(s"${3 % 2}")

  0
}

def day17part2(programState: ProgramState): Long = {

  println(programState)

  // println(s"Registers(51342988,0,0)")
  // println(s"b=${51342988 % 8} (List(2, 4),Bst(RegA) b = a % 8)")
  // println(s"Registers(51342988,4,0)")
  // println(s"b=${4 ^ 3} (List(1, 3),Bxl(Literal(3)) b = b xor Literal(3))")
  // println(s"Registers(51342988,7,0)")
  // println(s"c=${(51342988 / Math.pow(2.0, 7)).toLong} (List(7, 5),Cdv(RegB) c = a / 2 ^ b)")
  // println(s"Registers(51342988,7,401117)")
  // println(s"b=${7 ^ 401117} (List(4, 0),Bxc b = b xor c)")
  // println(s"Registers(51342988,401114,401117)")
  // println(s"b=${401114 ^ 3} (List(1, 3),Bxl(Literal(3)) b = b xor Literal(3))")
  // println(s"Registers(51342988,401113,401117)")
  // println(s"a=${51342988 / 8} (List(0, 3),Adv(Literal(3)) a = a / 2 ^ Literal(3))")
  // println(s"Registers(6417873,401113,401117)")
  // println(s" (List(5, 5),Out(RegB) output = RegB % 8)")
  // println(s"out=${401113 % 8} Registers(6417873,401113,401117)")
  // println(s" (List(3, 0),Jnz(Literal(0)) pointer = Literal(0) / 2)")


  // 2,4,1,3,7,5,4,0,1,3,0,3,5,5,3,0
  // val r1 = (0 to 2000000).filter { i =>
  //   debug(s"=========================== $i =======================")
  //   val result = loop(programState.copy(registers = programState.registers.copy(a = i)))
  //   result.slice(0, 4) == List(2, 4, 1, 3) //programState.raw
  // }
  //
  // println("===============")
  // println(r1.mkString(","))
  // r1.sliding(2).foreach(s => println(s"s(1) - s(0) ${s(1) - s(0)}"))
  // println("===============")

  // val r2 = Iterator.iterate((List.empty[Long], 465405, 1)) { case (ps, i, c8) =>
  //   val result = loop(programState.copy(registers = programState.registers.copy(a = i)))
  //   if (c8 == 8)
  //     (result, i + 466944, 1)
  //   else
  //     (result, i + 8192, c8 + 1)
  // }.take(1000).filter(_._1.slice(0, 6) == List(2, 4, 1, 3, 7, 5)).toList

  // val r2 = Iterator.iterate((List.empty[Long], 1546749L)) { case (ps, i) =>
  //   val result = loop(programState.copy(registers = programState.registers.copy(a = i)))
  //   (result, i + 4194304L)
  // }.take(10000).filter(_._1.slice(0, 10) == List(2, 4, 1, 3, 7, 5, 4, 0, 1, 3)).toList

  // val r2 = Iterator.iterate((List.empty[Long], 3247938045L)) { case (ps, i) =>
  //   val result = loop(programState.copy(registers = programState.registers.copy(a = i)))
  //   (result, i + 8589934592L)
  // }.take(100000).filter(_._1.slice(0, 16) == List(2, 4, 1, 3, 7, 5, 4, 0, 1, 3, 0, 3, 5, 5, 3, 0)).toList

  //8589934592
  //108107574778365
  val r2 = 108107574778365L


  // println("===============")
  // println(r2.mkString(","))
  // r2.sliding(2).foreach(s => println(s"s(1) - s(0) ${s(1)._2 - s(0)._2}"))
  // println("===============")
  // println(s"${108116164712957L - 8589934592L}")

  // val r1 = (300000000 to 400000000).par.find { i =>
  //   val result = loop(programState.copy(registers = programState.registers.copy(a = i)))
  //   result == programState.raw
  // }

  println(s"Result2: ${r2}")

  val input = List(2, 4, 1, 3, 7, 5, 4, 0, 1, 3, 0, 3, 5, 5, 3, 0)
  println(input.tails.toList.reverse.tail)
  println(Iterator.iterate(0L)( _ << 3).take(8).toList)

  0
}

