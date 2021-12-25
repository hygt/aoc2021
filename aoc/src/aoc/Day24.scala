package aoc

import aoc.Day24.Instruction.*

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal

object Day24:

  private val registers: Array[Long] = Array.fill(4)(0L)

  private val input: mutable.Queue[Int] = mutable.Queue.empty[Int]

  private def init(): Unit =
    input.clear()
    for i <- 0 until 4 do registers(i) = 0

  enum Register:
    def store(v: Long): Unit = registers(ordinal) = v
    def get: Long            = registers(ordinal)

    case W, X, Y, Z

  enum Operand:
    def get: Long = this match
      case L(v) => v.toLong
      case R(r) => r.get

    case L(v: Int)
    case R(r: Register)

  enum Instruction:
    def run(): Unit = this match
      case Inp(r)    => r.store(input.dequeue())
      case Add(a, b) => a.store(a.get + b.get)
      case Mul(a, b) => a.store(a.get * b.get)
      case Div(a, b) => a.store(a.get / b.get)
      case Mod(a, b) => a.store(a.get % b.get)
      case Eql(a, b) => if a.get == b.get then a.store(1) else a.store(0)

    case Inp(r: Register)
    case Add(a: Register, b: Operand)
    case Mul(a: Register, b: Operand)
    case Div(a: Register, b: Operand)
    case Mod(a: Register, b: Operand)
    case Eql(a: Register, b: Operand)

  def process(instructions: Seq[Instruction])(model: Long): Option[Long] =
    init()
    input.addAll(model.toString.toCharArray.nn.map(_.asDigit))
    instructions.foreach(_.run())
    assert(input.isEmpty)
    if Register.Z.get == 0L then Some(model)
    else None

  given Decoder[Instruction] with
    private def left(s: String): Register =
      Register.valueOf(s.toUpperCase.nn)

    private def right(s: String): Operand = s.toIntOption match
      case Some(v) => Operand.L(v)
      case None    => Operand.R(left(s))

    def decode(s: String): Either[String, Instruction] = try
      s match
        case s"inp $a"    => Right(Inp(left(a)))
        case s"add $a $b" => Right(Add(left(a), right(b)))
        case s"mul $a $b" => Right(Mul(left(a), right(b)))
        case s"div $a $b" => Right(Div(left(a), right(b)))
        case s"mod $a $b" => Right(Mod(left(a), right(b)))
        case s"eql $a $b" => Right(Eql(left(a), right(b)))
        case _            => Left(s"unrecognized instruction: $s")
    catch case NonFatal(e) => Left(e.getMessage.nn)
