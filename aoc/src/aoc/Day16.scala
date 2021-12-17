package aoc

import cats.parse.Accumulator.nonEmptyListAccumulator0
import cats.parse.Parser as P
import cats.syntax.apply.*
import scodec.bits.BitVector

object Day16:

  def part1(hex: String): Int =
    Parsers.packet
      .parse(BitVector.fromValidHex(hex).toBin)
      .map { case (padding, packet) => packet.versionNumbers }
      .getOrElse(-1)

  def part2(hex: String): Long =
    Parsers.packet
      .parse(BitVector.fromValidHex(hex).toBin)
      .map { case (padding, packet) => packet.eval }
      .getOrElse(-1L)

  sealed trait Packet:
    def version: Int
    def id: Int
    def versionNumbers: Int
    def eval: Long

  case class Literal(version: Int, value: Long) extends Packet:
    val id: Int             = 4
    def versionNumbers: Int = version
    def eval: Long          = value

  case class Operator(version: Int, id: Int, sub: Seq[Packet]) extends Packet:
    def versionNumbers: Int =
      version + sub.map(_.versionNumbers).sum
    def eval: Long = id match
      case 0 => sub.map(_.eval).sum
      case 1 => sub.map(_.eval).product
      case 2 => sub.map(_.eval).min
      case 3 => sub.map(_.eval).max
      case 5 =>
        sub match
          case Seq(a, b) if a.eval > b.eval => 1
          case _                            => 0
      case 6 =>
        sub match
          case Seq(a, b) if a.eval < b.eval => 1
          case _                            => 0
      case 7 =>
        sub match
          case Seq(a, b) if a.eval == b.eval => 1
          case _                             => 0

  private object Parsers:
    def bits(n: Int): P[Int] = P.length(n).map(s => Integer.parseUnsignedInt(s, 2))

    val group: P[String]       = P.char('1') *> P.length(4)
    val terminating: P[String] = P.char('0') *> P.length(4)

    val value: P[Long] = (group.rep0.with1 ~ terminating).map { (gs, t) =>
      java.lang.Long.parseUnsignedLong((gs.flatten ++ t).mkString, 2)
    }

    val literal: P[Literal] = ((bits(3) <* P.string("100")) ~ value).map(Literal.apply)

    val byCount: P[Seq[Packet]] =
      P.char('1') *> bits(11).flatMap { count => packet.repExactlyAs(count).map(_.toList) }

    val byLength: P[Seq[Packet]] =
      P.char('0') *> bits(15).flatMap(P.length).map { s =>
        packet.rep(1).map(_.toList).parseAll(s).getOrElse(List.empty)
      }

    val subpackets: P[Seq[Packet]] = byCount.backtrack | byLength

    val operator: P[Operator] = (bits(3), bits(3), subpackets).mapN(Operator.apply)

    def packet: P[Packet] = literal.backtrack | operator
