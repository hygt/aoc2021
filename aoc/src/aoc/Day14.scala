package aoc

import aoc.Decoder.splitTrim
import cats.instances.map.*
import cats.instances.seq.*
import cats.syntax.foldable.*

import scala.util.control.NonFatal

object Day14:

  case class Pair(head: Char, last: Char):
    override def toString: String = s"$head$last"

  object Pair:
    def apply(s: Seq[Char]): Pair = s match
      case Seq(h, l) => Pair(h, l)
      case _         => throw new IllegalArgumentException("argument must be exactly two chars")

  private type PairSet = Map[Pair, Long]

  object PairSet:
    def apply(s: String): PairSet =
      s.sliding(2).toSeq.foldMap(w => Map(Pair(w) -> 1L))

  def result(pairset: PairSet, first: Char): Long =
    val counts = pairset.toSeq
      .foldMap((pair, count) => Map(pair.last -> count))
      .updatedWith(first)(_.map(_ + 1L)) // count the first char
      .values
    counts.max - counts.min

  case class Polymer(sequence: String, pairs: Map[Pair, Char]):
    def run(n: Int): Long =
      val pairset = (1 to n).foldLeft(PairSet(sequence)) { (prev, i) =>
        prev.toSeq.foldMap { case (pair, count) =>
          val c = pairs(pair)
          Map(Pair(pair.head, c) -> count, Pair(c, pair.last) -> count)
        }
      }
      result(pairset, sequence.head)

  given Decoder[Polymer] with
    def decode(s: String): Either[String, Polymer] = try
      s.splitTrim("""\n\n""") match
        case Vector(head, tail) =>
          assert(head.nonEmpty)
          val pairs = tail
            .splitTrim("""\n""")
            .map {
              case s"$s -> $c" =>
                assert(s.length == 2)
                assert(c.length == 1)
                Pair(s) -> c(0)
              case s => throw new IllegalArgumentException(s"unrecognized mapping $s")
            }
            .toMap
          Right(Polymer(head, pairs))
        case _ => Left("not a polymer template")
    catch case NonFatal(e) => Left(e.getMessage.nn)
