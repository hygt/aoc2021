package aoc

import aoc.Decoder.splitTrim
import breeze.linalg.{DenseMatrix, DenseVector}
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

  case class Polymer(sequence: String, pairs: Map[Pair, Char]):
    private def result(pairset: PairSet): Long =
      val first = sequence.head
      val counts = pairset.toSeq.foldMap { (pair, count) =>
        if pair.last == first then Map(pair.last -> (count + 1L))
        else Map(pair.last                       -> count)
      }.values
      counts.max - counts.min

    def run(n: Int): Long =
      val pairset = (1 to n).foldLeft(PairSet(sequence)) { (prev, i) =>
        prev.toSeq.foldMap { case (pair, count) =>
          val c = pairs(pair)
          Map(Pair(pair.head, c) -> count, Pair(c, pair.last) -> count)
        }
      }
      result(pairset)

    def linalg(n: Int): Long =
      result(LinAlg(this).run(n))

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

  /** Alternative solution. */
  private class LinAlg(polymer: Polymer):
    private val size = polymer.pairs.size
    private val p2i  = polymer.pairs.keys.zipWithIndex.toMap
    private val i2p  = polymer.pairs.keys.zipWithIndex.map(_.swap).toMap

    private val matrix: DenseMatrix[Long] =
      val dm = DenseMatrix.zeros[Long](size, size)
      p2i.foreach { case (pair, i) =>
        val c  = polymer.pairs(pair)
        val y1 = p2i(Pair(pair.head, c))
        val y2 = p2i(Pair(c, pair.last))
        dm.update(y1, i, 1L)
        dm.update(y2, i, 1L)
      }
      dm

    private val vector: DenseVector[Long] =
      val pairset = PairSet(polymer.sequence)
      DenseVector.tabulate(size) { i =>
        pairset.getOrElse(i2p(i), 0L)
      }

    def run(n: Int): PairSet =
      val fold = (1 to n).foldLeft(vector)((acc, i) => matrix * acc)
      fold.toScalaVector.zipWithIndex.foldMap { (count, i) =>
        Map(i2p(i) -> count)
      }
