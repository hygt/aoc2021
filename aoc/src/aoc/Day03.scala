package aoc

import cats.Monoid
import cats.implicits.*
import scodec.bits.BitVector

import scala.annotation.tailrec

object Day03:

  opaque type Diag = BitVector

  given Decoder[Diag] with
    def decode(s: String): Diag =
      BitVector.fromValidBin(s)

  opaque type Counter = IndexedSeq[Int]

  given Monoid[Counter] with
    val empty: Counter = IndexedSeq.empty
    def combine(x: Counter, y: Counter): Counter =
      if x.isEmpty then y
      else if y.isEmpty then x
      else x.zip(y).map(_ + _)

  private def toCounter(diag: Diag): Counter =
    diag.toIndexedSeq.map {
      case true  => 1
      case false => -1
    }

  private def powerConsumption(c: Counter): Int =
    val (epsilon, gamma) = c.foldLeft((0, 0)) { case ((e, g), count) =>
      if count > 0 then ((e << 1) + 1, g << 1)
      else if count < 0 then (e << 1, (g << 1) + 1)
      else (e << 1, g << 1)
    }
    epsilon * gamma

  def process1(report: Seq[Diag]): Int =
    powerConsumption(report.foldMap(toCounter))

  @tailrec
  private def recurse(diags: Seq[Diag], i: Int, p: (Int, Int) => Boolean): Int = diags match
    case Nil         => throw new IllegalStateException
    case Seq(result) => result.toInt(signed = false)
    case _ =>
      val (left, right) = diags.partition(_.get(i))
      if p(left.size, right.size) then recurse(left, i + 1, p)
      else recurse(right, i + 1, p)

  def process2(report: Seq[Diag]): Int =
    val o2  = recurse(report, 0, Ordering[Int].gteq)
    val co2 = recurse(report, 0, Ordering[Int].lt)
    o2 * co2
