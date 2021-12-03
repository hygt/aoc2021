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

  private opaque type Counter = IndexedSeq[Int]

  private object Counter:
    def apply(diag: Diag): Counter =
      diag.toIndexedSeq.map {
        case true  => 1
        case false => -1
      }

  private given Monoid[Counter] with
    val empty: Counter = IndexedSeq.empty
    def combine(x: Counter, y: Counter): Counter =
      if x.isEmpty then y
      else if y.isEmpty then x
      else x.zip(y).map(_ + _)

  private def rating(counter: Counter, p: Int => Boolean): Int =
    val bits = counter.map { c => if p(c) then '1' else '0' }.mkString
    BitVector.fromValidBin(bits).toInt(signed = false)

  def process1(report: Seq[Diag]): Int =
    val combined = report.foldMap(Counter.apply)
    val epsilon  = rating(combined, _ > 0)
    val gamma    = rating(combined, _ < 0)
    epsilon * gamma

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
