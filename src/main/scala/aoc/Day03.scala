package aoc

import cats.Monoid
import cats.implicits.*

import scala.annotation.tailrec

object Day03:

  type Diag = String

  private type Counter = IndexedSeq[Int]

  private object Counter:
    def apply(diag: Diag): Counter =
      diag.map:
        case '1' => 1
        case '0' => -1

  private given Monoid[Counter] with
    val empty: Counter = IndexedSeq.empty
    def combine(x: Counter, y: Counter): Counter =
      if x.isEmpty then y
      else if y.isEmpty then x
      else x.zip(y).map(_ + _)

  private def rating(counter: Counter, p: Int => Boolean): Int =
    val bits = counter.map { c => if p(c) then '1' else '0' }.mkString
    Integer.parseUnsignedInt(bits, 2)

  def process1(report: Seq[Diag]): Int =
    val combined = report.foldMap(Counter.apply)
    val epsilon  = rating(combined, _ > 0)
    val gamma    = rating(combined, _ < 0)
    epsilon * gamma

  @tailrec
  private def recurse(diags: Seq[Diag], i: Int, p: (Int, Int) => Boolean): Int = diags match
    case Nil         => throw new IllegalStateException
    case Seq(result) => Integer.parseUnsignedInt(result, 2)
    case _ =>
      val (left, right) = diags.partition(_.charAt(i) == '1')
      if p(left.size, right.size) then recurse(left, i + 1, p)
      else recurse(right, i + 1, p)

  def process2(report: Seq[Diag]): Int =
    val o2  = recurse(report, 0, Ordering[Int].gteq)
    val co2 = recurse(report, 0, Ordering[Int].lt)
    o2 * co2
