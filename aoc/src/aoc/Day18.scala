package aoc

import cats.parse.{Numbers, Parser}

import scala.annotation.tailrec

object Day18:

  def reduce(snailfish: Seq[Nested]): Int = snailfish match
    case head +: tail =>
      tail
        .foldLeft(head) { (acc, x) =>
          acc + x
        }
        .magnitude
    case _ => -1

  def largest(snailfish: Seq[Nested]): Int =
    snailfish
      .combinations(2)
      .collect { case Seq(a, b) =>
        (a + b).magnitude max (b + a).magnitude
      }
      .max

  sealed trait Nested:
    @tailrec
    final def reduce: Nested =
      val (exploded, hasExploded) = explode
      if hasExploded then exploded.reduce
      else
        val (splitted, hasSplit) = split
        if hasSplit then splitted.reduce
        else this

    def +(that: Nested): Nested =
      Pair(this, that).reduce

    def magnitude: Int = this match
      case Val(v)            => v
      case Pair(left, right) => 3 * left.magnitude + 2 * right.magnitude

    private def pushdown(n: Nested, lv: Int, rv: Int): Nested = n match {
      case Pair(left, right) => Pair(pushdown(left, lv, 0), pushdown(right, 0, rv))
      case Val(v)            => Val(v + lv + rv)
    }

    private def inorder(n: Nested, level: Int): (Nested, Option[(Int, Int)]) = n match
      case p @ Pair(Val(vl), Val(vr)) =>
        if level < 4 then p -> None
        else Val(0)         -> Some((vl, vr))
      case p @ Pair(left, right) =>
        inorder(left, level + 1) match
          case (ln, None) =>
            inorder(right, level + 1) match
              case (rn, None)           => p                             -> None
              case (rn, Some((lv, rv))) => Pair(pushdown(ln, 0, lv), rn) -> Some((0, rv))
          case (ln, Some((lv, rv))) =>
            Pair(ln, pushdown(right, rv, 0)) -> Some((lv, 0))
      case v: Val => v -> None

    def explode: (Nested, Boolean) =
      val (n, e) = inorder(this, 0)
      n -> e.isDefined

    private def split(n: Nested, splitted: Boolean): (Nested, Boolean) = n match
      case _ if splitted => n -> true
      case Val(v) if v >= 10 =>
        val half = v / 2
        Pair(Val(half), Val(v - half)) -> true
      case Pair(left, right) =>
        val (ln, ls) = split(left, splitted)
        val (rn, rs) = split(right, ls)
        Pair(ln, rn) -> rs
      case _ => n -> false

    def split: (Nested, Boolean) = split(this, false)

  case class Pair(left: Nested, right: Nested) extends Nested:
    override def toString: String = s"[$left, $right]"

  case class Val(v: Int) extends Nested:
    override def toString: String = v.toString

  given Decoder[Nested] with
    private val `,` = Parser.char(',').void
    private val `[` = Parser.char('[').void
    private val `]` = Parser.char(']').void

    private val parser = Parser.recursive[Nested] { recurse =>
      val int  = Numbers.digits.map(d => Val(d.toInt))
      val pair = ((`[` *> recurse <* `,`) ~ recurse <* `]`).map { case (l: Nested, r: Nested) => Pair(l, r) }
      pair | int
    }

    def decode(s: String): Either[String, Nested] =
      parser.parseAll(s).left.map(_.toString)
