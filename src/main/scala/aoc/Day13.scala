package aoc

import aoc.Decoder.splitTrim

import scala.util.control.NonFatal

object Day13:

  case class Point(x: Int, y: Int)

  enum Fold:
    case X(x: Int)
    case Y(y: Int)

  extension (range: Range)
    /** Like `splitAt` but discards the midpoint, zips elements in the first part that overlap those in the reversed
      * second.
      */
    def splitFlipZip(n: Int): Seq[(Int, Int)] =
      val first  = range.take(n)
      val second = range.drop(n + 1)
      val diff   = first.size - second.size
      if diff > 0 then first.drop(diff).zip(second.reverse)
      else first.zip(second.reverse.drop(-diff))

  case class Paper(points: Set[Point], folds: Seq[Fold]):

    private def fold(n: Int): (Set[Point], Point) =
      val boundaries = Point(points.map(_.x).max, points.map(_.y).max)
      folds.take(n).foldLeft((points, boundaries)) { case ((set, max), fold) =>
        fold match
          case Fold.X(x) =>
            val folded = for
              y        <- 0 to max.y
              (x1, x2) <- (0 to max.x).splitFlipZip(x)
              if set(Point(x1, y)) || set(Point(x2, y))
            yield Point(x1, y)
            (folded.toSet, max.copy(x = x - 1))
          case Fold.Y(y) =>
            val folded = for
              x        <- 0 to max.x
              (y1, y2) <- (0 to max.y).splitFlipZip(y)
              if set(Point(x, y1)) || set(Point(x, y2))
            yield Point(x, y1)
            (folded.toSet, max.copy(y = y - 1))
      }

    def run1: Int =
      val (folded, _) = fold(1)
      folded.size

    def run2: String =
      val (folded, max) = fold(folds.size)
      val grid          = Array.fill(max.y + 1)(Array.fill(max.x + 1)(' '))
      folded.foreach(p => grid(p.y)(p.x) = '█')
      grid.map(_.mkString).mkString("\n")

  end Paper

  given Decoder[Paper] with
    def decode(s: String): Either[String, Paper] = try
      s.splitTrim("""\n\n""") match
        case Vector(head, tail) =>
          val points = head
            .splitTrim("""\n""")
            .map:
              case s"$x,$y" => Point(x.toInt, y.toInt)
              case s        => throw new IllegalArgumentException(s"unrecognized point $s")
          val folds = tail
            .splitTrim("""\n""")
            .map:
              case s"fold along x=$x" => Fold.X(x.toInt)
              case s"fold along y=$y" => Fold.Y(y.toInt)
              case s                  => throw new IllegalArgumentException(s"unrecognized fold $s")
          Right(Paper(points.toSet, folds))
        case _ => Left("not a paper")
    catch case NonFatal(e) => Left(e.getMessage.nn)
