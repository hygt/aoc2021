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
    def splitFlipZip(n: Int) =
      val first  = range.take(n)
      val second = range.drop(n + 1)
      val diff   = first.size - second.size
      if diff > 0 then first.drop(diff).zip(second.reverse)
      else first.zip(second.reverse.drop(-diff))

  case class Paper(points: Seq[Point], folds: Seq[Fold]):
    private var maxX                        = 0
    private var maxY                        = 0
    private var grid: Array[Array[Boolean]] = Array.empty

    private def init(): Unit =
      maxX = points.map(_.x).max
      maxY = points.map(_.y).max
      grid = Array.fill(maxY + 1)(Array.fill(maxX + 1)(false))
      points.foreach(p => grid(p.y)(p.x) = true)

    private def countAll: Int =
      grid.map(_.count(identity)).sum

    private def clearAfter(x: Int = 0, y: Int = 0): Unit =
      for
        i <- x to maxX
        j <- y to maxY
      do grid(j)(i) = false

    def step(n: Int = folds.size): Int =
      init()
      folds.take(n).foreach {
        case Fold.X(x) =>
          for
            y        <- 0 to maxY
            (x1, x2) <- (0 to maxX).splitFlipZip(x)
          do grid(y)(x1) |= grid(y)(x2)
          clearAfter(x = x)
          maxX = x - 1
        case Fold.Y(y) =>
          for
            x        <- 0 to maxX
            (y1, y2) <- (0 to maxY).splitFlipZip(y)
          do grid(y1)(x) |= grid(y2)(x)
          clearAfter(y = y)
          maxY = y - 1
      }
      countAll

    override def toString: String =
      grid
        .take(maxY + 1)
        .map { line =>
          line.take(maxX + 1).map(if _ then '█' else ' ').mkString
        }
        .mkString("\n")

  end Paper

  given Decoder[Paper] with
    def decode(s: String): Either[String, Paper] = try
      s.splitTrim("""\n\n""") match
        case Vector(head, tail) =>
          val points = head.splitTrim("""\n""").map {
            case s"$x,$y" => Point(x.toInt, y.toInt)
            case s        => throw new IllegalArgumentException(s"unrecognized point $s")
          }
          val folds = tail.splitTrim("""\n""").map {
            case s"fold along x=$x" => Fold.X(x.toInt)
            case s"fold along y=$y" => Fold.Y(y.toInt)
            case s                  => throw new IllegalArgumentException(s"unrecognized fold $s")
          }
          Right(Paper(points, folds))
        case _ => Left("not a paper")
    catch case NonFatal(e) => Left(e.getMessage.nn)
