package aoc

import breeze.linalg.CSCMatrix
import scala.util.control.NonFatal

object Day05:

  case class Segment(x1: Int, y1: Int, x2: Int, y2: Int):

    override def toString: String =
      s"$x1,$y1 -> $x2,$y2"

    def coords(diagonals: Boolean): Seq[(Int, Int)] =
      extension (a: Int)
        infix def comp(b: Int): Boolean =
          if diagonals then a <= b
          else a == b

      if (x1 comp x2) && y1 < y2 then (x1 to x2).zipAll(y1 to y2, x1, y1)
      else if (x1 comp x2) && y2 < y1 then (x1 to x2).zipAll(y1 to y2 by -1, x1, y1)
      else if x2 < x1 && (y1 comp y2) then (x1 to x2 by -1).zipAll(y1 to y2, x1, y1)
      else if x1 < x2 && (y1 comp y2) then (x1 to x2).zipAll(y1 to y2, x1, y1)
      else if x2 < x1 && (y2 comp y1) then (x1 to x2 by -1).zipAll(y1 to y2 by -1, x1, y1)
      else Seq.empty

  def process(segments: Seq[Segment], diagonals: Boolean): Int =
    val builder = CSCMatrix.Builder[Int](-1, -1)
    for {
      segment <- segments
      (r, c)  <- segment.coords(diagonals)
    } builder.add(r, c, 1)
    val matrix = builder.result()
    matrix.findAll(_ > 1).size

  given Decoder[Segment] with
    private val pattern = """(\d+),(\d+) -> (\d+),(\d+)""".r

    def decode(s: String): Either[String, Segment] = try
      s match
        case pattern(a, b, c, d) => Right(Segment(a.toInt, b.toInt, c.toInt, d.toInt))
        case _                   => Left(s"unrecognized pattern $s")
    catch case NonFatal(e) => Left(e.getMessage.nn)
