package aoc

import breeze.linalg.CSCMatrix

import scala.util.control.NonFatal

object Day05:

  case class Segment(x1: Int, y1: Int, x2: Int, y2: Int):

    def straight: Boolean = x1 == x2 || y1 == y2

    override def toString: String = s"$x1,$y1 -> $x2,$y2"

    def coords: Seq[(Int, Int)] =
      val xs = x1 to x2 by (if x1 < x2 then 1 else -1)
      val ys = y1 to y2 by (if y1 < y2 then 1 else -1)
      xs.zipAll(ys, x1, y1)

  def naive(segments: Seq[Segment], diagonals: Boolean): Int =
    val positions = for {
      segment <- segments
      if diagonals || segment.straight
      coord <- segment.coords
    } yield coord
    val overlapping = positions.diff(positions.distinct)
    overlapping.distinct.size

  def process(segments: Seq[Segment], diagonals: Boolean): Int =
    val grid = CSCMatrix.Builder[Int](-1, -1)
    for {
      segment <- segments
      if diagonals || segment.straight
      (r, c) <- segment.coords
    } grid.add(r, c, 1)
    grid.result().findAll(_ > 1).size

  given Decoder[Segment] with
    private val pattern = """(\d+),(\d+) -> (\d+),(\d+)""".r

    def decode(s: String): Either[String, Segment] = try
      s match
        case pattern(a, b, c, d) => Right(Segment(a.toInt, b.toInt, c.toInt, d.toInt))
        case _                   => Left(s"unrecognized pattern $s")
    catch case NonFatal(e) => Left(e.getMessage.nn)
