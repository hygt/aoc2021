package aoc

import aoc.Decoder.splitTrim
import scala.util.control.NonFatal

object Day25:

  case class Point(x: Int, y: Int)

  class Grid(h: Int, w: Int, eb: Set[Point], sb: Set[Point], free: Set[Point]):
    private def east(p: Point): Point =
      Point((p.x + 1) % w, p.y)

    private def south(p: Point): Point =
      Point(p.x, (p.y + 1) % h)

    private def next(prev: Set[Point], f: Point => Point, unoccupied: Set[Point]): Option[(Set[Point], Set[Point])] =
      val (yes, no) = prev.partition(p => unoccupied(f(p)))
      if yes.isEmpty then None
      else
        val add = yes.map(f)
        Some((no ++ add, unoccupied ++ yes -- add))

    def run(step: Int = 1): Int =
      next(eb, east, free) match
        case None =>
          next(sb, south, free) match
            case None =>
              step
            case Some((nsouth, nnfree)) =>
              Grid(h, w, eb, nsouth, nnfree).run(step + 1)
        case Some((neast, nfree)) =>
          next(sb, south, nfree) match
            case None =>
              Grid(h, w, neast, sb, nfree).run(step + 1)
            case Some((nsouth, nnfree)) =>
              Grid(h, w, neast, nsouth, nnfree).run(step + 1)

  given Decoder[Grid] with
    def decode(s: String): Either[String, Grid] = try
      val grid = s.splitTrim("""\n""")
      val h    = grid.size
      val w    = grid.head.length
      val points = for
        y <- 0 until h
        x <- 0 until w
      yield grid(y)(x) -> Point(x, y)
      val groups = points.groupMap(_._1)(_._2)
      Right(Grid(h, w, groups('>').toSet, groups('v').toSet, groups('.').toSet))
    catch case NonFatal(e) => Left(e.getMessage.nn)
