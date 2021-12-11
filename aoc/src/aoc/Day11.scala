package aoc

import aoc.Decoder.splitTrim

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal

object Day11:

  final val GridSize = 10

  case class Point(x: Int, y: Int):
    def adjacent: Seq[Point] =
      for
        a <- (x - 1 max 0) to (x + 1 min GridSize - 1)
        b <- (y - 1 max 0) to (y + 1 min GridSize - 1)
        if a != x || b != y
      yield Point(a, b)

  class Grid(initial: Seq[Seq[Int]]):
    /** For debugging purposes. */
    override def toString: String =
      grid.map(_.map(i => if i > 9 then 0 else i).mkString).mkString("\n")

    /** Mutable Set that keeps track of positions that have flashed during this step. */
    private val flashed = mutable.Set.empty[Point]

    /** Mutable Array that holds the current grid state. */
    private val grid: Array[Array[Int]] =
      Array.fill(GridSize)(Array.fill(GridSize)(0))

    private def init(): Unit =
      flashed.clear()
      for
        y <- 0 until GridSize
        x <- 0 until GridSize
      do grid(y)(x) = initial(y)(x)

    private def reset(): Unit =
      flashed.foreach(p => grid(p.y)(p.x) = 0)
      flashed.clear()

    private def increment(): Unit =
      for
        y <- 0 until GridSize
        x <- 0 until GridSize
      do grid(y)(x) += 1

    private def willFlash: Seq[Point] =
      for
        y <- 0 until GridSize
        x <- 0 until GridSize
        if grid(y)(x) > 9
        p = Point(x, y)
        if !flashed(p)
      yield p

    private def flash(flashing: Seq[Point]): Unit =
      flashing.flatMap(_.adjacent).foreach { p =>
        if !flashed(p) then grid(p.y)(p.x) += 1
      }
      flashed ++= flashing

    @tailrec
    private def loop(): Int =
      willFlash match
        case Nil => flashed.size
        case points =>
          flash(points)
          loop()

    private def next(): Int =
      reset()
      increment()
      loop()

    /** Part 1, run for `n` steps. */
    def run(n: Int): Int =
      init()
      (1 to n).foldLeft(0) { (acc, _) => acc + next() }

    @tailrec
    private def all(step: Int): Int =
      if next() < GridSize * GridSize then all(step + 1)
      else step

    /** Part 2, run until all octopuses flash at once. */
    def all(): Int =
      init()
      all(1)
  end Grid

  given Decoder[Grid] with
    def decode(s: String): Either[String, Grid] = try
      val lines = s
        .splitTrim("""\n""")
        .map(_.toCharArray.nn.map(_ - '0').toSeq)
      assert(lines.size == GridSize)
      assert(lines.head.size == GridSize)
      Right(Grid(lines))
    catch case NonFatal(e) => Left(e.getMessage.nn)
