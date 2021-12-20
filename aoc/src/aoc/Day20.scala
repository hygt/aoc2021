package aoc

import aoc.Day09.Point
import aoc.Decoder.splitTrim

import scala.util.control.NonFatal

object Day20:

  case class Point(x: Int, y: Int)

  case class Trench(algo: Seq[Boolean], pixels: Set[Point], outside: Boolean):
    private val minX = pixels.map(_.x).min
    private val minY = pixels.map(_.y).min
    private val maxX = pixels.map(_.x).max
    private val maxY = pixels.map(_.y).max

    private def lit(x: Int, y: Int): Boolean =
      val border = outside && algo.head // border is lit
      val binary = for
        j <- y - 1 to y + 1
        i <- x - 1 to x + 1
      yield
        if border && (i < minX || i > maxX || j < minY || j > maxY) then 1
        else if pixels(Point(i, j)) then 1
        else 0

      val index = binary.foldLeft(0) { (acc, x) => (acc * 2) + x }
      algo(index)

    private def enhance: Seq[Point] =
      for
        x <- minX - 1 to maxX + 1
        y <- minY - 1 to maxY + 1
        if lit(x, y)
      yield Point(x, y)

    def run(step: Int): Int =
      if step == 0 then pixels.size
      else Trench(algo, enhance.toSet, !outside).run(step - 1)

    override def toString: String =
      val grid = Array.fill(maxY - minY + 1)(Array.fill(maxX - minX + 1)(' '))
      pixels.foreach(p => grid(p.y - minY)(p.x - minX) = '█')
      grid.map(_.mkString).mkString("\n")

  given Decoder[Trench] with
    def decode(s: String): Either[String, Trench] = try
      s.splitTrim("""\n\n""") match
        case Seq(head, tail) =>
          val e = head.replaceAll("""\s""", "").nn.map(_ == '#')
          val p =
            for
              (line, y) <- tail.splitTrim("""\n""").zipWithIndex
              (char, x) <- line.zipWithIndex
              if char == '#'
            yield Point(x, y)
          Right(Trench(e, p.toSet, false))
        case _ => Left("not a trench")
    catch case NonFatal(e) => Left(e.getMessage.nn)
