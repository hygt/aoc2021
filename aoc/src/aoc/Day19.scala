package aoc

import aoc.Decoder.splitTrim
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.manhattanDistance
import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.traverse.*

import scala.collection.mutable
import scala.util.control.NonFatal
import scala.util.control.NonLocalReturns.*

object Day19:

  case class Point(x: Int, y: Int, z: Int):
    def vector: DenseVector[Int] = DenseVector(x, y, z)

  case class Report(points: Seq[Point]):
    val vectors: Set[DenseVector[Int]] =
      points.map(_.vector).toSet

    def rotate(rot: DenseMatrix[Int]): Set[DenseVector[Int]] =
      vectors.map(v => rot * v)

  private val rotations: Seq[DenseMatrix[Int]] = Seq(
    DenseMatrix((1, 0, 0), (0, 1, 0), (0, 0, 1)),
    DenseMatrix((1, 0, 0), (0, -1, 0), (0, 0, -1)),
    DenseMatrix((1, 0, 0), (0, 0, -1), (0, 1, 0)),
    DenseMatrix((1, 0, 0), (0, 0, 1), (0, -1, 0)),
    DenseMatrix((-1, 0, 0), (0, -1, 0), (0, 0, 1)),
    DenseMatrix((-1, 0, 0), (0, 1, 0), (0, 0, -1)),
    DenseMatrix((-1, 0, 0), (0, 0, -1), (0, -1, 0)),
    DenseMatrix((-1, 0, 0), (0, 0, 1), (0, 1, 0)),
    DenseMatrix((0, 1, 0), (-1, 0, 0), (0, 0, 1)),
    DenseMatrix((0, 1, 0), (1, 0, 0), (0, 0, -1)),
    DenseMatrix((0, 1, 0), (0, 0, 1), (1, 0, 0)),
    DenseMatrix((0, 1, 0), (0, 0, -1), (-1, 0, 0)),
    DenseMatrix((0, -1, 0), (1, 0, 0), (0, 0, 1)),
    DenseMatrix((0, -1, 0), (-1, 0, 0), (0, 0, -1)),
    DenseMatrix((0, -1, 0), (0, 0, 1), (-1, 0, 0)),
    DenseMatrix((0, -1, 0), (0, 0, -1), (1, 0, 0)),
    DenseMatrix((0, 0, 1), (0, 1, 0), (-1, 0, 0)),
    DenseMatrix((0, 0, 1), (0, -1, 0), (1, 0, 0)),
    DenseMatrix((0, 0, 1), (1, 0, 0), (0, 1, 0)),
    DenseMatrix((0, 0, 1), (-1, 0, 0), (0, -1, 0)),
    DenseMatrix((0, 0, -1), (0, -1, 0), (-1, 0, 0)),
    DenseMatrix((0, 0, -1), (0, 1, 0), (1, 0, 0)),
    DenseMatrix((0, 0, -1), (1, 0, 0), (0, -1, 0)),
    DenseMatrix((0, 0, -1), (-1, 0, 0), (0, 1, 0))
  )

  private def align(beacons: Set[DenseVector[Int]], report: Report): Option[(Set[DenseVector[Int]], DenseVector[Int])] =
    returning {
      for
        rot <- rotations
        rotated = report.rotate(rot)
        v1 <- beacons
        v2 <- rotated
        distance   = v1 - v2
        translated = rotated.map(_ + distance)
        if translated.intersect(beacons).size >= 12
      do throwReturn[Option[(Set[DenseVector[Int]], DenseVector[Int])]](Some(translated -> distance))

      None
    }

  private def loop(
      aligned: Set[DenseVector[Int]],
      found: Set[DenseVector[Int]],
      tail: Set[Report]
  ): (Set[DenseVector[Int]], Set[DenseVector[Int]]) =
    val (bs, ss, rs) = tail.foldLeft((aligned, found, Set.empty[Report])) { case ((beacons, scanners, rest), report) =>
      align(beacons, report) match
        case Some((b, s)) => (beacons ++ b, scanners + s, rest)
        case None         => (beacons, scanners, rest + report)
    }
    if rs.isEmpty then bs -> ss
    else loop(bs, ss, rs)

  private def aligned(reports: Seq[Report]): (Set[DenseVector[Int]], Set[DenseVector[Int]]) = reports match
    case head +: tail => loop(head.vectors, Set.empty, tail.toSet)
    case _            => throw new IllegalArgumentException("reports cannot be empty")

  def count(reports: Seq[Report]): Int =
    val (beacons, _) = aligned(reports)
    beacons.size

  def maxDistance(reports: Seq[Report]): Int =
    val (_, scanners) = aligned(reports)
    scanners.toSeq
      .combinations(2)
      .collect { case Seq(s1, s2) =>
        manhattanDistance(s1, s2).toInt
      }
      .max

  given Decoder[Seq[Report]] with
    def decode(s: String): Either[String, Seq[Report]] =
      s.splitTrim("""\n\n""").traverse { scanner =>
        scanner.splitTrim("""\n""") match
          case head +: tail =>
            tail.traverse(summon[Decoder[Point]].decode).map(Report.apply)
          case _ => Left("not a scanner")
      }

  given Decoder[Point] with
    def decode(s: String): Either[String, Point] = try
      s.splitTrim(",") match
        case Vector(x, y, z) => Right(Point(x.toInt, y.toInt, z.toInt))
        case _               => Left("not a vector")
    catch case NonFatal(e) => Left(e.getMessage.nn)
