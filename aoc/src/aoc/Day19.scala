package aoc

import aoc.Decoder.splitTrim
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.manhattanDistance
import cats.instances.either.*
import cats.instances.vector.*
import cats.syntax.traverse.*

import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.*
import scala.util.control.NonFatal

object Day19:

  type Point = DenseVector[Int]

  type VectorSet = Set[DenseVector[Int]]

  case class Report(vectors: VectorSet):
    val rotate: Seq[VectorSet] =
      rotations.map(rot => vectors.map(v => rot * v))

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

  private def align(beacons: VectorSet, rotated: VectorSet): Future[Option[(VectorSet, Point)]] = Future {
    val found = for
      v1 <- beacons
      v2 <- rotated
      distance   = v1 - v2
      translated = rotated.map(_ + distance)
      if translated.intersect(beacons).size >= 12
    yield translated -> distance
    found.headOption
  }

  /** Using Futures as a cheap way to parallelize this step. Short-circuiting on the first success would be better. */
  private def align(beacons: VectorSet, report: Report): Option[(VectorSet, Point)] =
    val res = Future
      .traverse(report.rotate) { rotated =>
        align(beacons, rotated)
      }
      .map(_.flatten.headOption)
    Await.result(res, 1.minute)

  private def loop(aligned: VectorSet, found: VectorSet, tail: Set[Report]): (VectorSet, VectorSet) =
    val (bs, ss, rs) = tail.foldLeft((aligned, found, Set.empty[Report])) { case ((beacons, scanners, rest), report) =>
      align(beacons, report) match
        case Some((b, s)) => (beacons ++ b, scanners + s, rest)
        case None         => (beacons, scanners, rest + report)
    }
    if rs.isEmpty then bs -> ss
    else loop(bs, ss, rs)

  private def aligned(reports: Seq[Report]): (VectorSet, VectorSet) = reports match
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
            tail.traverse(summon[Decoder[Point]].decode).map(ps => Report(ps.toSet))
          case _ => Left("not a scanner")
      }

  given Decoder[Point] with
    def decode(s: String): Either[String, Point] = try
      s.splitTrim(",") match
        case Vector(x, y, z) => Right(DenseVector(x.toInt, y.toInt, z.toInt))
        case _               => Left("not a vector")
    catch case NonFatal(e) => Left(e.getMessage.nn)
