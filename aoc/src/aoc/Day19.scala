package aoc

import aoc.Decoder.splitTrim
import breeze.linalg.{DenseMatrix, DenseVector, manhattanDistance}
import cats.effect.*
import cats.effect.implicits.*
import cats.effect.unsafe.implicits.global
import cats.instances.either.*
import cats.instances.seq.*
import cats.instances.vector.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*

import scala.annotation.tailrec
import scala.collection.mutable
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

  case class Result(vs: VectorSet, p: Point) extends Exception:
    def toOption: Option[(VectorSet, Point)] = Some(vs -> p)

  private def align(beacons: VectorSet)(rotated: VectorSet): IO[Unit] = IO {
    for
      v1 <- beacons
      v2 <- rotated
      distance   = v1 - v2
      translated = rotated.map(_ + distance)
      if translated.intersect(beacons).size >= 12
    do throw Result(translated, distance)
    ()
  }

  private def align(beacons: VectorSet, report: Report): Option[(VectorSet, Point)] =
    def extract(t: Throwable): Option[(VectorSet, Point)] = t match
      case r: Result => r.toOption
      case _         => None
    val res =
      IO
        .parTraverseN(rotations.size)(report.rotate)(align(beacons))
        .redeem(extract, _ => None)
    res.unsafeRunSync()

  @tailrec
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
