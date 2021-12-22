package aoc

import scala.util.control.NonFatal

object Day22:

  final val limit = 50

  case class Point(x: Int, y: Int, z: Int):
    def outside: Boolean =
      x.abs > limit || y.abs > limit || z.abs > limit

    def :<(other: Point): Boolean =
      x < other.x || y < other.y || z < other.z

    infix def min(other: Point): Point =
      Point(x min other.x, y min other.y, z min other.z)

    infix def max(other: Point): Point =
      Point(x max other.x, y max other.y, z max other.z)

  case class Cuboid(lo: Point, hi: Point):
    def volume: Long =
      (hi.x - lo.x + 1L) * (hi.y - lo.y + 1L) * (hi.z - lo.z + 1L)

    def intersect(other: Cuboid): Option[Cuboid] =
      if other.hi :< lo || hi :< other.lo then None
      else Some(Cuboid(lo max other.lo, hi min other.hi))

  case class Step(cu: Cuboid, on: Boolean):
    def outside: Boolean =
      cu.lo.outside || cu.hi.outside

  private class Naive(points: Set[Point]):
    def +(step: Step): Naive =
      val pts = for
        x <- step.cu.lo.x to step.cu.hi.x
        y <- step.cu.lo.y to step.cu.hi.y
        z <- step.cu.lo.z to step.cu.hi.z
      yield Point(x, y, z)
      if step.on then Naive(points ++ pts)
      else Naive(points -- pts)

    def count: Int = points.size

  private class Fast(prev: Seq[(Cuboid, Int)]):
    def +(step: Step): Fast =
      val intersections = prev.flatMap { case (cuboid, lit) =>
        step.cu.intersect(cuboid).map(cu => cu -> -lit)
      }
      if step.on then Fast(prev ++ intersections :+ (step.cu -> 1))
      else Fast(prev ++ intersections)

    def count: Long =
      prev.foldLeft(0L) { case (acc, (cuboid, lit)) =>
        acc + cuboid.volume * lit
      }

  def count(steps: Seq[Step], init: Boolean): Long =
    steps
      .foldLeft(Fast(Seq.empty)) { (acc, step) =>
        if init && step.outside then acc
        else acc + step
      }
      .count

  given Decoder[Step] with
    def decode(s: String): Either[String, Step] = try
      s match
        case s"on x=$x1..$x2,y=$y1..$y2,z=$z1..$z2" =>
          Right(Step(Cuboid(Point(x1.toInt, y1.toInt, z1.toInt), Point(x2.toInt, y2.toInt, z2.toInt)), true))
        case s"off x=$x1..$x2,y=$y1..$y2,z=$z1..$z2" =>
          Right(Step(Cuboid(Point(x1.toInt, y1.toInt, z1.toInt), Point(x2.toInt, y2.toInt, z2.toInt)), false))
        case _ => Left("not a reboot step")
    catch case NonFatal(e) => Left(e.getMessage.nn)
