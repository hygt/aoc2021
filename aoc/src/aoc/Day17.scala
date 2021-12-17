package aoc

import scala.annotation.tailrec
import scala.util.control.NonFatal

object Day17:

  case class Velocity(x: Int, y: Int):
    def next: Velocity =
      val vx =
        if x > 0 then x - 1
        else if x < 0 then x + 1
        else 0
      val vy = y - 1
      Velocity(vx, vy)

  case class Point(x: Int, y: Int):
    def inc(v: Velocity): Point =
      Point(x + v.x, y + v.y)

  case class Target(ll: Point, ur: Point):
    private def inside(point: Point): Boolean =
      ll.x <= point.x && point.x <= ur.x && ll.y <= point.y && point.y <= ur.y

    private def missed(point: Point): Boolean =
      point.x > ur.x || point.y < ll.y

    @tailrec
    private def explore(prev: Point, velocity: Velocity, maxH: Int): Option[Int] =
      val curP   = prev.inc(velocity)
      val curMax = maxH max curP.y
      if inside(curP) then Some(curMax)
      else if missed(curP) then None
      else explore(curP, velocity.next, curMax)

    def find(highest: Boolean): Int =
      val maxima = for
        h <- 1 to ur.x
        v <- (if highest then 1 else ll.y) to ll.y.abs
        m <- explore(Point(0, 0), Velocity(h, v), 0)
      yield m
      if highest then maxima.max
      else maxima.size

  given Decoder[Target] with
    def decode(s: String): Either[String, Target] = try
      s match
        case s"target area: x=$x1..$x2, y=$y1..$y2" =>
          Right(Target(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt)))
        case _ => Left("not a target")
    catch case NonFatal(e) => Left(e.getMessage.nn)
