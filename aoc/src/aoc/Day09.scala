package aoc

import scala.util.control.NonFatal

object Day09:

  case class Point(x: Int, y: Int):
    def up: Point    = Point(x, y - 1)
    def down: Point  = Point(x, y + 1)
    def left: Point  = Point(x - 1, y)
    def right: Point = Point(x + 1, y)

  private def minima(heights: Seq[Vector[Int]]): Seq[Point] =
    val l = heights.size
    val w = heights.head.size
    for
      y <- 0 until l
      x <- 0 until w
      h = heights(y)(x)
      if x == 0 || h < heights(y)(x - 1)
      if x == w - 1 || h < heights(y)(x + 1)
      if y == 0 || h < heights(y - 1)(x)
      if y == l - 1 || h < heights(y + 1)(x)
    yield Point(x, y)

  def risk1(heights: Seq[Vector[Int]]): Int =
    minima(heights).map(p => heights(p.y)(p.x) + 1).sum

  private def dfs(heights: Seq[Vector[Int]], start: Point): Int =
    val l = heights.size
    val w = heights.head.size
    def recurse(p: Point, visited: Set[Point]): (Int, Set[Point]) = p match
      case Point(-1, _) | Point(_, -1) | Point(`w`, _) | Point(_, `l`) =>
        (0, visited + p)
      case _ if visited(p) || heights(p.y)(p.x) == 9 =>
        (0, visited + p)
      case _ =>
        val (up, vup)       = recurse(p.up, visited + p)
        val (down, vdown)   = recurse(p.down, vup)
        val (left, vleft)   = recurse(p.left, vdown)
        val (right, vright) = recurse(p.right, vleft)
        (1 + up + down + left + right, vright)

    val (count, _) = recurse(start, Set.empty)
    count

  def risk2(heights: Seq[Vector[Int]]): Int =
    val basins   = minima(heights).map(p => dfs(heights, p))
    val biggest3 = basins.sorted(Ordering[Int].reverse).take(3)
    biggest3.product

  given Decoder[Vector[Int]] with
    def decode(s: String): Either[String, Vector[Int]] = try Right(s.toCharArray.nn.map(_ - '0').toVector)
    catch case NonFatal(e) => Left(e.getMessage.nn)
