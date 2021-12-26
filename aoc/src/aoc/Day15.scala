package aoc

import aoc.Decoder.splitTrim

import scala.collection.mutable
import scala.util.control.NonFatal

object Day15:

  case class Grid(grid: Seq[Seq[Int]]):
    private val size = grid.size

    case class Point(x: Int, y: Int):
      def adjacent: Seq[Point] =
        Seq(Point(x, y + 1), Point(x + 1, y), Point(x, y - 1), Point(x - 1, y)).filter { p =>
          0 <= p.x && p.x < size && 0 <= p.y && p.y < size
        }

    private val ordering = Ordering.by[(Point, Int), Int]((_, score) => score).reverse

    def dijkstra: Int =
      val queue = mutable.PriorityQueue.empty(ordering)
      val risk  = mutable.Map[Point, Int](Point(0, 0) -> 0)
      val dest  = Point(size - 1, size - 1)
      queue.enqueue(Point(0, 0) -> 0)
      var result = -1
      while queue.nonEmpty do
        val (current, distance) = queue.dequeue()
        if current == dest then
          queue.clear()
          result = distance
        else
          current.adjacent.foreach { neighbor =>
            val weight = grid(neighbor.y)(neighbor.x)
            val score  = risk.get(current).map(_ + weight).getOrElse(Int.MaxValue)
            if score < risk.getOrElse(neighbor, Int.MaxValue) then
              risk.update(neighbor, score)
              queue.enqueue(neighbor -> score)
          }
      result

    def times5: Grid =
      val arr = Array.fill(5 * size)(Array.fill(5 * size)(0))
      for
        i <- 0 until 5
        j <- 0 until 5
        x <- 0 until size
        y <- 0 until size
        v = grid(y)(x) + i + j
        w = if v > 9 then v - 9 else v
      do arr(j * size + y)(i * size + x) = w
      Grid(arr.map(_.toSeq).toSeq)

  given Decoder[Grid] with
    def decode(s: String): Either[String, Grid] = try
      val grid = s.splitTrim("""\n""").map(_.toCharArray.nn.map(_.asDigit).toVector)
      assert(grid.size == grid.head.size)
      Right(Grid(grid))
    catch case NonFatal(e) => Left(e.getMessage.nn)
