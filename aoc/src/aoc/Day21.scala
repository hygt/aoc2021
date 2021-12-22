package aoc

import cats.instances.long.*
import cats.instances.map.*
import cats.instances.seq.*
import cats.syntax.foldable.*

import scala.annotation.tailrec

object Day21:

  case class Position(one: Int, two: Int):
    def move(d: Int, turn: Boolean): Position =
      if turn then Position((one + d - 1) % 10 + 1, two)
      else Position(one, (two + d - 1)    % 10 + 1)

  case class Points(one: Int, two: Int):
    def inc(position: Position, turn: Boolean): Points =
      if turn then Points(one + position.one, two)
      else Points(one, two + position.two)

  /** Part 1 */
  private class State(position: Position, points: Points, target: Int, step: Int, turn: Boolean):

    private def roll: Int =
      List(0, 1, 2).map(x => (x + step) % 100 + 1).sum

    private def result(losing: Int): Int =
      step * losing

    @tailrec
    private def run: Int =
      if turn && points.two >= target then result(points.one)
      else if points.one >= target then result(points.two)
      else
        val pos = position.move(roll, turn)
        val pts = points.inc(pos, turn)
        State(pos, pts, target, step + 3, !turn).run

  object State:
    def start(one: Int, two: Int, target: Int): Int =
      State(Position(one, two), Points(0, 0), target, 0, true).run

  /** Part 2.
    *
    * The map holds the number of paths that arrived at (position, score) after the previous step. We accumulate the
    * number of (losing, winning) outcomes at every step. The recursion ends shortly since the puzzle was designed not
    * to overflow unsigned longs. The total number of winning universes is the sum of winning outcomes multiplied by the
    * previous player's losing ones. Thus we need to be careful not to count the last step, when player 1 wins.
    */
  private class Dirac(prev: Map[(Int, Int), Long], target: Int, acc: Seq[(Long, Long)]):

    private val rolls: Seq[(Int, Int)] =
      val all = for
        i <- 1 to 3
        j <- 1 to 3
        k <- 1 to 3
      yield i + j + k
      all.groupBy(identity).view.mapValues(_.size).toSeq

    @tailrec
    private def run: Seq[(Long, Long)] =
      val current = prev.toSeq.foldMap { case ((pos, score), paths) =>
        rolls.foldMap { case (roll, count) =>
          val npos   = (pos + roll - 1) % 10 + 1
          val nscore = score + npos
          Map((npos, nscore) -> count * paths)
        }
      }
      val (under, over) = current.partition { case ((_, score), _) => score < target }
      val losing        = under.values.sum
      val winning       = over.values.sum
      if under.isEmpty then (losing, winning) +: acc
      else Dirac(under, target, (losing, winning) +: acc).run

  object Dirac:
    def start(pos1: Int, pos2: Int, target: Int): Long =
      val p1 = Dirac(Map((pos1, 0) -> 1L), target, Seq.empty).run
      val p2 = Dirac(Map((pos2, 0) -> 1L), target, Seq.empty).run
      // sum of p1 wins * p2 losses; game stops with player 1, discard last p2 outcomes
      val s1 = p1.map(_._2).zip(p2.drop(1).map(_._1)).map(_ * _).sum
      // sum of p1 losses * p2 wins; game stops with player 2
      val s2 = p1.map(_._1).zip(p2.map(_._2)).map(_ * _).sum

      s1 max s2
