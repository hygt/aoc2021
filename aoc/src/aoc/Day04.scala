package aoc

import aoc.Decoder.splitTrim
import cats.implicits.*

import scala.util.control.NonFatal

object Day04:

  private type Board = Vector[Vector[Int]]

  private type Draw = Seq[Int]

  private type Index = Map[Set[Int], Int]

  private type Winner = (Draw, Int)

  case class Bingo(input: Draw, boards: Seq[Board]) {

    /** Builds the initial index of winning bingo combinations and their corresponding board number ID.
      */
    private val initial: Index =
      boards.zipWithIndex.foldMap { case (board, id) =>
        val rows = board.map(row => row.toSet -> id).toMap
        val cols = board.transpose.map(col => col.toSet -> id).toMap
        rows ++ cols
      }

    private def draws: Seq[Draw] =
      Seq.range(1, input.size + 1).map(input.take)

    private def score(draw: Draw, id: Int): Int =
      draw.last * boards(id).flatten.diff(draw).sum

    private def winning(index: Index)(draw: Draw): Seq[Winner] =
      index.keys
        .filter(_.subsetOf(draw.toSet))
        .flatMap(index.get)
        .map(id => draw -> id)
        .toSeq

    def first: Int =
      val winner = draws.map(winning(initial)).collectFirst { case Seq(first) =>
        first
      }
      score.tupled(winner.get)

    private def remove(index: Index, winners: Seq[Winner]): Index =
      val ids = winners.map((_, id) => id)
      index.filterNot((_, id) => ids.contains(id))

    def last: Int =
      val (_, w, _) = input.foldLeft((Seq.empty[Int], Seq.empty[Winner], initial)) { (acc, number) =>
        val (prev, winners, index) = acc
        val draw                   = prev :+ number
        winning(index)(draw) match
          case Nil     => (draw, winners, index)
          case current => (draw, current, remove(index, current))
      }
      score.tupled(w.last)

  }

  given Decoder[Bingo] with
    def decode(s: String): Either[String, Bingo] = try
      s.splitTrim("""\n\n""") match
        case head +: tail =>
          val input = head.splitTrim(",").map(_.toInt)
          val boards = tail.map { board =>
            board.splitTrim("""\n""").map { row =>
              row.splitTrim("""\s+""").map(_.toInt)
            }
          }

          Right(Bingo(input, boards))
        case _ => Left("failed to parse bingo")
    catch case NonFatal(e) => Left(e.getMessage.nn)
