package aoc

import aoc.Decoder.splitTrim
import cats.implicits.*

import scala.util.control.NonFatal

object Day04:

  private type Board = Vector[Vector[Int]]

  private type Draw = Seq[Int]

  private type Index = Map[Set[Int], Int]

  /** Index of the last number drawn -> board ID */
  private type Winner = (Int, Int)

  case class Bingo(input: Draw, boards: Seq[Board]):

    /** Builds an index of winning bingo combinations and their corresponding board number ID.
      */
    private val index: Index =
      boards.zipWithIndex.foldMap { (board, id) =>
        val rows = board.map(row => row.toSet -> id).toMap
        val cols = board.transpose.map(col => col.toSet -> id).toMap
        rows ++ cols
      }

    private def score(i: Int, board: Int): Int =
      val draw = input.take(i + 1)
      input(i) * boards(board).flatten.diff(draw).sum

    private def winning(i: Int): Seq[Winner] =
      val draw = input.take(i + 1)
      index.keys
        .filter(_.subsetOf(draw.toSet))
        .flatMap(index.get)
        .map(id => i -> id)
        .toSeq

    def first: Int =
      val winner = Seq
        .range(0, input.size)
        .map(winning)
        .collectFirst { case Seq(first) =>
          first
        }
      score.tupled(winner.get)

    def last: Int =
      val w = Seq.range(0, input.size).scanLeft(Seq.empty[Winner]) { (acc, number) =>
        val prev  = acc.map { (_, board) => board }
        val `new` = winning(number).filterNot { (_, board) => prev.contains(board) }
        acc ++ `new`
      }
      w.last match
        case _ :+ last => score.tupled(last)
        case _         => throw new IllegalStateException

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
