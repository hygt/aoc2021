package aoc

import aoc.Decoder.splitTrim

import cats.instances.seq.*
import cats.syntax.foldable.*

import scala.util.control.NonFatal
import scala.annotation.switch

object Day08:

  case class Entry(signals: Vector[String], output: Vector[String])

  private def unique(output: String): Boolean = (output.length: @switch) match
    case 2 => true
    case 3 => true
    case 4 => true
    case 7 => true
    case _ => false

  /** Part 1 */
  def count1(entries: Seq[Entry]): Int =
    entries.map(_.output.count(unique)).sum

  private val patterns: Set[Seq[Char]] =
    Seq("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg").map(_.toSeq).toSet

  private def digit(s: String): Int = (s: @switch) match
    case "abcefg"  => 0
    case "cf"      => 1
    case "acdeg"   => 2
    case "acdfg"   => 3
    case "bcdf"    => 4
    case "abdfg"   => 5
    case "abdefg"  => 6
    case "acf"     => 7
    case "abcdefg" => 8
    case "abcdfg"  => 9
    case _         => throw IllegalArgumentException("invalid segments")

  private val permutations: Seq[Seq[Char]] =
    ('a' to 'g').permutations.toSeq

  private def shift(permutation: Seq[Char])(signal: String): Seq[Char] =
    signal.map { c => permutation(c - 'a') }.sorted

  private def findPermutation(entry: Entry): Option[Seq[Char]] =
    permutations.collectFirstSome { permutation =>
      val shifted = entry.signals
        .map(shift(permutation))
        .toSet
      Option.when(shifted == patterns)(permutation)
    }

  private def solve(entry: Entry): Int =
    findPermutation(entry) match
      case Some(permutation) =>
        entry.output
          .map(o => shift(permutation)(o).mkString)
          .map(digit)
          .foldLeft(0) { case (acc, x) => 10 * acc + x }
      case None => throw new IllegalArgumentException("no valid permutation found")

  /** Part 2 */
  def count2(entries: Seq[Entry]): Int =
    entries.map(solve).sum

  given Decoder[Entry] with
    def decode(s: String): Either[String, Entry] = try
      val tokens            = s.splitTrim("""\s+""")
      val (signals, output) = tokens.splitAt(tokens.indexOf("|"))
      Right(Entry(signals, output.drop(1)))
    catch case NonFatal(e) => Left(e.getMessage.nn)
