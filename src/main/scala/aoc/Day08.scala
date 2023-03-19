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
    case "cf"      => 1
    case "acf"     => 7
    case "bcdf"    => 4
    case "abdfg"   => 5
    case "acdeg"   => 2
    case "acdfg"   => 3
    case "abcdfg"  => 9
    case "abcefg"  => 0
    case "abdefg"  => 6
    case "abcdefg" => 8
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

  private def bruteforce(entry: Entry): Int =
    findPermutation(entry) match
      case Some(permutation) =>
        entry.output
          .map(o => shift(permutation)(o).mkString)
          .map(digit)
          .foldLeft(0) { case (acc, x) => 10 * acc + x }
      case None => throw new IllegalArgumentException("no valid permutation found")

  private def find(signals: Set[String], n: Int)(size: Int): Option[Set[Set[Char]]] =
    val matches = signals.filter(_.length == size)
    if matches.size == n then Some(matches.map(_.toSet))
    else None

  private def solve(entry: Entry): Int =
    val signals = entry.signals.toSet

    def unique(size: Int): Option[Set[Char]] =
      find(signals, 1)(size).flatMap(_.headOption)

    def three = find(signals, 3)

    val mapping = for
      one          <- unique(2)
      seven        <- unique(3)
      four         <- unique(4)
      eight        <- unique(7)
      twoThreeFive <- three(5)
      zeroSixNine  <- three(6)
      (zeroNine, sixSet) = zeroSixNine.partition(seven.subsetOf)
      (nineSet, zeroSet) = zeroNine.partition(four.subsetOf)
      six  <- sixSet.headOption
      nine <- nineSet.headOption
      zero <- zeroSet.headOption
      (threeSet, twoFive) = twoThreeFive.partition(seven.subsetOf)
      (fiveSet, twoSet)   = twoFive.partition(_.subsetOf(nine))
      three <- threeSet.headOption
      five  <- fiveSet.headOption
      two   <- twoSet.headOption
    yield Map(
      zero  -> 0,
      one   -> 1,
      two   -> 2,
      three -> 3,
      four  -> 4,
      five  -> 5,
      six   -> 6,
      seven -> 7,
      eight -> 8,
      nine  -> 9
    )
    mapping match
      case Some(digits) =>
        assert(digits.size == 10)
        entry.output.map(_.toSet).flatMap(digits.get).foldLeft(0) { case (acc, x) => 10 * acc + x }
      case None => -1

  /** Part 2 */
  def count2(entries: Seq[Entry]): Int =
    entries.map(solve).sum

  given Decoder[Entry] with
    def decode(s: String): Either[String, Entry] = try
      val tokens            = s.splitTrim("""\s+""")
      val (signals, output) = tokens.splitAt(tokens.indexOf("|"))
      Right(Entry(signals, output.drop(1)))
    catch case NonFatal(e) => Left(e.getMessage.nn)
