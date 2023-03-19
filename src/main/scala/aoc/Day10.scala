package aoc

import aoc.Day10.Status.*

object Day10:

  private val points1 = Map(
    ')' -> 3L,
    ']' -> 57L,
    '}' -> 1197L,
    '>' -> 25137L
  )

  private val points2 = Map(
    ')' -> 1L,
    ']' -> 2L,
    '}' -> 3L,
    '>' -> 4L
  )

  /** @note Overflows Int, hence everything returns Long for convenience. */
  private def points2(cs: Seq[Char]): Long = cs.foldLeft(0L) { (acc, c) =>
    acc * 5L + points2(c)
  }

  private val matching = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  private def opening(c: Char): Boolean =
    matching.contains(c)

  private def parse(s: String): Status =
    s.foldLeft((List.empty[Char], Option.empty[Char])) { case ((stack, status), c) =>
      if status.isDefined then (stack, status)
      else if opening(c) then (stack :+ c, None)
      else if c != matching(stack.last) then (stack, Some(c))
      else (stack.dropRight(1), None)
    } match
      case (_, Some(c))                    => Corrupt(c)
      case (stack, None) if stack.nonEmpty => Incomplete(stack.reverse.map(matching))
      case _                               => throw new IllegalStateException("no input should be valid")

  enum Status(val score: Long):
    case Corrupt(c: Char)               extends Status(points1(c))
    case Incomplete(missing: Seq[Char]) extends Status(points2(missing))

  def solve1(lines: Seq[String]): Long =
    lines.map(parse).collect { case c: Corrupt => c.score }.sum

  def solve2(lines: Seq[String]): Long =
    val sorted = lines.map(parse).collect { case i: Incomplete => i.score }.sorted
    sorted(sorted.size / 2)
