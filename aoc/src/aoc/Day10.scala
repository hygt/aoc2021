package aoc

import aoc.Day10.Status.*
import cats.data.Validated.Invalid
import cats.instances.seq.*
import cats.syntax.foldable.*

import scala.collection.mutable

object Day10:

  enum Status(val score: Long):
    case Corrupt(c: Char) extends Status(points1(c))
    case Incomplete(missing: Seq[Char]) extends Status(points2(missing))

  private def points1(c: Char): Long = c match
    case ')' => 3L
    case ']' => 57L
    case '}' => 1197L
    case '>' => 25137L
    case _   => throw new IllegalArgumentException(s"unrecognized bracket $c")

  private def points2(c: Char): Long = c match
    case ')' => 1L
    case ']' => 2L
    case '}' => 3L
    case '>' => 4L
    case _   => throw new IllegalArgumentException(s"unrecognized bracket $c")

  /** @note Overflows Int, hence everything returns Long for convenience. */
  private def points2(cs: Seq[Char]): Long = cs.foldLeft(0L) { (acc, c) =>
    acc * 5L + points2(c)
  }

  private def opening(c: Char): Boolean = c match
    case '(' => true
    case '[' => true
    case '{' => true
    case '<' => true
    case _   => false

  private def matching(c: Char): Char = c match
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
    case _   => throw new IllegalArgumentException(s"unrecognized bracket $c")

  private def parse(s: String): Status =
    val stack = mutable.Stack.empty[Char]
    val chars = s.toCharArray.nn.toSeq
    chars.collectFirstSome { c =>
      if opening(c) then
        stack.push(c)
        None
      else if c != matching(stack.pop()) then Some(Corrupt(c))
      else None
    } match {
      case Some(corrupt: Corrupt) => corrupt
      case None if stack.nonEmpty => Incomplete(stack.toSeq.map(matching))
      case _                      => throw new IllegalStateException("no input should be valid")
    }

  def solve1(lines: Seq[String]): Long =
    lines.map(parse).collect { case c: Corrupt => c.score }.sum

  def solve2(lines: Seq[String]): Long =
    val sorted = lines.map(parse).collect { case i: Incomplete => i.score }.sorted
    sorted(sorted.size / 2)
