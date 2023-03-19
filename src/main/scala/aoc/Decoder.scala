package aoc

import cats.instances.vector.*
import cats.syntax.traverse.*

trait Decoder[T]:

  private val outer = this

  def decode(s: String): Either[String, T]

  def map[U](f: T => U): Decoder[U] =
    new Decoder[U]:
      def decode(s: String): Either[String, U] =
        outer.decode(s).map(f)

  extension (s: String)
    def decoded: T =
      decode(s) match
        case Right(r)    => r
        case Left(error) => throw new IllegalArgumentException(s"Invalid input: $error")

object Decoder:

  given Decoder[String] with
    def decode(s: String): Either[String, String] = Right(s)

  given Decoder[Int] with
    def decode(s: String): Either[String, Int] = s.toIntOption.toRight(s"$s is not a integer")

  given Decoder[Vector[Int]] with
    def decode(s: String): Either[String, Vector[Int]] =
      s.splitTrim(",")
        .map(_.toIntOption)
        .sequence
        .toRight("failed to parse integer vector")

  extension (s: String)
    /** Null-safe string splitter.
      *
      * @param regex
      *   the separator regex
      * @return
      *   a vector of tokens, trimmed
      */
    def splitTrim(regex: String): Vector[String] =
      val tokens = s.split(regex).nn
      tokens.toVector.collect:
        case s: String if !s.isBlank => s.trim.nn
