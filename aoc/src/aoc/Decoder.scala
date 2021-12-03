package aoc

trait Decoder[T]:
  def decode(s: String): T

  extension (s: String) def decoded: T = decode(s)

object Decoder:

  given Decoder[String] with
    def decode(s: String): String = s

  given Decoder[Int] with
    def decode(s: String): Int = s.toInt
