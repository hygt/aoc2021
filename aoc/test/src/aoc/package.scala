package aoc

import scala.io.Source

def load[T: Decoder](day: Int): Seq[T] =
  val lines = Source
    .fromResource(f"$day%02d.txt", getClass.getClassLoader.nn)
    .getLines
  decode[T](lines)

def decode[T: Decoder](sample: String): Seq[T] =
  decode[T](sample.linesIterator)

private def decode[T: Decoder](lines: Iterator[String]): Seq[T] =
  lines.filterNot(_.isBlank).map(_.trim.nn.decoded).toSeq

def entire[T: Decoder](day: Int): T =
  Source
    .fromResource(f"$day%02d.txt", getClass.getClassLoader.nn)
    .mkString
    .decoded
