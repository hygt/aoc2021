package aoc

import cats.instances.option.*
import cats.syntax.apply.*

object Day01:
  def increasing(depths: Seq[Int], n: Int): Int =
    depths
      .sliding(n + 1)
      .flatMap { w => w.lastOption.map2(w.headOption)(Ordering[Int].gt) }
      .count(identity)
