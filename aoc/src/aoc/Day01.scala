package aoc

import leopards.{*, given}

object Day01:
  def increasing(depths: Seq[Int], n: Int): Int =
    depths
      .sliding(n + 1)
      .flatMap { w => w.lastOption.map2(w.headOption)(Ordering[Int].gt) }
      .count(identity)
