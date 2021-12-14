package aoc

import aoc.Day14.{Polymer, given}

class Test14 extends munit.FunSuite:

  private val sample: Polymer =
    """
      |NNCB
      |
      |CH -> B
      |HH -> N
      |CB -> H
      |NH -> C
      |HB -> C
      |HC -> B
      |HN -> C
      |NN -> C
      |BH -> H
      |NC -> B
      |NB -> B
      |BN -> B
      |BB -> N
      |BC -> B
      |CC -> N
      |CN -> C
      |""".stripMargin.decoded

  private val input = entire[Polymer](14)

  test("part 1 - sample") {
    assertEquals(sample.run(10), 1588L)
  }

  test("part 1 - result: " + input.run(10)) {}

  test("part 2 - sample") {
    assertEquals(sample.run(40), 2188189693529L)
  }

  test("part 2 - result: " + input.run(40)) {}
