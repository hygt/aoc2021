package aoc

import aoc.Day13.{Paper, given}

class Test13 extends munit.FunSuite:

  private val sample: Paper =
    """
      |6,10
      |0,14
      |9,10
      |0,3
      |10,4
      |4,11
      |6,0
      |6,12
      |4,1
      |0,13
      |10,12
      |3,4
      |3,0
      |8,4
      |1,10
      |2,14
      |8,10
      |9,0
      |
      |fold along y=7
      |fold along x=5
      |""".stripMargin.decoded

  private val input = entire[Paper](13)

  test("part 1 - sample") {
    assertEquals(sample.run1, 17)
  }

  test("part 1 - result: " + input.run1) {}

  test("part 2 - sample") {
    println(sample.run2)
  }

  test("part 2 - result") {
    println(input.run2)
  }
