package aoc

import aoc.Day05.Segment

class Test05 extends munit.FunSuite:

  private val sample = decode[Segment] {
    """
    |0,9 -> 5,9
    |8,0 -> 0,8
    |9,4 -> 3,4
    |2,2 -> 2,1
    |7,0 -> 7,4
    |6,4 -> 2,0
    |0,9 -> 2,9
    |3,4 -> 1,4
    |0,0 -> 8,8
    |5,5 -> 8,2
    """.stripMargin
  }

  private val input = load[Segment](5)

  test("part 1 - sample") {
    assertEquals(Day05.process(sample, false), 5)
  }

  test("part 1 - result: " + Day05.process(input, false)) {}

  test("part 2 - sample") {
    assertEquals(Day05.process(sample, true), 12)
  }

  test("part 2 - result: " + Day05.process(input, true)) {}
