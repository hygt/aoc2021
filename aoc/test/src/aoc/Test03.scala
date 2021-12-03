package aoc

import aoc.Day03.Diag

class Test03 extends munit.FunSuite:

  private val sample = decode[Diag] {
    """
     |00100
     |11110
     |10110
     |10111
     |10101
     |01111
     |00111
     |11100
     |10000
     |11001
     |00010
     |01010
    """.stripMargin
  }

  private val input = load[Diag](3)

  test("part 1 - sample") {
    assertEquals(Day03.process1(sample), 198)
  }

  test("part 1 - result: " + Day03.process1(input)) {}

  test("part 2 - sample") {
    assertEquals(Day03.process2(sample), 230)
  }

  test("part 2 - result: " + Day03.process2(input)) {}
