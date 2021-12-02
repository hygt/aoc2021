package aoc

import aoc.Day02.Command

class Test02 extends munit.FunSuite:

  private val sample = decode[Command] {
    """
     |forward 5
     |down 5
     |forward 8
     |up 3
     |down 8
     |forward 2
    """.stripMargin
  }

  private val input = load[Command](2)

  test("part 1 - sample") {
    assertEquals(Day02.process1(sample), 150)
  }

  test("part 1 - result: " + Day02.process1(input)) {}

  test("part 2 - sample") {
    assertEquals(Day02.process2(sample), 900)
  }

  test("part 2 - result: " + Day02.process2(input)) {}
