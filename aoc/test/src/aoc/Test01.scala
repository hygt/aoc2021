package aoc

class Test01 extends munit.FunSuite:

  private val sample = decode[Int] { """
    |199
    |200
    |208
    |210
    |200
    |207
    |240
    |269
    |260
    |263
    """.stripMargin
  }

  private val input = load[Int](1)

  test("part 1 - sample") {
    assertEquals(Day01.increasing(sample, 1), 7)
  }

  test("part 1 - result: " + Day01.increasing(input, 1)) {}

  test("part 2 - sample") {
    assertEquals(Day01.increasing(sample, 3), 5)
  }

  test("part 2 - result: " + Day01.increasing(input, 3)) {}