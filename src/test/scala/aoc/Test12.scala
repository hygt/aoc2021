package aoc

import aoc.Day12.{Graph, given}

class Test12 extends munit.FunSuite:

  private val sample1: Graph =
    """
      |start-A
      |start-b
      |A-c
      |A-b
      |b-d
      |A-end
      |b-end
      |""".stripMargin.decoded

  private val sample2: Graph =
    """
      |dc-end
      |HN-start
      |start-kj
      |dc-start
      |dc-HN
      |LN-dc
      |HN-end
      |kj-sa
      |kj-HN
      |kj-dc
      |""".stripMargin.decoded

  private val input = entire[Graph](12)

  test("part 1 - sample") {
    assertEquals(Day12.process1(sample1), 10)
    assertEquals(Day12.process1(sample2), 19)
  }

  test("part 1 - result: " + Day12.process1(input)) {}

  test("part 2 - sample") {
    assertEquals(Day12.process2(sample1), 36)
    assertEquals(Day12.process2(sample2), 103)
  }

  test("part 2 - result: " + Day12.process2(input)) {}
