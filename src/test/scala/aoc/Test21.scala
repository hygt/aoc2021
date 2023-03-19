package aoc

import aoc.Day21.*

class Test21 extends munit.FunSuite:

  test("part 1 - sample") {
    assertEquals(State.start(4, 8, 1000), 739785)
  }

  test("part 1 - result: " + State.start(9, 6, 1000)) {}

  test("part 2 - sample") {
    assertEquals(Dirac.start(4, 8, 21), 444356092776315L)
  }

  test("part 2 - result: " + Dirac.start(9, 6, 21)) {}
