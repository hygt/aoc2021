package aoc

import aoc.Day17.{Target, given}

class Test17 extends munit.FunSuite:

  private val sample: Target = "target area: x=20..30, y=-10..-5".decoded

  private val input = entire[Target](17)

  test("part 1 - sample") {
    assertEquals(sample.find(true), 45)
  }

  test("part 1 - result: " + input.find(true)) {}

  test("part 2 - sample") {
    assertEquals(sample.find(false), 112)
  }

  test("part 2 - result: " + input.find(false)) {}
