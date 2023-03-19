package aoc

class Test10 extends munit.FunSuite:

  private val sample = decode[String] {
    """
      |[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]
      |""".stripMargin
  }

  private val input = load[String](10)

  test("part 1 - sample") {
    assertEquals(Day10.solve1(sample), 26397L)
  }

  test("part 1 - result: " + Day10.solve1(input)) {}

  test("part 2 - sample") {
    assertEquals(Day10.solve2(sample), 288957L)
  }

  test("part 2 - result: " + Day10.solve2(input)) {}
