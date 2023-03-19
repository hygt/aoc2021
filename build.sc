import mill._, scalalib._, scalafmt._

object aoc extends ScalaModule with ScalafmtModule {

  def scalaVersion = "3.1.2"
  
  def scalacOptions = Seq(
    "-language:existentials,experimental.macros,higherKinds,implicitConversions,postfixOps",
    "-deprecation",
    "-explain",
    "-explain-types",
    "-feature",
    "-Ykind-projector",
    "-Yexplicit-nulls",
    "-source",
    "future"
  )

  def ivyDeps = Agg(
    ivy"org.scalanlp::breeze:2.0",
    ivy"org.typelevel::cats-core:2.7.0",
    ivy"org.typelevel::cats-parse:0.3.6"
  )

  object test extends Tests with TestModule.Munit with ScalafmtModule {
    def ivyDeps = Agg(ivy"org.scalameta::munit:1.0.0-M1")

    def day(d: Int) = super.testOnly(f"aoc.Test$d%02d")
  }
}
