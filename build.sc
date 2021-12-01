import mill._, scalalib._, scalafmt._

object aoc extends ScalaModule with ScalafmtModule {

  def scalaVersion = "3.1.0"
  
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

  def moduleDeps = Seq(leopards)

  object test extends Tests with TestModule.Munit {
    def ivyDeps = Agg(ivy"org.scalameta::munit:1.0.0-M1")
  }
}

// no published artifact yet, let's build it from a git submodule
object leopards extends SbtModule {

  def millSourcePath = build.millSourcePath / "spotted-leopards" / "core" / "shared"

  def scalaVersion = "3.1.0"

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

  object test extends Tests with TestModule.Munit {
    def ivyDeps = Agg(ivy"org.scalameta::munit:1.0.0-M1")
  }
}
