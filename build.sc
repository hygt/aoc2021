import coursier.maven.MavenRepository
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

  def ivyDeps = Agg(ivy"org.typelevel::cats-core:2.7.0")

  object test extends Tests with TestModule.Munit with ScalafmtModule {
    def ivyDeps = Agg(ivy"org.scalameta::munit:1.0.0-M1")
  }

}
