import sbt._

object Dependencies {
  lazy val tests = Seq(
    "org.scalatest" %% "scalatest" % "3.0.5",
    "org.scalacheck" %% "scalacheck" % "1.14.0"
  ).map(_ % Test)

  lazy val cats = Seq(
    "org.typelevel" %% "cats-core" % "1.4.0"
  )

  lazy val deps = cats ++ tests
}
