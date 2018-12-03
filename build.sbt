import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.7",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "aoc2018",
    libraryDependencies := deps,
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
  )
