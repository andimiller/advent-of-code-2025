ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code-2025",
    libraryDependencies ++= List(
      "co.fs2" %% "fs2-io" % "3.12.2",
      "org.typelevel" %% "cats-effect-std" % "3.6.3",
      "org.typelevel" %% "cats-parse" % "1.1.0",
      "org.typelevel" %% "munit-cats-effect" % "2.1.0" % Test
    )
  )
