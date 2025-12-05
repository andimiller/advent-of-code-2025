import scala.scalanative.build._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.4"

lazy val adventOfCode = crossProject(JSPlatform, JVMPlatform, NativePlatform)
  .withoutSuffixFor(JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("aoc"))
  .settings(
    name                            := "advent-of-code-2025",
    libraryDependencies ++= List(
      "co.fs2"        %%% "fs2-io"            % "3.12.2",
      "org.typelevel" %%% "cats-effect-std"   % "3.6.3",
      "org.typelevel" %%% "cats-parse"        % "1.0.0",
      "com.lihaoyi"   %%% "sourcecode"        % "0.3.1",
      "org.typelevel" %%% "spire" % "0.18.0",
      "org.typelevel" %%% "munit-cats-effect" % "2.1.0" % Test
    ),
    scalacOptions += "-Xkind-projector",
    nativeConfig ~= {
      _.withLTO(LTO.thin)
        .withMode(Mode.releaseFast)
        .withGC(GC.commix)
    },
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }
  )

lazy val root = project
  .in(file("."))
  .aggregate(adventOfCode.js, adventOfCode.jvm, adventOfCode.native)
  .settings(
    publish / skip := true,
    Compile / sources := Nil,
    Test / sources := Nil
  )
