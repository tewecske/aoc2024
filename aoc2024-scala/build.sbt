val scala3Version = "3.5.2"

scalacOptions ++= Seq(
  "-no-indent",
)

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    // https://mvnrepository.com/artifact/com.lihaoyi/os-lib
    libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.11.3",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    // https://mvnrepository.com/artifact/org.scalatest/scalatest
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test,
  )
