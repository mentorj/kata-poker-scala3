ThisBuild / scalaVersion     := "3.3.3"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.javaxpert"
ThisBuild / organizationName := "Kata Poker"

lazy val root = (project in file("."))
  .settings(
    name := "poker-kata",
    // https://mvnrepository.com/artifact/org.scalameta/munit
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.3" % Test

    // libraryDependencies += munit % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
