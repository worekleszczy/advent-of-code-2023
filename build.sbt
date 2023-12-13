import Dependencies.*

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "example"
val circeVersion = "0.14.1"

lazy val root = (project in file("."))
  .settings(
    name := "aoc-2023",
    libraryDependencies ++= List(
      munit            % Test,
      "org.typelevel" %% "cats-parse"    % "0.3.9",
      "com.beachape"  %% "enumeratum"    % "1.7.3",
      "io.circe"      %% "circe-core"    % circeVersion,
      "io.circe"      %% "circe-generic" % circeVersion,
      "io.circe"      %% "circe-parser"  % circeVersion
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
