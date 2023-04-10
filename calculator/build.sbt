ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.11.12"

lazy val root = (project in file("."))
  .settings(
    name := "calculator"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)
