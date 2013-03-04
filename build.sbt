name := "react"

organization := "js-scala"

version := "0.1-SNAPSHOT"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
    "EPFL" %% "js-scala" % "0.3-SNAPSHOT",
    "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

parallelExecution in Test := false
