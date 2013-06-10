name := "react"

organization := "js-scala"

version := "0.2-SNAPSHOT"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.1"

libraryDependencies ++= Seq(
    "EPFL" %% "js-scala" % "0.4-SNAPSHOT",
    "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

parallelExecution in Test := false
