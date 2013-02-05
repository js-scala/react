name := "react"

organization := "js-scala"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
    "EPFL" %% "js-scala" % "0.3-SNAPSHOT",
    "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

parallelExecution in Test := false
