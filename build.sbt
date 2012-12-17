name := "react"

organization := "js-scala"

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := "2.10.0-M7"

libraryDependencies ++= Seq(
    "EPFL" %% "js-scala" % "0.2-SNAPSHOT",
    "org.scalatest" % "scalatest_2.10.0-M7" % "1.9-2.10.0-M7-B1" % "test"
)

scalacOptions ++= Seq("-deprecation", "-unchecked", "-Xexperimental", "-Yvirtualize")

parallelExecution in Test := false
