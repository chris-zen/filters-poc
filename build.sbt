name := "predicates"

version := "1.0"

scalaVersion := "2.10.6"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

logBuffered in Test := false
