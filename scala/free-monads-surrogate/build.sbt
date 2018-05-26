name := "free-monads-surrogate"

version := "1.0"

scalaVersion := "2.11.8"


lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.6"

lazy val logback = "ch.qos.logback"  %  "logback-classic"    % "1.1.7"

libraryDependencies ++= Seq(
  logback,
  scalatest
)
