name := "mandelbrot explorer"
version := "0.8"

organization := "nemzsom"

scalaVersion := "2.11.0"

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8", // yes, this is 2 args
  "-feature",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-numeric-widen",
  "-Xfuture",
  "-Ywarn-unused-import",
  "-target:jvm-1.7"
)

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.1.1",
  "ch.qos.logback" % "logback-core" % "1.1.1",
  "org.slf4j" % "log4j-over-slf4j" % "1.7.6",
  "com.typesafe.scala-logging" % "scala-logging-slf4j_2.11" % "2.1.2",
  "junit" % "junit" % "4.10" % "test",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  //"org.scala-lang" % "scala-swing" % "2.11.0",
  "org.scala-lang.modules" % "scala-swing_2.11" % "2.0.0-M2",
  "io.reactivex" %% "rxscala" % "0.24.0"
)
