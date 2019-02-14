name := "scala-exercises"

version := "1.0"

scalaVersion := "2.12.8"


libraryDependencies += "junit" % "junit" % "4.10" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.4" % "test"
libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.12.8"

// cats
scalacOptions += "-Ypartial-unification"
libraryDependencies += "org.typelevel" %% "cats-core" % "1.5.0"