val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dbg",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"
  )
