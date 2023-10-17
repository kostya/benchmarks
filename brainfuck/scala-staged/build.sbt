val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "bf",
    version := "0.1.0",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-staging" % scala3Version
    )
  )
