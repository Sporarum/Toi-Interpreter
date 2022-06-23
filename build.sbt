val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "toi_interpreter",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq( 
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "org.scalameta" %% "munit" % "0.7.29" % Test
    )
  )
