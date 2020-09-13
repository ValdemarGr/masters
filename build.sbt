import Dependencies._
import Settings._

lazy val root = (project in file("root"))
  .settings(commonSettings: _*)
  .settings(
    fork in run := true,
    scalaVersion in ThisBuild := "2.12.10",
    version      in ThisBuild := "0.0.1",
    name := "via",
    libraryDependencies ++= Seq(
      catsCore,
      catsEffect,
      catsFree,
      fs2Core,
      fs2IO,
      http4sCore,
      http4sDsl,
      http4sClient,
      scalaTest,
      googleCreds,
      gcCore,
      gcsStorage,
      circeCore,
      circeGeneric,
      circeParser
    )
  )
