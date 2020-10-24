import sbt._

object Dependencies {
  lazy val catsCore = "org.typelevel" %% "cats-core" % "2.1.1"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "2.1.1"
  lazy val catsFree = "org.typelevel" %% "cats-free" % "2.1.1"

  lazy val fs2Core = "co.fs2" %% "fs2-core" % "2.3.0" // For cats 2 and cats-effect 2
  lazy val fs2IO = "co.fs2" %% "fs2-io" % "2.3.0"

  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.1.2" % Test
  lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.3" % Test
  lazy val scalaTestScalaCheckIntegration = "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test

  lazy val refined = "eu.timepit" %% "refined" % "0.9.15"
  lazy val refinedCats = "eu.timepit" %% "refined-cats" % "0.9.15"

  lazy val atto = "org.tpolecat" %% "atto-core" % "0.8.0"
  lazy val attoRefined = "org.tpolecat" %% "atto-refined" % "0.8.0"

  lazy val gll = "com.codecommit" %% "gll-combinators" % "2.3"
}
