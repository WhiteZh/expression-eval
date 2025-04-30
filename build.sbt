ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.5"

assembly / mainClass := Some("app.expression_eval.App")

lazy val root = (project in file("."))
    .settings(name := "expression-eval",
              libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test)
