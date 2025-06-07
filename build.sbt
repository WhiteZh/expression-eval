ThisBuild / version := "0.1.1"

ThisBuild / scalaVersion := "3.7.1"

ThisBuild / scalacOptions ++= Seq("-deprecation",
                                  "-encoding", "UTF-8",
                                  "-feature",
                                  "-unchecked")

assembly / mainClass := Some("app.expression_eval.App")

lazy val root = (project in file("."))
    .settings(name := "expression-eval",
              libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test)
