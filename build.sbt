ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.8"

lazy val root = (project in file("."))
  .settings(
    name := "Whe4Snever",
    libraryDependencies += "org.apache.pekko" %% "pekko-actor-typed" % "1.6.0"
  )
