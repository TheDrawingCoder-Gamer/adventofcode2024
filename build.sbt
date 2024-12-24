ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

lazy val root = (project in file("."))
  .settings(
    name := "adventofcode2024",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0",
    libraryDependencies += "co.fs2" %% "fs2-core" % "3.11.0",
    libraryDependencies += "org.typelevel" %% "cats-effect" % "3.5.7",
    libraryDependencies +=
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
    Compile / run / fork := true
  )

lazy val bench = project.in(file("bench"))
  .dependsOn(root)
  .enablePlugins(JmhPlugin)
  .settings(
      Jmh / sourceDirectory := (Compile / sourceDirectory).value,
      Jmh / classDirectory := (Compile / classDirectory).value,
      Jmh / dependencyClasspath := (Compile / dependencyClasspath).value,
      Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
      Jmh / run := (Jmh / run).dependsOn(Jmh / compile).evaluated
  )