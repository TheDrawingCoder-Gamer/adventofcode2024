import scala.scalanative.build.Mode.releaseFull
import scala.scalanative.build._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

val goodDir = file(".")


lazy val root = crossProject(JSPlatform, JVMPlatform)
   .crossType(CrossType.Pure)
  .in(file("."))
  .settings(
    name := "adventofcode2024",
    libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0",
    Compile / run / fork := true,
    Compile / run / baseDirectory := goodDir,
  )
  /*
  .nativeSettings(
    nativeConfig ~= { c =>
      c.withMode(releaseFull).withOptimize(true).withGC(GC.none)
    }
  )

   */

lazy val bench = project.in(file("bench"))
  .dependsOn(root.jvm)
  //.dependsOn(root)
  .enablePlugins(JmhPlugin)
  .settings(
      Jmh / sourceDirectory := (Compile / sourceDirectory).value,
      Jmh / classDirectory := (Compile / classDirectory).value,
      Jmh / dependencyClasspath := (Compile / dependencyClasspath).value,
      Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
      Jmh / run := (Jmh / run).dependsOn(Jmh / compile).evaluated,
      run / baseDirectory := goodDir
  )
