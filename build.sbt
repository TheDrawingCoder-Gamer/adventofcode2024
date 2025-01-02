import scala.scalanative.build.Mode.releaseFull
import scala.scalanative.build._

ThisBuild / organization := "gay.menkissing"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

val goodDir = file(".")

publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

lazy val root = project.aggregate(core.jvm, core.js, core.native)

lazy val core = crossProject(JSPlatform, JVMPlatform, NativePlatform)
    //project
  .in(file("core"))
  .settings(
    name := "adventofcode2024",
    libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0",
    libraryDependencies += "org.typelevel" %%% "cats-parse" % "1.1.0",
    libraryDependencies += "org.typelevel" %%% "cats-collections-core" % "0.9.9",
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
  .dependsOn(core.jvm)
  //.dependsOn(root)
  .enablePlugins(JmhPlugin)
  .settings(

      Jmh / sourceDirectory := (Compile / sourceDirectory).value,
      Jmh / classDirectory := (Compile / classDirectory).value,
      Jmh / dependencyClasspath := (Compile / dependencyClasspath).value,
      Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
      Jmh / run := (Jmh / run).dependsOn(Jmh / compile).evaluated,
      run / baseDirectory := goodDir,
      publishLocal := nop
  )
