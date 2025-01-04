import scala.scalanative.build.Mode.releaseFull
import scala.scalanative.build.*
import complete.DefaultParsers.*
import org.scalajs.jsenv.{Input, RunConfig}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.sys.process.Process
import scala.sys.process.*

ThisBuild / organization := "gay.menkissing"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

val goodDir = file(".")

lazy val runNode = inputKey[Unit]("Run a node app with arguments.")

publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

lazy val root = project.aggregate(core.jvm, core.js, core.native)
                       .settings(
                         publish / skip := true
                       )

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

  .nativeSettings(
    nativeConfig ~= { c =>
      c.withMode(releaseFull)
    }
  )



lazy val bench = crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("bench"))
  .dependsOn(core)
  //.dependsOn(root)
  .configurePlatform(JVMPlatform)(_.enablePlugins(JmhPlugin))
  .settings(
    publish / skip := true,
    run / baseDirectory := goodDir,
  )
  .jvmSettings(
      Jmh / sourceDirectory := (Compile / sourceDirectory).value,
      Jmh / classDirectory := (Compile / classDirectory).value,
      Jmh / dependencyClasspath := (Compile / dependencyClasspath).value,
      Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
      Jmh / run := (Jmh / run).dependsOn(Jmh / compile).evaluated,
  )
  .nativeSettings(
    nativeConfig ~= { c =>
      c.withMode(releaseFull)
    }
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= {
      _.withModuleKind(ModuleKind.ESModule)
       // .withExperimentalUseWebAssembly(true)
    },
    runNode := {
      val report = (Compile / fullLinkJS).value.data
      val args: Seq[String] = spaceDelimited("<arg>").parsed

      val mainModule = report.publicModules.find(_.moduleID == "main").get


      val linkerOutputDir = (Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value

      val path = (linkerOutputDir / mainModule.jsFileName).toPath



      val process = Process(Seq[String]("node", "--expose-gc", path.toString, "--") ++ args, (run / baseDirectory).value)

      process.!<
    }
  )


lazy val inputHelper = project.in(file("inputhelper"))
    .settings(
      publish / skip := true,
      run / baseDirectory := goodDir,
      libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M22"
    )


