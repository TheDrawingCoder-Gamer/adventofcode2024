import scala.scalanative.build.Mode.releaseFull
import scala.scalanative.build.*
import complete.DefaultParsers.*
import org.scalajs.jsenv.{Input, RunConfig}

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.sys.process.Process
import scala.sys.process.*

ThisBuild / organization := "gay.menkissing"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

val goodDir = file(".")

lazy val runNode = inputKey[Unit]("Run a node app with arguments.")

lazy val benchAll = inputKey[Unit]("Bench for all platforms.")

publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

lazy val root = project.aggregate(core.jvm, core.js, core.native)
                       .settings(
                         publish / skip := true,

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
    Compile / run / mainClass := Some("gay.menkissing.bench.Main"),

  )
  .jvmSettings(
      Jmh / sourceDirectory := (Compile / sourceDirectory).value,
      Jmh / classDirectory := (Compile / classDirectory).value,
      Jmh / dependencyClasspath := (Compile / dependencyClasspath).value,
      Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
      Jmh / run := (Jmh / run).dependsOn(Jmh / compile).evaluated,
      Compile / run / fork := true
  )
  .nativeSettings(
    nativeConfig ~= { c =>
      c.withMode(releaseFull)
    }
  )
  .jsSettings(
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



      val process = Process(Seq[String]("node", "--expose-gc", "-", s"${path.toString}", "--") ++ args, (run / baseDirectory).value)
      val is = new ByteArrayInputStream(
        s"""
          |const { JSMain } = await import("${path.toString}")
          |
          |JSMain.main()
          |""".stripMargin.getBytes(StandardCharsets.UTF_8))
      process.#<(is).!
    }
  )

lazy val inputHelper = project.in(file("inputhelper"))
    .settings(
      publish / skip := true,
      run / baseDirectory := goodDir,
      libraryDependencies += "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M22"
    )


lazy val benchExec = project
  .settings(
    benchAll := {
      Def.inputTaskDyn {
        val args = spaceDelimited("<arg>").parsed.mkString(" ")
        Def.sequential(
          (bench.jvm / Compile / run).toTask(" --quiet " ++ args),
          (bench.js / runNode).toTask(" --quiet " ++ args),
          (bench.native / Compile / run).toTask(" --quiet " ++ args)
        )
      }.evaluated
    },
    publish / skip := true
  )
