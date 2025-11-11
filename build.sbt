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

ThisBuild / scalaVersion := "3.7.3"

ThisBuild / scalacOptions ++=
  Seq("-Wunused:imports", "-Wunused:locals", "-Wunused:params")

val goodDir = file(".")

lazy val runNode = inputKey[Unit]("Run a node app with arguments.")

lazy val benchAll = inputKey[Unit]("Bench for all platforms.")

lazy val runAlt = inputKey[Unit]("Run an alternate language")

publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

def paddedDay(n: Int): String =
  if (n < 10)
    s"0$n"
  else
    n.toString

lazy val root =
  project.aggregate(core.jvm, core.js, core.native).settings(
    publish / skip := true,
    runAlt := {
      val args = spaceDelimited("<arg>").parsed

      println(args)
      val lang = args.head
      val year = args.tail.head.toInt
      val day = args.tail.tail.head.toInt

      val (cmd, cwd) =
        lang match {
          case "haskell" =>
            (
              Seq[String](
                "stack",
                "run",
                "--cwd",
                goodDir.getAbsolutePath,
                "--",
                year.toString,
                day.toString
              ),
              goodDir / "alt_langs/haskell"
            )
          case "ruby" =>
            (
              Seq[String](
                "ruby",
                (goodDir / s"alt_langs/ruby/src/y$year/Day${paddedDay(day)}.rb")
                  .getAbsolutePath
              ),
              goodDir
            )
          case "elixir" =>
            (
              Seq[String](
                "elixir",
                (goodDir /
                  s"alt_langs/elixir/src/y$year/Day${paddedDay(day)}.exs")
                  .getAbsolutePath
              ),
              goodDir
            )
          case "haxe" =>
            (
              Seq[String](
                "haxe",
                "--class-path",
                (goodDir / s"alt_langs/haxe/src/").getAbsolutePath,
                "--main",
                s"y$year.Day${paddedDay(day)}",
                "--interp"
              ),
              goodDir
            )

        }
      Process(cmd, cwd).run()
    }
  )

lazy val core =
  crossProject(JSPlatform, JVMPlatform, NativePlatform)
    // project
    .in(file("core")).settings(
      name := "adventofcode2024",
      libraryDependencies += "org.typelevel" %%% "cats-core" % "2.13.0",
      libraryDependencies += "org.typelevel" %%% "alleycats-core" % "2.13.0",
      libraryDependencies += "org.typelevel" %%% "cats-parse" % "1.1.0",
      libraryDependencies +=
        "org.typelevel" %%% "cats-collections-core" % "0.9.9",
      libraryDependencies += "org.typelevel" %%% "kittens" % "3.5.0",
      libraryDependencies += "com.github.j-mie6" %%% "parsley" % "4.6.1",
      libraryDependencies += "com.github.j-mie6" %%% "parsley-cats" % "1.5.0",
      libraryDependencies ++= Seq(
        "io.circe" %%% "circe-core",
        "io.circe" %%% "circe-generic",
        "io.circe" %%% "circe-parser"
      ).map(_ % "0.14.14"),
      libraryDependencies ++= Seq(
        "dev.optics" %%% "monocle-core",
        "dev.optics" %%% "monocle-macro"
      ).map(_ % "3.3.0"),
      // use the syntax that I thought was already in scala 3 by now
      scalacOptions ++= Seq(
        "-Xkind-projector:underscores",
        "-experimental"
      ),
      // for testing only, won't be enabled when i finalize the commit
      // libraryDependencies +=
      //  "org.scala-lang.modules" %%% "scala-parallel-collections" % "1.2.0",
      // no spire until they fix it for native 0.5
      // libraryDependencies += "org.typelevel" %%% "spire" % "0.18.0",
      Compile / run / fork := true,
      Compile / run / baseDirectory := goodDir
    ).nativeSettings(
      nativeConfig ~= { c =>
        c.withMode(releaseFull)
      },
      bspEnabled := false
    ).jsSettings(
      bspEnabled := false
    )

lazy val bench =
  crossProject(JSPlatform, JVMPlatform, NativePlatform).in(file("bench"))
    .dependsOn(core)
    // .dependsOn(root)
    .configurePlatform(JVMPlatform)(_.enablePlugins(JmhPlugin)).settings(
      publish / skip := true,
      run / baseDirectory := goodDir,
      Compile / run / mainClass := Some("gay.menkissing.bench.Main"),
      scalacOptions ++= Seq("-experimental")
    ).jvmSettings(
      Compile / run / fork := true
    ).nativeSettings(
      nativeConfig ~= { c =>
        c.withMode(releaseFull)
      }
    ).jsSettings(
      scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.ESModule)
        // .withExperimentalUseWebAssembly(true)
      },
      runNode := {
        val report = (Compile / fullLinkJS).value.data
        val args: Seq[String] = spaceDelimited("<arg>").parsed

        val mainModule = report.publicModules.find(_.moduleID == "main").get

        val linkerOutputDir =
          (Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value

        val path = (linkerOutputDir / mainModule.jsFileName).toPath

        val process =
          Process(
            Seq[String](
              "node",
              "--expose-gc",
              "-",
              s"${path.toString}",
              "--"
            ) ++ args,
            (run / baseDirectory).value
          )
        val is =
          new ByteArrayInputStream(
            s"""
               |const { JSMain } = await import("${path.toString}")
               |
               |JSMain.main()
               |""".stripMargin.getBytes(StandardCharsets.UTF_8)
          )
        process.#<(is).!
      }
    )

lazy val inputHelper =
  project.in(file("inputhelper")).settings(
    publish / skip := true,
    run / baseDirectory := goodDir,
    libraryDependencies +=
      "com.softwaremill.sttp.client4" %% "core" % "4.0.0-M22"
  )

lazy val benchExec =
  project.settings(
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
