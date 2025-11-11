package gay.menkissing.bench.spawn

import gay.menkissing.bench.{BenchmarkRunOpts, Main}

import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

object Spawn:
  def run(name: String, runOpts: BenchmarkRunOpts): Option[Vector[Double]] =
    // Native doesn't actually need forking so this should be fine
    val future = Future(Main.benchmarkMap(name).run(runOpts.verbosity))
    val duration = runOpts.timeout.getOrElse(Duration.Inf)
    try Some(Await.result(future, duration))
    catch case e: TimeoutException => None
