package gay.menkissing.advent
package y2022

import gay.menkissing.common.*
import cats.*
import cats.syntax.all.*
import monocle.syntax.all.*
import monocle.macros.*
// import scala.collection.parallel.CollectionConverters.*

import monocle.*

object Day19 extends Problem:
  type Input = List[Blueprint]
  type Output = Int

  final case class Cost(ore: Int, clay: Int, obsidian: Int):
    def canAfford(ore: Int, clay: Int, obsidian: Int): Boolean =
      this.ore <= ore && this.clay <= clay && this.obsidian <= obsidian

  final case class Minerals(ore: Int, clay: Int, obsidian: Int, geode: Int):
    def isValid: Boolean = ore >= 0 && clay >= 0 && obsidian >= 0 && geode >= 0

    def canAfford(cost: Cost): Boolean = cost.canAfford(ore, clay, obsidian)

    infix def -(that: Cost): Minerals =
      copy(
        ore = ore - that.ore,
        clay = clay - that.clay,
        obsidian = obsidian - that.obsidian
      )

    infix def +(that: Robots): Minerals =
      Minerals(
        this.ore + that.ore,
        this.clay + that.clay,
        this.obsidian + that.obsidian,
        this.geode + that.geode
      )

    def mine(robots: Robots): Minerals = this + robots

  object Minerals:
    val empty: Minerals = Minerals(0, 0, 0, 0)

    val segmentRegex = raw"([0-9]+) (ore|clay|obsidian)".r
    def parseCost(cost: String): Minerals =
      var ore = 0
      var clay = 0
      var obsidian = 0
      segmentRegex.findAllMatchIn(cost).foreach: m =>
        val c = m.group(1).toInt
        m.group(2) match
          case "ore"      => ore = c
          case "clay"     => clay = c
          case "obsidian" => obsidian = c
      Minerals(ore, clay, obsidian, 0)

  // Because we can only build 1 robot each step,
  // Once we have N Z robots, and the sum of all costs that take Z are <= N,
  // That robot shouldn't be built anymore
  final case class Blueprint
    (
      id: Int,
      oreCost: Cost,
      clayCost: Cost,
      obsidianCost: Cost,
      geodeCost: Cost
    ):

    val maxUsefulOre =
      Seq(oreCost.ore, clayCost.ore, obsidianCost.ore, geodeCost.ore).sum
    def maxUsefulClay = obsidianCost.clay
    def maxUsefulObsidian = geodeCost.obsidian
    def maxUsefulGeode = Int.MaxValue

  object Blueprint:
    val BlueprintRE =
      raw"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian."
        .r

    def parse(str: String): Blueprint =
      str match
        case BlueprintRE(
              id,
              oreOre,
              clayOre,
              obsidianOre,
              obsidianClay,
              geodeOre,
              geodeObsidian
            ) =>
          Blueprint(
            id.toInt,
            Cost(oreOre.toInt, 0, 0),
            Cost(clayOre.toInt, 0, 0),
            Cost(obsidianOre.toInt, obsidianClay.toInt, 0),
            Cost(geodeOre.toInt, 0, geodeObsidian.toInt)
          )

  sealed trait Robot:
    val stateRobotLens: Lens[PState, Int]
    val blueprintCostLens: Getter[Blueprint, Cost]
    val blueprintMaxLens: Getter[Blueprint, Int]

  object Robot:
    object Ore extends Robot:
      val stateRobotLens: Lens[PState, Int] = GenLens[PState](_.robots.ore)
      val blueprintCostLens: Getter[Blueprint, Cost] = Getter(_.oreCost)
      val blueprintMaxLens: Getter[Blueprint, Int] = Getter(_.maxUsefulOre)
    object Clay extends Robot:
      val stateRobotLens: Lens[PState, Int] = GenLens[PState](_.robots.clay)
      val blueprintCostLens: Getter[Blueprint, Cost] = Getter(_.clayCost)
      val blueprintMaxLens: Getter[Blueprint, Int] = Getter(_.maxUsefulClay)
    object Obsidian extends Robot:
      val stateRobotLens: Lens[PState, Int] = GenLens[PState](_.robots.obsidian)
      val blueprintCostLens: Getter[Blueprint, Cost] = Getter(_.obsidianCost)
      val blueprintMaxLens: Getter[Blueprint, Int] = Getter(_.maxUsefulObsidian)
    object Geode extends Robot:
      val stateRobotLens: Lens[PState, Int] = GenLens[PState](_.robots.geode)
      val blueprintCostLens: Getter[Blueprint, Cost] = Getter(_.geodeCost)
      val blueprintMaxLens: Getter[Blueprint, Int] = Getter(_ => Int.MaxValue)

  final case class Robots(ore: Int, clay: Int, obsidian: Int, geode: Int)
  object Robots:
    val initial = Robots(1, 0, 0, 0)

  final case class PState(robots: Robots, inStock: Minerals, time: Int):
    def forceBuild(robot: Robot, blueprint: Blueprint): PState =
      val cost = robot.blueprintCostLens.get(blueprint)
      robot.stateRobotLens.modify(_ + 1)(this.copy(inStock = inStock - cost))
    def build(robot: Robot, blueprint: Blueprint): Option[PState] =
      val cost = robot.blueprintCostLens.get(blueprint)
      Option.when(inStock.canAfford(cost)):
        robot.stateRobotLens.modify(_ + 1)(this.copy(inStock = inStock - cost))
    def buildOre(blueprint: Blueprint): Option[PState] =
      build(Robot.Ore, blueprint)
    def buildClay(blueprint: Blueprint): Option[PState] =
      build(Robot.Clay, blueprint)
    def buildObsidian(blueprint: Blueprint): Option[PState] =
      build(Robot.Obsidian, blueprint)
    def buildGeode(blueprint: Blueprint): Option[PState] =
      build(Robot.Geode, blueprint)

    private def smartIdeaToBuild(cost: Cost, max: Int, value: Int): Boolean =
      value < max && inStock.canAfford(cost)

    def shouldBuild(blueprint: Blueprint, robot: Robot): Boolean =
      smartIdeaToBuild(
        robot.blueprintCostLens.get(blueprint),
        robot.blueprintMaxLens.get(blueprint),
        robot.stateRobotLens.get(this)
      )

    def shouldBuildOre(blueprint: Blueprint): Boolean =
      shouldBuild(blueprint, Robot.Ore)

    def smartBuildOre(blueprint: Blueprint): Option[PState] =
      Option.when(shouldBuildOre(blueprint)):
        this.focus(_.robots.ore).modify(_ + 1).focus(_.inStock)
          .modify(_ - blueprint.oreCost)
    def shouldBuildClay(blueprint: Blueprint): Boolean =
      shouldBuild(blueprint, Robot.Clay)
    def smartBuildClay(blueprint: Blueprint): Option[PState] =
      Option.when(shouldBuildClay(blueprint)):
        this.focus(_.robots.clay).modify(_ + 1).focus(_.inStock)
          .modify(_ - blueprint.clayCost)
    def shouldBuildObsidian(blueprint: Blueprint): Boolean =
      shouldBuild(blueprint, Robot.Obsidian)
    def smartBuildObsidian(blueprint: Blueprint): Option[PState] =
      Option.when(shouldBuildObsidian(blueprint)):
        this.focus(_.robots.obsidian).modify(_ + 1).focus(_.inStock)
          .modify(_ - blueprint.obsidianCost)
    def shouldBuildGeode(blueprint: Blueprint): Boolean =
      inStock.canAfford(blueprint.geodeCost)
    // We pass in robots here so that we can more easily handle the weird semantics of
    // 1. reserve resources
    // 2. mine
    // 3. add robots
    def mine(robots: Robots): PState = copy(inStock = inStock + robots)

    // Constrain our domain values so we can take advantage of memoization
    /*
    def mineGood(blueprint: Blueprint): PState =
      val newMinerals =
        Minerals(
          ore = (inStock.ore + robots.ore) min
            (blueprint.maxUsefulOre * (time - 1)),
          clay = (inStock.clay + robots.clay) min
            (blueprint.maxUsefulClay * (time - 1)),
          obsidian = (inStock.obsidian + robots.obsidian) min
            (blueprint.maxUsefulObsidian * (time - 1)),
          geode = inStock.geode + robots.geode
        )
      copy(inStock = newMinerals)
     */

  def fallingSumtorial(x: Int, n: Int): Int =
    // xn - triangular(n - 1)
    x * n - IntSequences.triangleNumber(n - 1).toInt
  def risingSumtorial(x: Int, n: Int): Int =
    // The factorial conversion of rising to falling applies to sumtorial too
    // This is strictly equivilant to Iterator.from(x).take(n).sum
    fallingSumtorial(x + n - 1, n)
    // Iterator.from(x).take(n).sum

  def possibleGeneration(cur: Int, timeRemaining: Int, n: Int): Int =
    cur + risingSumtorial(n, timeRemaining)

  def makesSenseToBuild
    (blueprint: Blueprint, robot: Robot)
    (cur: Int, ore: Int, clay: Int, obsidian: Int): Boolean =
    robot.blueprintMaxLens.get(blueprint) > cur &&
      robot.blueprintCostLens.get(blueprint).canAfford(ore, clay, obsidian)

  final case class Matrix(arr: IArray[Double], w: Int, h: Int)

  def maxGeodes(blueprint: Blueprint, totalTime: Int): Int =
    var maxGeodes = 0
    var maxSeen = 0
    // this is like this to (hopefully) prevent any
    // heap allocation
    def dfs
      (
        ore: Int,
        clay: Int,
        obsidian: Int,
        geode: Int,
        oreRobots: Int,
        clayRobots: Int,
        obsidianRobots: Int,
        geodeRobots: Int,
        timeLeft: Int
      ): Unit =

      maxSeen = maxSeen.max(geode)
      if timeLeft <= 0 then
        maxGeodes = maxGeodes.max(geode)
        maxSeen = maxGeodes
      else
        val upperBound =
          possibleGeneration(
            cur = geode,
            timeRemaining = timeLeft,
            n = geodeRobots
          )
        if upperBound > maxSeen then
          // If can choose geode, always do so
          if blueprint.geodeCost.canAfford(ore, clay, obsidian) then
            dfs(
              ore = ore + oreRobots - blueprint.geodeCost.ore,
              clay = clay + clayRobots - blueprint.geodeCost.clay,
              obsidian =
                obsidian + obsidianRobots - blueprint.geodeCost.obsidian,
              geode = geode + geodeRobots,
              oreRobots = oreRobots,
              clayRobots = clayRobots,
              obsidianRobots = obsidianRobots,
              geodeRobots = geodeRobots + 1,
              timeLeft = timeLeft - 1
            )
            // dfs(newState.buildGeode(blueprint).get)
          else if timeLeft == 1 then
            // if only 1 time left nad we cant make geode robot do nothing
            maxGeodes = maxGeodes.max(geode + geodeRobots)
          else
            // if we can build obsidian, then always choose that,
            // as it directly improves our geode abilities
            if timeLeft > 3 && blueprint.obsidianCost.canAfford(
                ore,
                clay,
                obsidian
              )
            then
              dfs(
                ore = ore + oreRobots - blueprint.obsidianCost.ore,
                clay = clay + clayRobots - blueprint.obsidianCost.clay,
                obsidian =
                  obsidian + obsidianRobots - blueprint.obsidianCost.obsidian,
                geode = geode + geodeRobots,
                oreRobots = oreRobots,
                clayRobots = clayRobots,
                obsidianRobots = obsidianRobots + 1,
                geodeRobots = geodeRobots,
                timeLeft = timeLeft - 1
              )
            else
              // Otherwise, do everything else we can do
              if timeLeft > 7 && makesSenseToBuild(blueprint, Robot.Ore)(
                  oreRobots,
                  ore,
                  clay,
                  obsidian
                )
              then
                dfs(
                  ore = ore + oreRobots - blueprint.oreCost.ore,
                  clay = clay + clayRobots - blueprint.oreCost.clay,
                  obsidian =
                    obsidian + obsidianRobots - blueprint.oreCost.obsidian,
                  geode = geode + geodeRobots,
                  oreRobots = oreRobots + 1,
                  clayRobots = clayRobots,
                  obsidianRobots = obsidianRobots,
                  geodeRobots = geodeRobots,
                  timeLeft = timeLeft - 1
                )
              if timeLeft > 5 && makesSenseToBuild(blueprint, Robot.Clay)(
                  clayRobots,
                  ore,
                  clay,
                  obsidian
                )
              then
                dfs(
                  ore = ore + oreRobots - blueprint.clayCost.ore,
                  clay = clay + clayRobots - blueprint.clayCost.clay,
                  obsidian =
                    obsidian + obsidianRobots - blueprint.clayCost.obsidian,
                  geode = geode + geodeRobots,
                  oreRobots = oreRobots,
                  clayRobots = clayRobots + 1,
                  obsidianRobots = obsidianRobots,
                  geodeRobots = geodeRobots,
                  timeLeft = timeLeft - 1
                )

              dfs(
                ore = ore + oreRobots,
                clay = clay + clayRobots,
                obsidian = obsidian + obsidianRobots,
                geode = geode + geodeRobots,
                oreRobots = oreRobots,
                clayRobots = clayRobots,
                obsidianRobots = obsidianRobots,
                geodeRobots = geodeRobots,
                timeLeft = timeLeft - 1
              )

    dfs(
      ore = 0,
      clay = 0,
      obsidian = 0,
      geode = 0,
      oreRobots = 1,
      clayRobots = 0,
      obsidianRobots = 0,
      geodeRobots = 0,
      timeLeft = totalTime
    )
    maxGeodes

  lazy val input = FileIO.getInput(2022, 19)

  def parse(str: String): List[Blueprint] =
    str.linesIterator.map(Blueprint.parse).toList

  def part1(input: List[Blueprint]): Int =
    input.map: b =>
      b.id * maxGeodes(b, 24)
    .sum

  def part2(input: List[Blueprint]): Int =
    input.take(3).map: b =>
      val r = maxGeodes(b, 32)
      // println(r)
      r
    .product
