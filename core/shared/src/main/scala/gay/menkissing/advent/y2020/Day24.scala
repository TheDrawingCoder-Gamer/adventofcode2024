package gay.menkissing.advent
package y2020

import parsley.*
import gay.menkissing.common.repeated

object Day24 extends Problem:
  type Input = List[List[HexDir]]
  type Output = Int

  enum HexDir:
    case East
    case SouthEast
    case SouthWest
    case West
    case NorthWest
    case NorthEast
  object HexDir:
    import parsley.character.strings
    lazy val parser =
      strings(
        "se",
        "sw",
        "nw",
        "ne",
        "w",
        "e"
      ).map:
        case "e"  => East
        case "se" => SouthEast
        case "sw" => SouthWest
        case "w"  => West
        case "nw" => NorthWest
        case "ne" => NorthEast
    lazy val parseMany = Parsley.some(parser)

  lazy val input = FileIO.getInput(2020, 24)

  def parse(str: String): Input =
    str.linesIterator.map: line =>
      HexDir.parseMany.parse(line).get
    .toList

  // cube coordinates: https://www.redblobgames.com/grids/hexagons/
  case class HexCoord(q: Int, r: Int, s: Int):
    def move(dir: HexDir): HexCoord =
      dir match
        case HexDir.East      => HexCoord(q + 1, r, s - 1)
        case HexDir.SouthEast => HexCoord(q, r + 1, s - 1)
        case HexDir.SouthWest => HexCoord(q - 1, r + 1, s)
        case HexDir.West      => HexCoord(q - 1, r, s + 1)
        case HexDir.NorthWest => HexCoord(q, r - 1, s + 1)
        case HexDir.NorthEast => HexCoord(q + 1, r - 1, s)

    def neighbors: List[HexCoord] = HexDir.values.map(this.move).toList

    def identify(from: List[HexDir]): HexCoord =
      from.foldLeft(this)((a, dir) => a.move(dir))

  def determineStart(input: List[List[HexDir]]): Set[HexCoord] =
    input.foldLeft(Set.empty[HexCoord]): (blackUp, tile) =>
      val realCoord = HexCoord(0, 0, 0).identify(tile)
      if blackUp(realCoord) then blackUp - realCoord else blackUp + realCoord
  def part1(input: List[List[HexDir]]): OutputP1 = determineStart(input).size

  // even tho it would be hilarious to reuse day 17s code entirely, we have different test condition
  // if we were given "keep alive at 2 or 3 and birth at 3" then we could reuse day 17
  // not going to stop me from doing the minimal possible changes tho LOL
  def step(state: Set[HexCoord]): Set[HexCoord] =
    val extendedSet = state.flatMap(it => it.neighbors.toSet + it)
    val withKilled =
      state.filter: p =>
        val r = p.neighbors.count(state.apply)
        r == 1 || r == 2
    val deadSet = extendedSet -- state
    val withBorn =
      deadSet.filter:
        _.neighbors.count(state.apply) == 2
    withKilled ++ withBorn

  def part2(input: List[List[HexDir]]): OutputP2 =
    val state = determineStart(input)
    step.repeated(100)(state).size
