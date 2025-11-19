package gay.menkissing.advent
package y2020

import parsley.*
import gay.menkissing.common.repeated
import gay.menkissing.common.conwayStep

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

  def input = FileIO.getInput(2020, 24)

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

  // Use scala 3.8.0's standard conway step function
  val step = conwayStep[HexCoord](_.neighbors, it => it == 1 || it == 2, _ == 2)

  def part2(input: List[List[HexDir]]): OutputP2 =
    val state = determineStart(input)
    step.repeated(100)(state).size
