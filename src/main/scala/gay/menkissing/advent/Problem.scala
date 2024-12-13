package gay.menkissing.advent



trait Problem[Input, Output] {
  def parse(str: String): Input

  def part1(input: Input): Output
  def part2(input: Input): Output

  def input: String

  def fullPart1: Output = part1(parse(input))
  def fullPart2: Output = part2(parse(input))


}
