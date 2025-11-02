package gay.menkissing.advent
package y2024

object Day19 extends Problem:
  type Input = (List[String], List[String])
  type Output = Long

  override def parse(str: String): (List[String], List[String]) =
    val Array(towelsStr, designStr) = str.split("\n\n")

    val towels = towelsStr.split(',').map(_.trim).toList
    val designs = designStr.split('\n').toList

    (towels.sortBy(_.length)(using Ordering[Int].reverse), designs)

  def parseDesign(towels: List[String], design: String): Boolean =
    def go(curTowels: List[String], restDesign: String): Boolean =
      if restDesign.isEmpty then true
      else
        curTowels match
          case head :: next =>
            if restDesign.startsWith(head) then
              val nextDesign = restDesign.drop(head.length)
              go(towels, nextDesign) || go(next, restDesign)
            else go(next, restDesign)
          case Nil => false
    go(towels, design)
  def countDesigns(towels: List[String], design: String): Long =
    def go
      (
        pattern: String,
        total: Long,
        cache: Map[String, Long]
      ): (Long, Map[String, Long]) =
      cache.get(pattern) match
        case Some(count) => (total + count, cache)
        case _           =>
          val (count, cache2) =
            towels.foldLeft(0L -> cache):
              case ((count, cache), towel) =>
                if pattern.startsWith(towel) then
                  go(pattern.drop(towel.length), count, cache)
                else (count, cache)
          (total + count, cache2 + (pattern -> count))

    go(design, 0L, Map("" -> 1L))._1

  override def part1(input: (List[String], List[String])): Long =
    val (towels, designs) = input
    designs.count(parseDesign(towels, _)).toLong

  override def part2(input: (List[String], List[String])): Long =
    val (towels, designs) = input
    designs.map(countDesigns(towels, _)).sum

  override lazy val input: String = FileIO.getInput(2024, 19)
