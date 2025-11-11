package gay.menkissing.advent
package y2020

object Day04 extends Problem:
  type Input = List[Passport]
  type Output = Int

  final case class Passport
    (
      byr: Option[Int],
      iyr: Option[Int],
      eyr: Option[Int],
      hgt: Option[String],
      hcl: Option[String],
      ecl: Option[String],
      pid: Option[String]
    ):
    def isValid: Boolean =
      (for
        _ <- byr
        _ <- iyr
        _ <- eyr
        _ <- hgt
        _ <- hcl
        _ <- ecl
        _ <- pid
      yield ()).isDefined

    def isValidP2: Boolean =
      (for
        byr <- this.byr
        if byr >= 1920 && byr <= 2002
        iyr <- this.iyr
        if iyr >= 2010 && iyr <= 2020
        eyr <- this.eyr
        if eyr >= 2020 && eyr <= 2030
        hgt <- this.hgt
        (heightNumStr, heightUnit) = hgt.span(_.isDigit)
        heightNum = heightNumStr.toInt
        if (heightUnit == "cm" && heightNum >= 150 && heightNum <= 193) ||
          (heightUnit == "in" && heightNum >= 59 && heightNum <= 76)
        hcl <- this.hcl
        if hcl.head == '#' && hcl.tail.length == 6 &&
          hcl.tail.forall(it => it.isDigit || (it >= 'a' && it <= 'f'))
        ecl <- this.ecl
        if ecl == "amb" || ecl == "blu" || ecl == "brn" || ecl == "gry" ||
          ecl == "grn" || ecl == "hzl" || ecl == "oth"
        pid <- this.pid
        if pid.length == 9
      yield ()).isDefined

  override def parse(str: String): List[Passport] =
    str.split("\n\n").map: block =>
      var byr = Option.empty[Int]
      var iyr = Option.empty[Int]
      var eyr = Option.empty[Int]
      var hgt = Option.empty[String]
      var hcl = Option.empty[String]
      var ecl = Option.empty[String]
      var pid = Option.empty[String]
      block.split(raw"\s+").foreach:
        // throw if not valid int
        case s"byr:$b" => byr = Some(b.toInt)
        case s"iyr:$i" => iyr = Some(i.toInt)
        case s"eyr:$e" => eyr = Some(e.toInt)
        case s"hgt:$h" => hgt = Some(h)
        case s"hcl:$h" => hcl = Some(h)
        case s"ecl:$e" => ecl = Some(e)
        case s"pid:$p" => pid = Some(p)
        case _         => ()
      Passport(byr, iyr, eyr, hgt, hcl, ecl, pid)
    .toList

  override def part1(input: List[Passport]): Int = input.count(_.isValid)

  override def part2(input: List[Passport]): Int = input.count(_.isValidP2)

  override lazy val input: String = FileIO.getInput(2020, 4)
