package gay.menkissing.advent
package y2015

import gay.menkissing.common.*


import collection.mutable

object Day10 extends Problem:
  type Input = String
  type Output = Int

  def parse(str: String): String = str.trim

  // stupid science...
  // i can't be assed to do this all manually
  // ^ yet i did anyway. classic

  val elements =
    Map(
      "H" -> "22",
      "He" -> "13112221133211322112211213322112",
      "Li" -> "312211322212221121123222112",
      "Be" -> "111312211312113221133211322112211213322112",
      "B" -> "1321132122211322212221121123222112",
      "C" -> "3113112211322112211213322112",
      "N" -> "111312212221121123222112",
      "O" -> "132112211213322112",
      "F" -> "31121123222112",
      "Ne" -> "111213322112",
      "Na" -> "123222112",
      "Mg" -> "3113322112",
      "Al" -> "1113222112",
      "Si" -> "1322112",
      "P" -> "311311222112",
      "S" -> "1113122112",
      "Cl" -> "132112",
      "Ar" -> "3112",
      "K" -> "1112",
      "Ca" -> "12",
      "Sc" -> "3113112221133112",
      "Ti" -> "11131221131112",
      "V" -> "13211312",
      "Cr" -> "31132",
      "Mn" -> "111311222112",
      "Fe" -> "13122112",
      "Co" -> "32112",
      "Ni" -> "11133112",
      "Cu" -> "131112",
      "Zn" -> "312",
      "Ga" -> "13221133122211332",
      "Ge" -> "31131122211311122113222",
      "As" -> "11131221131211322113322112",
      "Se" -> "13211321222113222112",
      "Br" -> "3113112211322112",
      "Kr" -> "11131221222112",
      "Rb" -> "1321122112",
      "Sr" -> "3112112",
      "Y" -> "1112133",
      "Zr" -> "12322211331222113112211",
      "Nb" -> "1113122113322113111221131221",
      "Mo" -> "13211322211312113211",
      "Tc" -> "311322113212221",
      "Ru" -> "132211331222113112211",
      "Rh" -> "311311222113111221131221",
      "Pd" -> "111312211312113211",
      "Ag" -> "132113212221",
      "Cd" -> "3113112211",
      "In" -> "11131221",
      "Sn" -> "13211",
      "Sb" -> "3112221",
      "Te" -> "1322113312211",
      "I" -> "311311222113111221",
      "Xe" -> "11131221131211",
      "Cs" -> "13211321",
      "Ba" -> "311311",
      "La" -> "11131",
      "Ce" -> "1321133112",
      "Pr" -> "31131112",
      "Nd" -> "111312",
      "Pm" -> "132",
      "Sm" -> "311332",
      "Eu" -> "1113222",
      "Gd" -> "13221133112",
      "Tb" -> "3113112221131112",
      "Dy" -> "111312211312",
      "Ho" -> "1321132",
      "Er" -> "311311222",
      "Tm" -> "11131221133112",
      "Yb" -> "1321131112",
      "Lu" -> "311312",
      "Hf" -> "11132",
      "Ta" -> "13112221133211322112211213322113",
      "W" -> "312211322212221121123222113",
      "Re" -> "111312211312113221133211322112211213322113",
      "Os" -> "1321132122211322212221121123222113",
      "Ir" -> "3113112211322112211213322113",
      "Pt" -> "111312212221121123222113",
      "Au" -> "132112211213322113",
      "Hg" -> "31121123222113",
      "Tl" -> "111213322113",
      "Pb" -> "123222113",
      "Bi" -> "3113322113",
      "Po" -> "1113222113",
      "At" -> "1322113",
      "Rn" -> "311311222113",
      "Fr" -> "1113122113",
      "Ra" -> "132113",
      "Ac" -> "3113",
      "Th" -> "1113",
      "Pa" -> "13",
      "U" -> "3"
    )

  val sortedElements =
    elements.toList.map(_.swap).sortBy(_._1.length)(using Ordering[Int].reverse)

  val decaysTo =
    Map[String, List[String]](
      "H" -> List("H"),
      "He" -> List("Hf", "Pa", "H", "Ca", "Li"),
      "Li" -> List("He"),
      "Be" -> List("Ge", "Ca", "Li"),
      "B" -> List("Be"),
      "C" -> List("B"),
      "N" -> List("C"),
      "O" -> List("N"),
      "F" -> List("O"),
      "Ne" -> List("F"),
      "Na" -> List("Ne"),
      "Mg" -> List("Pm", "Na"),
      "Al" -> List("Mg"),
      "Si" -> List("Al"),
      "P" -> List("Ho", "Si"),
      "S" -> List("P"),
      "Cl" -> List("S"),
      "Ar" -> List("Cl"),
      "K" -> List("Ar"),
      "Ca" -> List("K"),
      "Sc" -> List("Ho", "Pa", "H", "Ca", "Co"),
      "Ti" -> List("Sc"),
      "V" -> List("Ti"),
      "Cr" -> List("V"),
      "Mn" -> List("Cr", "Si"),
      "Fe" -> List("Mn"),
      "Co" -> List("Fe"),
      "Ni" -> List("Zn", "Co"),
      "Cu" -> List("Ni"),
      "Zn" -> List("Cu"),
      "Ga" -> List("Eu", "Ca", "Ac", "H", "Ca", "Zn"),
      "Ge" -> List("Ho", "Ga"),
      "As" -> List("Ge", "Na"),
      "Se" -> List("As"),
      "Br" -> List("Se"),
      "Kr" -> List("Br"),
      "Rb" -> List("Kr"),
      "Sr" -> List("Rb"),
      "Y" -> List("Sr", "U"),
      "Zr" -> List("Y", "H", "Ca", "Tc"),
      "Nb" -> List("Er", "Zr"),
      "Mo" -> List("Nb"),
      "Tc" -> List("Mo"),
      "Ru" -> List("Eu", "Ca", "Tc"),
      "Rh" -> List("Ho", "Ru"),
      "Pd" -> List("Rh"),
      "Ag" -> List("Pd"),
      "Cd" -> List("Ag"),
      "In" -> List("Cd"),
      "Sn" -> List("In"),
      "Sb" -> List("Pm", "Sn"),
      "Te" -> List("Eu", "Ca", "Sb"),
      "I" -> List("Ho", "Te"),
      "Xe" -> List("I"),
      "Cs" -> List("Xe"),
      "Ba" -> List("Cs"),
      "La" -> List("Ba"),
      "Ce" -> List("La", "H", "Ca", "Co"),
      "Pr" -> List("Ce"),
      "Nd" -> List("Pr"),
      "Pm" -> List("Nd"),
      "Sm" -> List("Pm", "Ca", "Zn"),
      "Eu" -> List("Sm"),
      "Gd" -> List("Eu", "Ca", "Co"),
      "Tb" -> List("Ho", "Gd"),
      "Dy" -> List("Tb"),
      "Ho" -> List("Dy"),
      "Er" -> List("Ho", "Pm"),
      "Tm" -> List("Er", "Ca", "Co"),
      "Yb" -> List("Tm"),
      "Lu" -> List("Yb"),
      "Hf" -> List("Lu"),
      "Ta" -> List("Hf", "Pa", "H", "Ca", "W"),
      "W" -> List("Ta"),
      "Re" -> List("Ge", "Ca", "W"),
      "Os" -> List("Re"),
      "Ir" -> List("Os"),
      "Pt" -> List("Ir"),
      "Au" -> List("Pt"),
      "Hg" -> List("Au"),
      "Tl" -> List("Hg"),
      "Pb" -> List("Tl"),
      "Bi" -> List("Pm", "Pb"),
      "Po" -> List("Bi"),
      "At" -> List("Po"),
      "Rn" -> List("Ho", "At"),
      "Fr" -> List("Rn"),
      "Ra" -> List("Fr"),
      "Ac" -> List("Ra"),
      "Th" -> List("Ac"),
      "Pa" -> List("Th"),
      "U" -> List("Pa")
    )
  // ???
  def segmentedStr(s: String): List[String] =
    s.toList.segmented.map(_.foldString(""))

  def rle(str: String): String =
    val chunks = segmentedStr(str)

    val r = chunks.map(it => it.length.toString + it.head.toString).mkString("")
    // println(r)
    r

  def tryAsElements(s: String): Option[List[String]] =
    val es = mutable.ListBuffer[String]()
    var curS = s

    while curS.nonEmpty do
      sortedElements.find(it => curS.startsWith(it._1)) match
        case Some((v, k)) =>
          curS = curS.drop(v.length)
          es.prepend(k)
        case None => return None

    Some(es.reverse.toList)

  def lookandsay(str: String, limit: Int): Int =
    var i = 0
    var maybeElems = tryAsElements(str)

    var curS = str
    while maybeElems.isEmpty && i < limit do
      curS = rle(curS)
      maybeElems = tryAsElements(str)
      i += 1

    if i >= limit then curS.length
    else
      val elems = maybeElems.get
      (i until limit).foldLeft(elems.groupMapReduce(identity)(_ => 1)(_ + _)):
        (elems, _) =>
          val newElems = mutable.HashMap[String, Int]()
          elems.foreach: (k, v) =>
            decaysTo(k).foreach: kk =>
              newElems.updateWith(kk):
                case Some(vv) => Some(vv + v)
                case None     => Some(v)
          newElems.toMap
      .map((k, v) => elements(k).length * v).sum

  // we will never see anything > 3
  /*
    1 group ->
      1 -> becomes a group of len two
      2, 3 -> becomes two groups of len one
    2 group ->
      2 -> becomes 1 group of len 2 (mostly stable? can collpase if neighbor is 2, but from emperical tests that seems to cause it to loop back)
      1, 3 -> becomes 2 groups of len one
    3 group ->
      3 -> should _never_ happen
      1, 2 -> becomes 2 groups of len one

    221 -> 2211 -> 2221 -> 3211 -> 3221 -> (ignore 3, it acts as a buffer when its on the left)
    32211 ... so yes. its mostly stable.
    lets try to break it tho:
    221222 -> 221132 -> 22211312 -> 3221131112 -> 132221133112 ->
    (we can ignore the 3 on the left, it won't affect the inner part anymore)
    ...3221232112 -> (13)22111213122112 -> (223)112111311222112 ->
    (21123113213)22112 -> 222112 -> (3)22112 -> 222112 -> (3)22112

    The last digit seems to always remain the same

    lets actually try and prove 3 wont effect thing
    113112211 -> 2113212221
    13211311123113112211 -> 1113122113311213|2113212221

    3 can sometimes be in groups of 2 so
    33133 -> 231123 -> 1213211213
    but it doesn't really matter - the 3 is on the right so it won't affect anything unless we generate a 3
    3111 -> 1331 -> 112311
    111 -> (3)1 -> (13)11
    ok it can be incorrect but if we always discard 3s


    -- i watched the video linked in p2 and its basically cheating by telling me the answer
    I was right about 3 segregating but not for the right reason: 3 will _never_ interact with its neighbors again

   */
  def part1(input: String): Int = lookandsay(input, 40)

  def part2(input: String): Int = lookandsay(input, 50)

  lazy val input: String = FileIO.getInput(2015, 10)
