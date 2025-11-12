package gay.menkissing.advent
package y2020

import collection.mutable

object Day21 extends ProblemAdv:
  type Input = List[(List[String], List[String])]
  type OutputP1 = Int
  type OutputP2 = String

  lazy val input = FileIO.getInput(2020, 21)

  def parse(str: String): Input =
    str.linesIterator.map:
      case s"$start (contains $rest)" =>
        (start.split(" ").toList, rest.split(", ").toList)
    .toList

  def impossibleAllergens
    (input: List[(List[String], List[String])]): Map[String, Set[String]] =
    val ingredients = input.flatMap(_._1).toSet
    val impossibleAllergens = mutable.Map.empty[String, mutable.Set[String]]

    input.foreach: (ings, allergens) =>
      val otherIngredients = (ingredients -- ings)
      otherIngredients.foreach: ing =>
        val v = impossibleAllergens.getOrElseUpdate(ing, mutable.Set())
        v.addAll(allergens)

    impossibleAllergens.map((k, v) => k -> v.toSet).toMap

  def part1(input: List[(List[String], List[String])]): OutputP1 =
    val allergens = input.flatMap(_._2).toSet

    val impossibleAllergens = this.impossibleAllergens(input)
    // println(impossibleAllergens.mkString("\n"))

    val cleanIngredients =
      impossibleAllergens.filter(_._2.size == allergens.size)
    input.flatMap(_._1).count(cleanIngredients.contains)

  def part2(input: List[(List[String], List[String])]): OutputP2 =
    val allergens = input.flatMap(_._2).toSet
    val impossibleAllergens = this.impossibleAllergens(input)
    // have at least ONE : )
    val dirtyIngredients =
      impossibleAllergens.filter(_._2.size < allergens.size)
    assert(dirtyIngredients.size == allergens.size)
    val allergenMap = mutable.Map[String, String]()
    val curDirty = dirtyIngredients.to(mutable.Map)
    while allergenMap.size < allergens.size do
      val nowKnown = curDirty.filter((_, v) => v.size == allergens.size - 1)
      nowKnown.foreach: (knownK, knownV) =>
        val res = (allergens -- knownV).head
        allergenMap(knownK) = res
        curDirty.mapValuesInPlace((_, v) => v + res)
          .filterInPlace((_, v) => v.size < allergens.size)
    // println(allergenMap.mkString("\n"))
    allergenMap.toList.sortBy(_._2).map(_._1).mkString(",")
