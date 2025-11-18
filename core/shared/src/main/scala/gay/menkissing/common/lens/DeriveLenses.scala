package gay.menkissing.common.lens

import monocle.*
import gay.menkissing.common.ThisShouldntHappenError

type ToLens[S] = [T] =>> Lens[S, T]

type LensesFor[A] = NamedTuple.Map[NamedTuple.From[A], ToLens[A]]

trait DeriveLenses[A](using lensesMap: LensesMap[A]) extends Selectable:
  type Fields = LensesFor[A]

  private val lenses = lensesMap.lenses

  inline def selectDynamic(name: String): PLens[A, A, ?, ?] =
    lenses.getOrElse(
      name,
      throw new ThisShouldntHappenError("Fields are typed at compile time")
    )
