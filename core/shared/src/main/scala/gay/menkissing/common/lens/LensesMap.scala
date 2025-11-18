package gay.menkissing.common
package lens

import monocle.*
import scala.quoted.*
import scala.deriving.*

case class LensesMap[S](lenses: Map[String, PLens[S, S, ?, ?]])

object LensesMap:
  inline given derived[S](using m: Mirror.ProductOf[S]): LensesMap[S] =
    ${ derivedImpl[S]('m) }

  def derivedImpl[S: Type]
    (mirror: Expr[Mirror.ProductOf[S]])
    (using Quotes): Expr[LensesMap[S]] =

    val ls =
      Macros.exprOfMap:
        Macros.summonProductFields[S](mirror).map: elem =>
          import elem.fieldType

          elem.label -> LensMacros.makeLensImpl(elem)
            .asExprOf[PLens[S, S, ?, ?]]

    '{ LensesMap($ls) }
